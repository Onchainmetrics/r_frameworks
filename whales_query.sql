WITH token_meta AS (
    SELECT start_date, total_supply, symbol
    FROM dune.latecapdao.result_metadata_matview
    WHERE token_address = '{{Contract Address}}'
),

MEV_suspects AS (
    SELECT address 
    FROM dune.latecapdao.result_mev_suspects_matview
),
price AS (
    SELECT
        date_trunc('hour',block_time) as time,
        COUNT(*) as n_samples,
        APPROX_PERCENTILE(value,.5) as USD
    FROM (
        SELECT amount_usd/token_bought_amount as value, block_time
        FROM dex_solana.trades
        WHERE block_time >= (SELECT start_date FROM token_meta)
            AND amount_usd > 1
            AND amount_usd IS NOT NULL 
            AND token_bought_amount > 0 
            AND token_bought_mint_address = '{{Contract Address}}'
        UNION ALL
        SELECT amount_usd/token_sold_amount as value, block_time
        FROM dex_solana.trades
        WHERE block_time >= (SELECT start_date FROM token_meta)
            AND amount_usd IS NOT NULL 
            AND token_sold_amount > 0 
            AND token_sold_mint_address = '{{Contract Address}}'
    ) f
    WHERE value < 1000
    GROUP BY 1
    ORDER BY 1 DESC, n_samples DESC  -- Added n_samples as secondary sort
    LIMIT 1
),
historical_whale_activity AS (
    SELECT 
        trader_id as address,
        SUM(CASE 
            WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
            WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
        END) as total_token_volume,
        SUM(amount_usd) as total_usd_volume,
        MAX(amount_usd) as largest_trade,
        COUNT(*) as trade_count
    FROM dex_solana.trades
    WHERE block_time >= (SELECT start_date FROM token_meta)
        AND (token_bought_mint_address = '{{Contract Address}}'
             OR token_sold_mint_address = '{{Contract Address}}')
        AND amount_usd > 100  -- Minimum trade size
    GROUP BY trader_id
),

relevant_wallets AS (
    SELECT DISTINCT
        COALESCE(lb.token_balance_owner, h.address) as address,
        COALESCE(lb.token_balance, 0) as token_balance,
        FORMAT(CAST(ROUND(COALESCE(lb.token_balance, 0) / (SELECT total_supply FROM token_meta) * 100, 2) as varchar), '0.##%') as supply_owned,
        h.total_token_volume,
        h.total_usd_volume,
        h.largest_trade,  -- Add this to support ordering
        h.trade_count     -- Add this to support filtering
    FROM historical_whale_activity h
    LEFT JOIN solana_utils.latest_balances lb 
        ON h.address = lb.token_balance_owner 
        AND lb.token_mint_address = '{{Contract Address}}'
    WHERE (
        h.total_usd_volume >= 50000 OR  -- Had significant trading volume
        h.largest_trade >= 5000 OR      -- Made large individual trades
        lb.token_balance >= (SELECT total_supply * 0.001 FROM token_meta) OR  -- Hold significant supply
        h.trade_count >= 10              -- Active traders
    )
    AND NOT EXISTS (
        SELECT 1 FROM MEV_suspects m 
        WHERE m.address = COALESCE(lb.token_balance_owner, h.address)
    )
    ORDER BY h.total_usd_volume DESC NULLS LAST
    LIMIT 200
),
smart_money AS (
    SELECT wallet as trader_id
    FROM dune.latecapdao.result_alfa_wallets_labeled
),

behavior_metrics AS (
    WITH price_lags AS (
        SELECT 
            trader_id,
            block_time,
            amount_usd/NULLIF(token_bought_amount, 0) as price,
            LAG(amount_usd/NULLIF(token_bought_amount, 0)) 
                OVER (PARTITION BY trader_id ORDER BY block_time) as prev_price
        FROM dex_solana.trades t
        WHERE EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    ),
    price_sensitivity_calc AS (
        SELECT 
            trader_id,
            CORR(price, prev_price) as price_sensitivity
        FROM price_lags
        GROUP BY trader_id
    ),
    profit_calcs AS (
        SELECT 
            trader_id,
            block_time,
            amount_usd/NULLIF(token_bought_amount, 0) as buy_price,
            LEAD(amount_usd/NULLIF(token_bought_amount, 0), 24) 
                OVER (ORDER BY block_time) as future_price
        FROM dex_solana.trades t
        WHERE token_bought_amount > 0
        AND EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    ),
    avg_profits AS (
        SELECT 
            trader_id,
            AVG(future_price - buy_price) as avg_24h_profit_on_buys
        FROM profit_calcs
        WHERE future_price IS NOT NULL
        GROUP BY trader_id
    )
    SELECT 
        t.trader_id,
        ps.price_sensitivity,
        COUNT(CASE WHEN 
            (t.token_bought_amount > 0 AND price_change < 0) OR 
            (t.token_sold_amount > 0 AND price_change > 0) 
        THEN 1 END) * 100.0 / COUNT(*) as contrarian_score,
        ap.avg_24h_profit_on_buys
    FROM dex_solana.trades t
    LEFT JOIN price_sensitivity_calc ps ON t.trader_id = ps.trader_id
    LEFT JOIN avg_profits ap ON t.trader_id = ap.trader_id
    JOIN (
        SELECT 
            block_time,
            (amount_usd/token_bought_amount - 
             LAG(amount_usd/token_bought_amount) OVER (ORDER BY block_time)) / 
             LAG(amount_usd/token_bought_amount) OVER (ORDER BY block_time) as price_change
        FROM dex_solana.trades
        WHERE token_bought_mint_address = '{{Contract Address}}'
    ) price_changes ON t.block_time = price_changes.block_time
    WHERE EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    GROUP BY t.trader_id, ps.price_sensitivity, ap.avg_24h_profit_on_buys
),

risk_metrics AS (
    WITH trade_times AS (
        SELECT 
            trader_id,
            block_time,
            token_bought_amount,
            token_sold_amount,
            LAG(block_time) OVER (PARTITION BY trader_id ORDER BY block_time) as prev_time
        FROM dex_solana.trades t
        WHERE EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    )
    SELECT 
        trader_id,
        MAX(token_bought_amount) / NULLIF(AVG(token_bought_amount), 0) as max_position_ratio,
        COUNT(DISTINCT token_bought_mint_address) as token_diversity,
        COUNT(*) as total_trades,
        COUNT(CASE WHEN token_sold_amount > 0 THEN 1 END) / 
            NULLIF(COUNT(CASE WHEN token_bought_amount > 0 THEN 1 END), 0) as sell_buy_ratio
    FROM dex_solana.trades t
    WHERE EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    GROUP BY trader_id
),
position_changes AS (
    SELECT 
        trader_id,
        COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '1' day 
            THEN (CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN amount_usd
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN -amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_position_24h_usd,
        
        COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '7' day 
            THEN (CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN amount_usd
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN -amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_position_7d_usd,
        
        COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '30' day 
            THEN (CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN amount_usd
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN -amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_position_30d_usd,
                COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '90' day 
            THEN (CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN amount_usd
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN -amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_position_90d_usd
    FROM dex_solana.trades t
    WHERE EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    GROUP BY trader_id
),

pnl_metrics AS (
    SELECT 
        trader_id,
        COALESCE(SUM(CASE 
            WHEN token_bought_mint_address = '{{Contract Address}}' 
            THEN amount_usd 
            ELSE 0 
        END), 0) as total_bought_usd,
        
        COALESCE(SUM(CASE 
            WHEN token_sold_mint_address = '{{Contract Address}}' 
            THEN amount_usd 
            ELSE 0 
        END), 0) as total_sold_usd
    FROM dex_solana.trades t
    WHERE EXISTS (SELECT 1 FROM relevant_wallets rw WHERE rw.address = t.trader_id)
    GROUP BY trader_id
),
wallet_values AS (
    SELECT 
        rw.address,
        rw.supply_owned,
        rw.token_balance,
        rw.token_balance * p.USD as usd_value,
        rw.total_token_volume,
        rw.total_usd_volume,
        COALESCE(pc.net_position_24h_usd, 0) as net_position_24h_usd,
        COALESCE(pc.net_position_7d_usd, 0) as net_position_7d_usd,
        COALESCE(pc.net_position_30d_usd, 0) as net_position_30d_usd,
        COALESCE(pc.net_position_90d_usd, 0) as net_position_90d_usd,
        COALESCE(rm.total_trades, 0) as total_trades,
        COALESCE(pnl.total_bought_usd, 0) as total_bought_usd,
        COALESCE(pnl.total_sold_usd, 0) as total_sold_usd,
        COALESCE(pnl.total_sold_usd, 0) + (rw.token_balance * p.USD) - COALESCE(pnl.total_bought_usd, 0) as unrealized_pnl
    FROM relevant_wallets rw
    CROSS JOIN price p
    LEFT JOIN position_changes pc ON rw.address = pc.trader_id
    LEFT JOIN risk_metrics rm ON rw.address = rm.trader_id
    LEFT JOIN pnl_metrics pnl ON rw.address = pnl.trader_id
)
SELECT 
    address,
    supply_owned,
    token_balance,
    usd_value,
    total_token_volume,
    total_usd_volume,
    total_bought_usd,
    total_sold_usd,
    unrealized_pnl,
    net_position_24h_usd,
    net_position_7d_usd,
    net_position_30d_usd,
    net_position_90d_usd,
    total_trades,
CASE 
    WHEN EXISTS (SELECT 1 FROM smart_money sm WHERE sm.trader_id = address) THEN
        CASE
            WHEN net_position_7d_usd > 0 AND net_position_30d_usd > 0 AND net_position_90d_usd > 0 THEN 'ALPHA_ACCUMULATING'
            WHEN net_position_7d_usd < 0 AND net_position_30d_usd < 0 AND net_position_90d_usd < 0 THEN 'ALPHA_DISTRIBUTING'
            ELSE 'ALPHA_NEUTRAL'
        END
WHEN token_balance = 0 THEN 
    CASE
        WHEN ABS(net_position_7d_usd) > 1000 OR ABS(net_position_24h_usd) > 500 THEN 'MIXED'
        WHEN net_position_90d_usd < -10000 THEN 'STRONG_DISTRIBUTING'
        WHEN net_position_90d_usd < 0 THEN 'DISTRIBUTING'
        ELSE 'EXITED'  -- Only if truly no significant activity
    END
   -- Strong accumulating: Must have tokens AND show accumulation pattern in at least 2 timeframes
WHEN token_balance > 0 AND (
    (net_position_7d_usd > usd_value * 0.1 AND net_position_30d_usd > usd_value * 0.2) OR
    (net_position_30d_usd > usd_value * 0.2 AND net_position_90d_usd > usd_value * 0.3) OR
    (net_position_7d_usd > usd_value * 0.1 AND net_position_90d_usd > usd_value * 0.3)
) THEN 'STRONG_ACCUMULATING'

-- Strong distributing: Must have tokens AND show distribution pattern in at least 2 timeframes
WHEN token_balance > 0 AND (
    (net_position_7d_usd < -usd_value * 0.1 AND net_position_30d_usd < -usd_value * 0.2) OR
    (net_position_30d_usd < -usd_value * 0.2 AND net_position_90d_usd < -usd_value * 0.3) OR
    (net_position_7d_usd < -usd_value * 0.1 AND net_position_90d_usd < -usd_value * 0.3)
) THEN 'STRONG_DISTRIBUTING'
    WHEN token_balance > 0 AND net_position_7d_usd >= 0 
        AND (net_position_30d_usd > 0 OR net_position_90d_usd > 0) THEN 'ACCUMULATING'
    WHEN token_balance > 0 AND net_position_7d_usd <= 0 
        AND (net_position_30d_usd < 0 OR net_position_90d_usd < 0) THEN 'DISTRIBUTING'
    WHEN token_balance > 0 AND ABS(net_position_90d_usd) < usd_value * 0.05 
        AND ABS(net_position_30d_usd) < usd_value * 0.05 
        AND ABS(net_position_7d_usd) < usd_value * 0.05 THEN 'HOLDING'
    WHEN token_balance > 0 AND (
        (net_position_7d_usd > 0 AND net_position_30d_usd < 0) OR
        (net_position_7d_usd < 0 AND net_position_30d_usd > 0) OR
        (net_position_30d_usd > 0 AND net_position_90d_usd < 0) OR
        (net_position_30d_usd < 0 AND net_position_90d_usd > 0)
    ) THEN 'MIXED'
    ELSE 'EXITED'
END as behavior_pattern,
    p.USD as current_price,
    tm.symbol as token_symbol,
    tm.total_supply
FROM wallet_values
CROSS JOIN price p
CROSS JOIN token_meta tm
WHERE total_usd_volume > 50000  -- Minimum activity threshold
ORDER BY total_usd_volume DESC