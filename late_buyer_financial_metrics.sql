-- Late Buyer Financial Analysis Query
-- Calculates comprehensive financial metrics for top late buyer entities from uploaded dataset
-- Parameters: {{Contract Address}}, {{ticker}}

WITH token_meta AS (
    SELECT start_date, total_supply, symbol
    FROM dune.latecapdao.result_metadata_matview
    WHERE token_address = '{{Contract Address}}'
),

late_buyer_wallets AS (
    SELECT 
        address,
        wallet_type,
        CAST(current_balance AS DOUBLE) as current_balance,
        CASE 
            WHEN supply_pct IS NULL OR TRY_CAST(supply_pct AS DOUBLE) IS NULL THEN 0.0
            ELSE TRY_CAST(supply_pct AS DOUBLE)
        END as supply_pct_numeric,
        
        CASE 
            WHEN CAST(cluster_id AS VARCHAR) = 'N/A' OR cluster_id IS NULL THEN 'Individual'
            ELSE CONCAT('Late Buyer Cluster ', CAST(cluster_id AS VARCHAR))
        END as entity_name
    FROM dune.latecapdao.dataset_{{ticker}}_comprehensive
    WHERE current_balance > 0
        AND wallet_type = 'Late_Buyer'  -- Only late buyers
    ORDER BY current_balance DESC
    LIMIT 25  -- Top 25 late buyers by current balance
),

-- Get all DEX trades for these late buyer addresses (using same pattern as whales query)
wallet_trades AS (
    SELECT 
        t.trader_id as wallet_address,
        lbw.entity_name,
        lbw.supply_pct_numeric,
        lbw.current_balance,
        t.block_time,
        CASE 
            WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN 'buy'
            WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN 'sell'
            ELSE 'other'
        END as trade_type,
        CASE 
            WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN t.token_bought_amount
            WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN t.token_sold_amount
            ELSE 0
        END as token_amount,
        CASE 
            WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN t.amount_usd
            WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN t.amount_usd
            ELSE 0
        END as usd_amount
    FROM dex_solana.trades t
    INNER JOIN late_buyer_wallets lbw ON t.trader_id = lbw.address
    WHERE t.block_time >= (SELECT start_date FROM token_meta)
        AND (t.token_bought_mint_address = '{{Contract Address}}' OR t.token_sold_mint_address = '{{Contract Address}}')
        AND t.amount_usd > 1  -- Minimum trade size
),

-- Get current token price (FIXED: Latest legitimate transaction)
current_price AS (
    SELECT 
        amount_usd / CASE 
            WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
            WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
        END as current_price_usd
    FROM dex_solana.trades
    WHERE (token_bought_mint_address = '{{Contract Address}}' OR token_sold_mint_address = '{{Contract Address}}')
        AND amount_usd >= 5000  -- Minimum 5K USD transaction for legitimacy
        AND amount_usd IS NOT NULL
        AND (
            (token_bought_mint_address = '{{Contract Address}}' AND token_bought_amount > 0) OR
            (token_sold_mint_address = '{{Contract Address}}' AND token_sold_amount > 0)
        )
        AND (amount_usd / CASE 
            WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
            WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
        END) > 0  -- Valid price calculation
    ORDER BY block_time DESC
    LIMIT 1
),

-- Trading financial metrics with flow calculations (matching insider query pattern)
trading_metrics AS (
    SELECT 
        wallet_address,
        entity_name,
        supply_pct_numeric,
        current_balance,
        
        -- Investment metrics
        SUM(CASE WHEN trade_type = 'buy' THEN usd_amount ELSE 0 END) as total_invested_usd,
        SUM(CASE WHEN trade_type = 'sell' THEN usd_amount ELSE 0 END) as total_sold_usd,
        
        -- Token flow tracking
        SUM(CASE 
            WHEN trade_type = 'buy' THEN token_amount 
            WHEN trade_type = 'sell' THEN -token_amount 
            ELSE 0 
        END) as net_purchased_tokens,
        
        -- Flow calculations (positive = net buying, negative = net selling)
        COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '7' day 
            THEN (CASE 
                WHEN trade_type = 'buy' THEN usd_amount
                WHEN trade_type = 'sell' THEN -usd_amount
                ELSE 0
            END)
            ELSE 0 
        END), 0) as flow_7d_usd,
        
        COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '30' day 
            THEN (CASE 
                WHEN trade_type = 'buy' THEN usd_amount
                WHEN trade_type = 'sell' THEN -usd_amount
                ELSE 0
            END)
            ELSE 0 
        END), 0) as flow_30d_usd,
        
        COALESCE(SUM(CASE 
            WHEN block_time >= NOW() - INTERVAL '90' day 
            THEN (CASE 
                WHEN trade_type = 'buy' THEN usd_amount
                WHEN trade_type = 'sell' THEN -usd_amount
                ELSE 0
            END)
            ELSE 0 
        END), 0) as flow_90d_usd,
        
        -- Trading activity
        COUNT(CASE WHEN trade_type = 'buy' THEN 1 END) as buy_count,
        COUNT(CASE WHEN trade_type = 'sell' THEN 1 END) as sell_count,
        MIN(block_time) as first_trade_time,
        MAX(block_time) as last_trade_time
        
    FROM wallet_trades
    WHERE trade_type IN ('buy', 'sell')
    GROUP BY wallet_address, entity_name, supply_pct_numeric, current_balance
)

-- Final output with comprehensive metrics
SELECT 
    tm.wallet_address,
    SUBSTRING(tm.wallet_address, 1, 8) || '...' as wallet_short,
    tm.entity_name,
    
    -- Position metrics (FIXED LOGIC)
    ROUND(tm.supply_pct_numeric, 4) as supply_pct,
    ROUND(tm.current_balance / 1e6, 2) as total_balance_millions,
    
    -- Purchased balance: Cannot exceed current balance
    ROUND(LEAST(GREATEST(tm.net_purchased_tokens, 0), tm.current_balance) / 1e6, 2) as purchased_balance_millions,
    
    -- Transferred balance: Current minus purchased (but never negative)
    ROUND(GREATEST(tm.current_balance - GREATEST(tm.net_purchased_tokens, 0), 0) / 1e6, 2) as transferred_balance_millions,
    
    -- Transferred percentage
    ROUND(
        CASE 
            WHEN tm.current_balance > 0 
            THEN GREATEST(tm.current_balance - GREATEST(tm.net_purchased_tokens, 0), 0) / tm.current_balance * 100 
            ELSE 0 
        END, 
        1
    ) as transferred_pct,
    
    -- Portfolio values (FIXED)
    ROUND(tm.current_balance * cp.current_price_usd, 2) as total_portfolio_value_usd,
    ROUND(LEAST(GREATEST(tm.net_purchased_tokens, 0), tm.current_balance) * cp.current_price_usd, 2) as purchased_tokens_value_usd,
    ROUND(GREATEST(tm.current_balance - GREATEST(tm.net_purchased_tokens, 0), 0) * cp.current_price_usd, 2) as transferred_tokens_value_usd,
    
    -- Trading financial metrics (ONLY for purchased tokens)
    ROUND(tm.total_invested_usd, 2) as total_invested_usd,
    ROUND(tm.total_sold_usd, 2) as total_sold_usd,
    ROUND(tm.total_sold_usd - tm.total_invested_usd, 2) as realized_pnl_usd,
    
    -- Unrealized PnL (FIXED: only on purchased tokens still held)
    ROUND(
        CASE 
            WHEN GREATEST(tm.net_purchased_tokens, 0) > 0 AND tm.total_invested_usd > tm.total_sold_usd
            THEN (LEAST(GREATEST(tm.net_purchased_tokens, 0), tm.current_balance) * cp.current_price_usd) - (tm.total_invested_usd - tm.total_sold_usd)
            ELSE 0
        END, 
        2
    ) as unrealized_pnl_usd,
    
    -- Total trading PnL (FIXED)
    ROUND(
        (tm.total_sold_usd - tm.total_invested_usd) + 
        CASE 
            WHEN GREATEST(tm.net_purchased_tokens, 0) > 0 AND tm.total_invested_usd > tm.total_sold_usd
            THEN (LEAST(GREATEST(tm.net_purchased_tokens, 0), tm.current_balance) * cp.current_price_usd) - (tm.total_invested_usd - tm.total_sold_usd)
            ELSE 0
        END,
        2
    ) as total_trading_pnl_usd,
    
    -- ROI calculations (FIXED)
    ROUND(
        CASE 
            WHEN tm.total_invested_usd > 0 
            THEN ((tm.total_sold_usd - tm.total_invested_usd) + 
                  CASE 
                      WHEN GREATEST(tm.net_purchased_tokens, 0) > 0 AND tm.total_invested_usd > tm.total_sold_usd
                      THEN (LEAST(GREATEST(tm.net_purchased_tokens, 0), tm.current_balance) * cp.current_price_usd) - (tm.total_invested_usd - tm.total_sold_usd)
                      ELSE 0
                  END) / tm.total_invested_usd * 100
            ELSE 0 
        END, 
        1
    ) as trading_roi_percent,
    
    -- Average cost basis (FIXED)
    ROUND(
        CASE 
            WHEN GREATEST(tm.net_purchased_tokens, 0) > 0 AND tm.total_invested_usd > tm.total_sold_usd
            THEN (tm.total_invested_usd - tm.total_sold_usd) / LEAST(GREATEST(tm.net_purchased_tokens, 0), tm.current_balance)
            ELSE 0
        END, 
        6
    ) as avg_cost_basis_usd,
    
    -- Current price and price comparison
    ROUND(cp.current_price_usd, 6) as current_price_usd,
    
    -- Flow metrics (for heatmap visualizations)
    ROUND(tm.flow_7d_usd, 2) as flow_7d_usd,
    ROUND(tm.flow_30d_usd, 2) as flow_30d_usd,
    ROUND(tm.flow_90d_usd, 2) as flow_90d_usd,
    
    -- Trading activity
    tm.buy_count,
    tm.sell_count,
    tm.buy_count + tm.sell_count as total_trades,
    tm.first_trade_time,
    tm.last_trade_time,
    CASE 
        WHEN tm.first_trade_time IS NOT NULL 
        THEN DATE_DIFF('day', tm.first_trade_time, CURRENT_TIMESTAMP) 
        ELSE NULL 
    END as days_holding,
    
    -- Behavioral classification (FIXED)
    CASE 
        WHEN tm.buy_count = 0 AND tm.sell_count = 0 THEN 'NO_TRADES'
        WHEN tm.sell_count = 0 THEN 'HODLER'
        WHEN GREATEST(tm.net_purchased_tokens, 0) = 0 AND tm.sell_count > 0 THEN 'EXITED'
        WHEN tm.sell_count > tm.buy_count THEN 'NET_SELLER'
        WHEN tm.buy_count > tm.sell_count * 2 THEN 'ACCUMULATOR'
        ELSE 'TRADER'
    END as behavior_type,
    
    -- Additional context
    tmd.symbol as token_symbol,
    cp.current_price_usd as current_price

FROM trading_metrics tm
CROSS JOIN current_price cp
CROSS JOIN token_meta tmd
ORDER BY tm.supply_pct_numeric DESC
LIMIT 25;