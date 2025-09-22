WITH insider_wallets AS (
    SELECT 
        address,
        wallet_type,
        CAST(current_balance AS DOUBLE) as current_balance,
        CASE 
            WHEN supply_pct IS NULL OR TRY_CAST(supply_pct AS DOUBLE) IS NULL THEN 0.0
            ELSE TRY_CAST(supply_pct AS DOUBLE)
        END as supply_pct_numeric,
        classification,
        CASE 
            WHEN CAST(cluster_id AS VARCHAR) = 'N/A' OR cluster_id IS NULL THEN 'Individual'
            ELSE CONCAT('Cluster ', CAST(cluster_id AS VARCHAR))
        END as entity_name
    FROM dune.latecapdao.dataset_{{ticker}}_insiders
    WHERE current_balance IS NOT NULL 
        AND current_balance != ''
        AND CAST(current_balance AS DOUBLE) > 0
),

token_meta AS (
    SELECT start_date, total_supply, symbol
    FROM dune.latecapdao.result_metadata_matview
    WHERE token_address = '{{Contract Address}}'
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
    ORDER BY 1 DESC, n_samples DESC
    LIMIT 1
),

insider_flows AS (
    SELECT 
        t.trader_id as address,
        -- Total sold USD (lifetime)
        COALESCE(SUM(CASE 
            WHEN t.token_sold_mint_address = '{{Contract Address}}' 
            THEN t.amount_usd 
            ELSE 0 
        END), 0) as total_sold_usd,
        
        -- 7-day net flows (positive = net buying, negative = net selling)
        COALESCE(SUM(CASE 
            WHEN t.block_time >= NOW() - INTERVAL '7' day 
            THEN (CASE 
                WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN t.amount_usd
                WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN -t.amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_flow_7d_usd,
        
        -- 30-day net flows
        COALESCE(SUM(CASE 
            WHEN t.block_time >= NOW() - INTERVAL '30' day 
            THEN (CASE 
                WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN t.amount_usd
                WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN -t.amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_flow_30d_usd,
        
        -- 90-day net flows
        COALESCE(SUM(CASE 
            WHEN t.block_time >= NOW() - INTERVAL '90' day 
            THEN (CASE 
                WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN t.amount_usd
                WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN -t.amount_usd
                ELSE 0
            END)
            ELSE 0 
        END), 0) as net_flow_90d_usd,
        
        -- 7-day sell amounts
        COALESCE(SUM(CASE 
            WHEN t.block_time >= NOW() - INTERVAL '7' day 
                AND t.token_sold_mint_address = '{{Contract Address}}' 
            THEN t.amount_usd 
            ELSE 0 
        END), 0) as sold_7d_usd,
        
        -- 30-day sell amounts
        COALESCE(SUM(CASE 
            WHEN t.block_time >= NOW() - INTERVAL '30' day 
                AND t.token_sold_mint_address = '{{Contract Address}}' 
            THEN t.amount_usd 
            ELSE 0 
        END), 0) as sold_30d_usd,
        
        -- 90-day sell amounts
        COALESCE(SUM(CASE 
            WHEN t.block_time >= NOW() - INTERVAL '90' day 
                AND t.token_sold_mint_address = '{{Contract Address}}' 
            THEN t.amount_usd 
            ELSE 0 
        END), 0) as sold_90d_usd,
        
        -- Total transaction count for context
        COUNT(*) as total_transactions
        
    FROM dex_solana.trades t
    INNER JOIN insider_wallets iw ON t.trader_id = iw.address
    WHERE (t.token_bought_mint_address = '{{Contract Address}}' 
           OR t.token_sold_mint_address = '{{Contract Address}}')
        AND t.block_time >= (SELECT start_date FROM token_meta)
    GROUP BY t.trader_id
)

SELECT 
    iw.address,
    iw.wallet_type,
    iw.entity_name,
    iw.classification,
    iw.current_balance,
   ROUND(iw.supply_pct_numeric, 6) as supply_pct_numeric,
    iw.current_balance * COALESCE(p.USD, 0) as current_holding_usd,
    
    -- Flow metrics
    COALESCE(if.net_flow_7d_usd, 0) as net_flow_7d_usd,
    COALESCE(if.net_flow_30d_usd, 0) as net_flow_30d_usd, 
    COALESCE(if.net_flow_90d_usd, 0) as net_flow_90d_usd,
    
    -- Sell metrics
    COALESCE(if.total_sold_usd, 0) as total_sold_usd,
    COALESCE(if.sold_7d_usd, 0) as sold_7d_usd,
    COALESCE(if.sold_30d_usd, 0) as sold_30d_usd,
    COALESCE(if.sold_90d_usd, 0) as sold_90d_usd,
    
    -- Behavior classification
    CASE 
        WHEN COALESCE(if.net_flow_7d_usd, 0) = 0 
             AND COALESCE(if.net_flow_30d_usd, 0) = 0 
             AND COALESCE(if.net_flow_90d_usd, 0) = 0 THEN 'HOLDING'
             
        WHEN COALESCE(if.net_flow_7d_usd, 0) > 1000 
             AND COALESCE(if.net_flow_30d_usd, 0) > 0 THEN 'STRONG_ACCUMULATING'
             
        WHEN COALESCE(if.net_flow_7d_usd, 0) > 0 
             AND COALESCE(if.net_flow_30d_usd, 0) > 0 THEN 'ACCUMULATING'
             
        WHEN COALESCE(if.net_flow_7d_usd, 0) < -1000 
             AND COALESCE(if.net_flow_30d_usd, 0) < 0 THEN 'STRONG_DISTRIBUTING'
             
        WHEN COALESCE(if.net_flow_7d_usd, 0) < 0 
             AND COALESCE(if.net_flow_30d_usd, 0) < 0 THEN 'DISTRIBUTING'
             
        WHEN (COALESCE(if.net_flow_7d_usd, 0) > 0 AND COALESCE(if.net_flow_30d_usd, 0) < 0)
             OR (COALESCE(if.net_flow_7d_usd, 0) < 0 AND COALESCE(if.net_flow_30d_usd, 0) > 0) THEN 'MIXED'
             
        ELSE 'NEUTRAL'
    END as behavior_pattern,
    
    -- Additional context
    COALESCE(if.total_transactions, 0) as total_transactions,
    COALESCE(p.USD, 0) as current_price,
    tm.symbol as token_symbol
    
FROM insider_wallets iw
LEFT JOIN insider_flows if ON iw.address = if.address
LEFT JOIN price p ON 1=1
LEFT JOIN token_meta tm ON 1=1
ORDER BY iw.supply_pct_numeric DESC, COALESCE(if.net_flow_7d_usd, 0) DESC