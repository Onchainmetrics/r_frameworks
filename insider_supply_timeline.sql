-- Complete Insider Supply Timeline - Daily Snapshots for ALL Addresses
-- Ensures ALL insider addresses are included EVERY day from launch to today
-- Parameters: {{Contract Address}}, {{ticker}}

WITH insider_wallets AS (
    SELECT 
        address,
        wallet_type,
        CAST(current_balance AS DOUBLE) as current_balance
    FROM dune.latecapdao.dataset_{{ticker}}_insiders
    WHERE current_balance IS NOT NULL 
),

token_meta AS (
    SELECT start_date, total_supply, symbol
    FROM dune.latecapdao.result_metadata_matview
    WHERE token_address = '{{Contract Address}}'
),

-- Get ALL trading days from launch to today
all_trading_days AS (
    SELECT DISTINCT DATE(t.block_time) as trade_date
    FROM dex_solana.trades t
    CROSS JOIN token_meta tm
    WHERE (t.token_bought_mint_address = '{{Contract Address}}' OR t.token_sold_mint_address = '{{Contract Address}}')
        AND t.block_time >= tm.start_date
        AND t.amount_usd > 1
    ORDER BY DATE(t.block_time)
),

-- First, get daily token changes for each insider address
insider_daily_changes AS (
    SELECT 
        t.trader_id as address,
        DATE(t.block_time) as trade_date,
        
        -- Daily token balance change
        SUM(CASE 
            WHEN t.token_bought_mint_address = '{{Contract Address}}' THEN t.token_bought_amount
            WHEN t.token_sold_mint_address = '{{Contract Address}}' THEN -t.token_sold_amount
            ELSE 0
        END) as daily_token_change,
        
        -- Daily volume for this address
        SUM(CASE 
            WHEN t.token_bought_mint_address = '{{Contract Address}}' OR t.token_sold_mint_address = '{{Contract Address}}' 
            THEN t.amount_usd 
            ELSE 0 
        END) as daily_volume_usd
        
    FROM dex_solana.trades t
    CROSS JOIN token_meta tm
    WHERE (t.token_bought_mint_address = '{{Contract Address}}' OR t.token_sold_mint_address = '{{Contract Address}}')
        AND t.block_time >= tm.start_date
        AND t.amount_usd > 1
        AND t.trader_id IN (SELECT address FROM insider_wallets)
    GROUP BY t.trader_id, DATE(t.block_time)
),

-- Then calculate cumulative changes using window function
insider_cumulative_changes AS (
    SELECT 
        address,
        trade_date,
        daily_token_change,
        daily_volume_usd,
        
        -- Cumulative token balance change up to and including this date
        SUM(daily_token_change) OVER (
            PARTITION BY address 
            ORDER BY trade_date 
            ROWS UNBOUNDED PRECEDING
        ) as cumulative_token_change
        
    FROM insider_daily_changes
),

-- CRITICAL: Create complete matrix of ALL addresses × ALL trading days
complete_address_day_matrix AS (
    SELECT 
        atd.trade_date,
        iw.address,
        iw.current_balance as current_balance
    FROM all_trading_days atd
    CROSS JOIN insider_wallets iw  -- This ensures ALL 322 addresses appear EVERY day
),

-- Calculate balance for each address on each day
daily_address_balances AS (
    SELECT 
        cadm.trade_date,
        cadm.address,
        cadm.current_balance,
        
        -- Balance on this date = current_balance - (changes that happen AFTER this date)
        cadm.current_balance - COALESCE(
            (SELECT SUM(icc.daily_token_change)
             FROM insider_cumulative_changes icc 
             WHERE icc.address = cadm.address 
               AND icc.trade_date > cadm.trade_date),  -- Sum of changes AFTER this date
            0
        ) as balance_on_date,
        
        -- Get daily volume (0 if no trades on this day)
        COALESCE(
            (SELECT icc.daily_volume_usd
             FROM insider_cumulative_changes icc 
             WHERE icc.address = cadm.address 
               AND icc.trade_date = cadm.trade_date), 
            0
        ) as daily_volume
        
    FROM complete_address_day_matrix cadm
),

-- Aggregate daily totals across ALL addresses
daily_insider_supply AS (
    SELECT 
        dab.trade_date,
        tm.total_supply,
        
        -- Debugging: Count addresses included
        COUNT(DISTINCT dab.address) as addresses_included,  -- Should ALWAYS be 322
        
        -- Total insider balance on this date (sum across all addresses)
        SUM(GREATEST(dab.balance_on_date, 0)) as total_insider_balance,
        
        -- Supply percentage
        ROUND((SUM(GREATEST(dab.balance_on_date, 0)) / tm.total_supply * 100), 4) as insider_supply_pct,
        
        -- Count metrics
        COUNT(CASE WHEN dab.balance_on_date > 0 THEN 1 END) as active_insiders,
        COUNT(CASE WHEN dab.daily_volume > 0 THEN 1 END) as active_traders,
        
        -- Trading volume
        SUM(dab.daily_volume) as total_daily_volume
        
    FROM daily_address_balances dab
    CROSS JOIN token_meta tm
    GROUP BY dab.trade_date, tm.total_supply
)

-- Final output with complete daily snapshots
SELECT 
    trade_date,
    addresses_included,  -- DEBUGGING: Should always be 322
    insider_supply_pct,
    ROUND(total_insider_balance / 1e6, 2) as insider_balance_millions,
    active_insiders,
    active_traders,
    ROUND(total_daily_volume, 0) as total_daily_volume,
    
    -- Moving averages
    ROUND(AVG(insider_supply_pct) OVER (
        ORDER BY trade_date 
        ROWS BETWEEN 6 PRECEDING AND CURRENT ROW
    ), 4) as supply_pct_7d_avg,
    
    -- Daily change
    ROUND(insider_supply_pct - LAG(insider_supply_pct, 1) OVER (ORDER BY trade_date), 4) as daily_change,
    
    -- Trend indicator
    CASE 
        WHEN insider_supply_pct = MAX(insider_supply_pct) OVER () THEN 'PEAK'
        WHEN insider_supply_pct = MIN(insider_supply_pct) OVER () THEN 'BOTTOM'
        WHEN insider_supply_pct > LAG(insider_supply_pct, 1) OVER (ORDER BY trade_date) THEN 'RISING'
        WHEN insider_supply_pct < LAG(insider_supply_pct, 1) OVER (ORDER BY trade_date) THEN 'FALLING'
        ELSE 'STABLE'
    END as trend_indicator

FROM daily_insider_supply
ORDER BY trade_date ASC

-- VALIDATION CHECKLIST:
-- 1. addresses_included should ALWAYS equal your total insider count (322)
-- 2. If addresses_included varies, the CROSS JOIN matrix isn't working
-- 3. Most recent insider_supply_pct should match your activity data (0.93%)
-- 4. Each row represents a complete snapshot of ALL insider balances for that day