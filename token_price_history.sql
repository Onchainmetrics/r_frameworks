-- Token Market Cap History Query
-- Daily market cap data for overlaying with insider supply evolution
-- Parameters: {{Contract Address}}

WITH token_meta AS (
    SELECT total_supply
    FROM dune.latecapdao.result_metadata_matview
    WHERE token_address = '{{Contract Address}}'
),

daily_prices AS (
    SELECT 
        DATE(block_time) as trade_date,
        
        -- Volume weighted average price for the day
        SUM(amount_usd) / SUM(
            CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
                ELSE 0
            END
        ) as price_usd,
        
        -- Daily trading metrics
        SUM(amount_usd) as daily_volume_usd,
        COUNT(*) as daily_trades,
        
        -- Price range for the day
        MIN(amount_usd / 
            CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
                ELSE 1
            END
        ) as price_low,
        
        MAX(amount_usd / 
            CASE 
                WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
                WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
                ELSE 1
            END
        ) as price_high
        
    FROM dex_solana.trades t
    CROSS JOIN token_meta tm
    WHERE (t.token_bought_mint_address = '{{Contract Address}}' OR t.token_sold_mint_address = '{{Contract Address}}')
        AND t.amount_usd > 1
        AND (
            (t.token_bought_mint_address = '{{Contract Address}}' AND t.token_bought_amount > 0) OR
            (t.token_sold_mint_address = '{{Contract Address}}' AND t.token_sold_amount > 0)
        )
    GROUP BY DATE(t.block_time)
    HAVING SUM(
        CASE 
            WHEN token_bought_mint_address = '{{Contract Address}}' THEN token_bought_amount
            WHEN token_sold_mint_address = '{{Contract Address}}' THEN token_sold_amount
            ELSE 0
        END
    ) > 0  -- Ensure we have valid token amounts for price calculation
)

SELECT 
    dp.trade_date,
    ROUND(dp.price_usd, 8) as price_usd,
    ROUND(dp.price_low, 8) as price_low, 
    ROUND(dp.price_high, 8) as price_high,
    
    -- MARKET CAP CALCULATIONS (in millions)
    ROUND((dp.price_usd * tm.total_supply) / 1e6, 2) as market_cap_millions,
    ROUND((dp.price_low * tm.total_supply) / 1e6, 2) as market_cap_low_millions,
    ROUND((dp.price_high * tm.total_supply) / 1e6, 2) as market_cap_high_millions,
    
    ROUND(dp.daily_volume_usd, 0) as daily_volume_usd,
    dp.daily_trades,
    
    -- Market cap change calculations
    ROUND(
        ((dp.price_usd * tm.total_supply) - LAG(dp.price_usd * tm.total_supply, 1) OVER (ORDER BY dp.trade_date)) / 
        LAG(dp.price_usd * tm.total_supply, 1) OVER (ORDER BY dp.trade_date) * 100, 
        2
    ) as market_cap_change_pct,
    
    -- Price change for reference
    ROUND(
        (dp.price_usd - LAG(dp.price_usd, 1) OVER (ORDER BY dp.trade_date)) / 
        LAG(dp.price_usd, 1) OVER (ORDER BY dp.trade_date) * 100, 
        2
    ) as price_change_pct,
    
    -- Market cap moving averages (in millions)
    ROUND(AVG(dp.price_usd * tm.total_supply) OVER (
        ORDER BY dp.trade_date 
        ROWS BETWEEN 6 PRECEDING AND CURRENT ROW
    ) / 1e6, 2) as market_cap_7d_avg_millions,
    
    ROUND(AVG(dp.price_usd * tm.total_supply) OVER (
        ORDER BY dp.trade_date 
        ROWS BETWEEN 29 PRECEDING AND CURRENT ROW
    ) / 1e6, 2) as market_cap_30d_avg_millions

FROM daily_prices dp
CROSS JOIN token_meta tm
ORDER BY trade_date ASC

-- USAGE NOTES:
-- 1. Export this as token_price_history.csv 
-- 2. Run alongside insider_supply_timeline.sql
-- 3. Join both datasets in R for combined supply vs market cap analysis
-- 4. Market cap in millions is much more intuitive than raw token price
-- 5. Shows the complete market valuation story alongside insider distribution