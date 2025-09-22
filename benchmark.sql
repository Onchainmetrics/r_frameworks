WITH benchmark_tokens AS (
    SELECT token_address FROM (
        VALUES 
        ('{{token_address}}'),
        ('DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263'),  -- BONK
        ('EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm'),  -- WIF
        ('9BB6NFEcjBCtnNLFko2FqVQBq8HHM13kCyYcdQbgpump'),  -- FART
        ('HeLp6NuQkmYB4pYWo2zYs22mESHXPQYzXbB8n4V98jwC'),  -- AI16Z
        ('61V8vBaqAGMpgDQi4JcAwo1dmBGHsyhzodcPqnEVpump'),  -- ARC
        ('Hjw6bEcHtbHGpQr8onG3izfJY5DJiWdt7uk2BfdSpump'),  -- SNAI
        ('6p6xgHyF7AeE6TZkSmFsko444wqoP15icUSqi2jfGiPN'),  -- TRUMP
        ('2qEHjDLDLbuBgRYvsxhc5D6uDWAivNFZGan56P1tpump'),  -- PNUT
        ('63LfDmNb3MQ8mw9MtZ2To9bEA2M71kZUUGq5tiJxcqj9'),  -- GIGA
        ('A8C3xuqscfmyLrte3VmTqrAq8kgMASius9AFNANwpump'),  -- FWOG
        ('eL5fUxj2J4CiQsmW85k5FG9DvuQjjUoBHoQBi2Kpump'),   -- UFD
        ('9UYAYvVS2cZ3BndbsoG1ScJbjfwyEPGxjE79hh5ipump'),  -- DOGEAI
        ('MEW1gQWJ3nEXg2qgERiKu7FAFj79PHvQVREQUzScPP5'),   -- MEW
        ('6ogzHhzdrQr9Pgv6hZ2MNze7UrzBMAFyBBWUYp1Fhitx'),  -- RETARDIO
        ('J7tYmq2JnQPvxyhcXpCDrvJnc9R5ts8rv7tgVHDPsw7U')   -- FLOYDAI
    ) AS t(token_address)
),
token_ages AS (
    SELECT 
        token_address,
        start_date,
        CASE 
            WHEN EXTRACT(day FROM NOW() - start_date) > 30 THEN 'day'
            WHEN EXTRACT(day FROM NOW() - start_date) > 7 THEN '4hour'
            ELSE 'hour'
        END as grouping_interval
    FROM dune.latecapdao.result_metadata_matview
    WHERE token_address IN (SELECT token_address FROM benchmark_tokens)
),
base_trades AS (
    SELECT 
        t.token_address,
        block_time,
        CASE 
            WHEN token_bought_mint_address = t.token_address THEN amount_usd / token_bought_amount
            ELSE amount_usd / token_sold_amount
        END as price,
        amount_usd
    FROM dex_solana.trades
    INNER JOIN benchmark_tokens t
        ON token_bought_mint_address = t.token_address 
        OR token_sold_mint_address = t.token_address
    INNER JOIN token_ages ta 
        ON t.token_address = ta.token_address 
        AND block_time >= ta.start_date
),time_groups AS (
    SELECT
        bt.token_address,
        CASE 
            WHEN ta.grouping_interval = 'day' 
                THEN date_trunc('day', block_time)
            WHEN ta.grouping_interval = '4hour' 
                THEN date_trunc('hour', block_time) + 
                     INTERVAL '4' HOUR * (CAST(extract(hour from block_time) AS INTEGER) / 4)
            ELSE date_trunc('hour', block_time)
        END as time_interval,
        ARRAY_AGG(price ORDER BY block_time)[1] as open_price,
        ARRAY_AGG(price ORDER BY block_time DESC)[1] as close_price,
        SUM(amount_usd) as volume,
        COUNT(*) as trade_count
    FROM base_trades bt
    INNER JOIN token_ages ta ON bt.token_address = ta.token_address
    GROUP BY 1, 2
)
SELECT 
    token_address,
    time_interval,
    open_price,
    close_price,
    volume,
    trade_count,
    AVG(close_price) OVER (
        PARTITION BY token_address 
        ORDER BY time_interval 
        ROWS BETWEEN 7 PRECEDING AND CURRENT ROW
    ) as sma_8,
    AVG(close_price) OVER (
        PARTITION BY token_address 
        ORDER BY time_interval 
        ROWS BETWEEN 19 PRECEDING AND CURRENT ROW
    ) as sma_20,
    ROUND(((close_price - open_price) / NULLIF(open_price, 0) * 100), 2) as price_change_pct
FROM time_groups
ORDER BY token_address, time_interval DESC;