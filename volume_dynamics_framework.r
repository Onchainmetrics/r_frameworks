---
title: "new full framework"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

# Token Analysis Framework
# Complete implementation with parameterized data loading
# =============================================================================

# ---------------------------- 1. SETUP ---------------------------------------
# Load all required packages
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(gridExtra)
library(zoo)
library(RcppRoll)

# Main configuration function - SET THIS ONCE
initialize_analysis <- function(token_address, token_name, data_dir = NULL) {
  # Store global parameters
  if(is.null(data_dir)) {
    data_dir <- getwd()
  }
  
  # Output configuration summary
  cat(sprintf("Initializing analysis for %s (%s)\n", token_name, token_address))
  cat(sprintf("Data directory: %s\n", data_dir))
  
  # Create file paths based on token name
  whale_file <- file.path(data_dir, paste0(token_name, "_whales.csv"))
  benchmark_file <- file.path(data_dir, paste0("benchmark_", token_name, ".csv"))
  
  # Verify files exist
  if(!file.exists(whale_file)) {
    stop(sprintf("Whale data file not found: %s", whale_file))
  }
  if(!file.exists(benchmark_file)) {
    stop(sprintf("Benchmark data file not found: %s", benchmark_file))
  }
  
  # Load data files
  whale_data <- read.csv(whale_file)
  benchmark_prices <- read.csv(benchmark_file)
  
  # Ensure time_interval is correctly formatted in benchmark data
  benchmark_prices <- benchmark_prices %>%
    mutate(time_interval = as.POSIXct(time_interval))
  
  # Verify token exists in benchmark data
  if(!token_address %in% benchmark_prices$token_address) {
    stop(sprintf("Token address %s not found in benchmark data", token_address))
  }
  
  # Return configuration and data
  return(list(
    token_address = token_address,
    token_name = token_name,
    data_dir = data_dir,
    whale_data = whale_data,
    benchmark_prices = benchmark_prices
  ))
}

# Token names mapping - predefined for common tokens
TOKEN_NAMES <- c(
  "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263" = "BONK",
  "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm" = "WIF",
  "9BB6NFEcjBCtnNLFko2FqVQBq8HHM13kCyYcdQbgpump" = "FARTCOIN",
  "HeLp6NuQkmYB4pYWo2zYs22mESHXPQYzXbB8n4V98jwC" = "AI16Z",
  "61V8vBaqAGMpgDQi4JcAwo1dmBGHsyhzodcPqnEVpump" = "ARC",
  "Hjw6bEcHtbHGpQr8onG3izfJY5DJiWdt7uk2BfdSpump" = "SNAI",
  "6p6xgHyF7AeE6TZkSmFsko444wqoP15icUSqi2jfGiPN" = "TRUMP",
  "2qEHjDLDLbuBgRYvsxhc5D6uDWAivNFZGan56P1tpump" = "PNUT",
  "63LfDmNb3MQ8mw9MtZ2To9bEA2M71kZUUGq5tiJxcqj9" = "GIGA",
  "A8C3xuqscfmyLrte3VmTqrAq8kgMASius9AFNANwpump" = "FWOG",
  "eL5fUxj2J4CiQsmW85k5FG9DvuQjjUoBHoQBi2Kpump" = "UFD",
  "9UYAYvVS2cZ3BndbsoG1ScJbjfwyEPGxjE79hh5ipump" = "DOGEAI",
  "MEW1gQWJ3nEXg2qgERiKu7FAFj79PHvQVREQUzScPP5" = "MEW",
  "6ogzHhzdrQr9Pgv6hZ2MNze7UrzBMAFyBBWUYp1Fhitx" = "RETARDIO",
  "J7tYmq2JnQPvxyhcXpCDrvJnc9R5ts8rv7tgVHDPsw7U" = "FLOYDAI"
)

# ---------------------------- 2. WHALE ANALYSIS -----------------------------
# Helper function for detailed insights with new thresholds
get_key_insight <- function(trend_7d, trend_30d, strong_acc_share, strong_dist_share, strong_acc_wallets, strong_dist_wallets) {
  # Distribution dominance check (lowered from 3x to 2x, share from 60% to 40%)
  dist_dominance <- strong_dist_share > 40 && strong_dist_wallets > strong_acc_wallets * 2
  
  # Accumulation dominance check (adjusted to match distribution)
  acc_dominance <- strong_acc_share > 40 && strong_acc_wallets > strong_dist_wallets * 0.5
  
  # Recent trend divergence (lowered from 30% to 15%)
  trend_divergence <- abs(trend_7d - trend_30d) > 15
  
  # Pressure thresholds (based on quartile analysis)
  heavy_pressure <- 40  # ~75th percentile
  moderate_pressure <- 20  # ~50th percentile
  light_pressure <- 10  # ~25th percentile
  
  if(dist_dominance && trend_7d > moderate_pressure) {
    "💡 Strong buying against distribution - potential reversal forming"
  } else if(dist_dominance) {
    "💡 Heavy distribution phase - majority of whales reducing positions"
  } else if(acc_dominance && trend_7d < -light_pressure) {
    "💡 Recent selling into whale accumulation - possible dip"
  } else if(acc_dominance) {
    "💡 Sustained whale accumulation phase"
  } else if(trend_divergence && trend_7d > trend_30d) {
    "💡 Recent shift to buying pressure - watch for continuation"
  } else if(trend_divergence && trend_7d < trend_30d) {
    "💡 Recent shift to selling pressure - watch for continuation"
  } else {
    "💡 Mixed signals - no clear directional bias"
  }
}

analyze_whales <- function(whale_data, token_name) {
  # Filter out MIXED behavior
  whale_data <- whale_data %>%
    filter(behavior_pattern != "MIXED")
  
  # 1. Prepare the data with new metrics
  whale_summary <- whale_data %>%
    # Force EXITED flows to be negative
    mutate(
      flow_90d = case_when(
        behavior_pattern == "EXITED" ~ -abs(net_position_90d_usd),
        TRUE ~ net_position_90d_usd
      ),
      flow_30d = case_when(
        behavior_pattern == "EXITED" ~ -abs(net_position_30d_usd),
        TRUE ~ net_position_30d_usd
      ),
      flow_7d = case_when(
        behavior_pattern == "EXITED" ~ -abs(net_position_7d_usd),
        TRUE ~ net_position_7d_usd
      )
    ) %>%
    group_by(behavior_pattern) %>%
    summarise(
      flow_90d = sum(flow_90d),
      flow_30d = sum(flow_30d),
      flow_7d = sum(flow_7d),
      wallet_count = n(),
      total_value = sum(usd_value),
      total_flow = sum(abs(net_position_90d_usd))
    ) %>%
    mutate(
      activity_share = total_flow / sum(total_flow) * 100,
      flow_impact = flow_90d / sum(total_flow) * 100,
      avg_flow_per_wallet = flow_90d / wallet_count,
      behavior_pattern = factor(behavior_pattern, levels = behavior_pattern[order(abs(flow_90d), decreasing = TRUE)])
    )
  
  # 2. Calculate market insights (ONCE)
  buying_pressure <- whale_summary %>%
    filter(flow_90d > 0) %>%
    summarise(
      total_buying = sum(total_flow),
      wallet_count = sum(wallet_count)
    )
  
  selling_pressure <- whale_summary %>%
    filter(flow_90d < 0) %>%
    summarise(
      total_selling = sum(total_flow),
      wallet_count = sum(wallet_count)
    )
  
  total_pressure <- (buying_pressure$total_buying - selling_pressure$total_selling) / 
    (buying_pressure$total_buying + selling_pressure$total_selling) * 100
  
  # 3. Create visualization
  whale_flow_chart <- ggplot(whale_summary, 
                            aes(x = reorder(behavior_pattern, abs(flow_90d)), y = flow_90d/1000)) +
    geom_bar(stat = "identity", 
            aes(fill = flow_90d > 0),
            width = 0.7) +
    scale_fill_manual(values = c("#FF4B4B", "#00C805")) +
    geom_text(aes(
      label = sprintf("%d whales\n$%.1fk/whale", 
                     wallet_count, 
                     abs(avg_flow_per_wallet/1000))
    ),
    hjust = ifelse(whale_summary$flow_90d > 0, -0.1, 1.1),
    size = 3) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::dollar_format(prefix="$", suffix="k"),
      limits = c(min(whale_summary$flow_90d/1000) * 1.2,
                max(whale_summary$flow_90d/1000) * 1.2),
      # Correct syntax for secondary axis
      sec.axis = dup_axis(
        name = NULL,
        labels = function(x) sprintf("%.0f%%", 
                                    x / sum(abs(whale_summary$flow_90d/1000)) * 100)
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      axis.text = element_text(size = 11)
    ) +
    labs(
      title = sprintf("Whale Flows by Behavior Pattern 🐋 | %s", toupper(token_name)),
      subtitle = sprintf("90d flows | As of %s | %s", 
                        format(Sys.Date(), "%Y-%m-%d"),
                        ifelse(total_pressure < -40, "🔴 Heavy Distribution",
                              ifelse(total_pressure < -20, "🟠 Distribution",
                                    ifelse(total_pressure < 20, "⚪ Neutral/Mixed",
                                          ifelse(total_pressure < 40, "🟡 Accumulation",
                                                "🟢 Heavy Accumulation"))))),
      y = "Net Flow (USD)",
      caption = "Bars show 90d net flow | Labels show wallet count and average position size"
    )
  
  # 4. Prepare insights
  insights <- list(
    strong_acc_share = ifelse("STRONG_ACCUMULATING" %in% whale_summary$behavior_pattern,
                             whale_summary$activity_share[whale_summary$behavior_pattern == "STRONG_ACCUMULATING"],
                             0),
    strong_acc_wallets = ifelse("STRONG_ACCUMULATING" %in% whale_summary$behavior_pattern,
                               whale_summary$wallet_count[whale_summary$behavior_pattern == "STRONG_ACCUMULATING"],
                               0),
    strong_dist_share = sum(whale_summary$activity_share[whale_summary$behavior_pattern %in% 
                                                       c("STRONG_DISTRIBUTING", "EXITED")], na.rm = TRUE),
    strong_dist_wallets = sum(whale_summary$wallet_count[whale_summary$behavior_pattern %in% 
                                                       c("STRONG_DISTRIBUTING", "EXITED")], na.rm = TRUE),
    net_pressure = total_pressure,
    total_flow = sum(whale_summary$total_flow),
    total_wallets = sum(whale_summary$wallet_count),
    trend_7d = sum(whale_summary$flow_7d) / sum(abs(whale_summary$flow_7d)) * 100,
    trend_30d = sum(whale_summary$flow_30d) / sum(abs(whale_summary$flow_30d)) * 100,
    avg_acc_size = ifelse(sum(whale_summary$wallet_count[whale_summary$flow_90d > 0]) > 0,
                         sum(whale_summary$total_flow[whale_summary$flow_90d > 0]) / 
                           sum(whale_summary$wallet_count[whale_summary$flow_90d > 0]) / 1e3,
                         0),
    avg_dist_size = ifelse(sum(whale_summary$wallet_count[whale_summary$flow_90d < 0]) > 0,
                          sum(whale_summary$total_flow[whale_summary$flow_90d < 0]) / 
                            sum(whale_summary$wallet_count[whale_summary$flow_90d < 0]) / 1e3,
                          0)
  )
  
  return(list(
    plot = whale_flow_chart,
    insights = insights,
    data = whale_summary
  ))
}

generate_whale_summary <- function(analysis) {
  with(analysis$insights, {
    # Get the key insight
    insight <- get_key_insight(
      trend_7d, trend_30d,
      strong_acc_share, strong_dist_share,
      strong_acc_wallets, strong_dist_wallets
    )
    
    # Pressure thresholds based on quartile analysis
    heavy_pressure <- 40    # ~75th percentile
    moderate_pressure <- 20 # ~50th percentile
    
    paste0(
      "🐋 Whale Analysis (90d)\n\n",
      
      # Key Signal
      "Signal: ", 
      if(net_pressure < -heavy_pressure) "🔴 Heavy Distribution" 
      else if(net_pressure < -moderate_pressure) "🟠 Distribution"
      else if(net_pressure < moderate_pressure) "⚪ Neutral/Mixed"
      else if(net_pressure < heavy_pressure) "🟡 Accumulation"
      else "🟢 Heavy Accumulation",
      "\n\n",
      
      # Key Stats
      "• ", strong_acc_wallets, " whales accumulating ($", sprintf("%.1f", avg_acc_size), "k avg)\n",
      "• ", strong_dist_wallets, " whales distributing ($", sprintf("%.1f", avg_dist_size), "k avg)\n",
      "• Total Volume: $", sprintf("%.1f", total_flow/1e6), "M\n\n",
      
      # Recent Movement
      "Recent Action:\n",
      "• 7d: ", ifelse(trend_7d > 0, "🟢", "🔴"), " ", 
      sprintf("%.0f", abs(trend_7d)), "% ", 
      ifelse(trend_7d > 0, "buy", "sell"), " pressure\n",
      "• 30d: ", ifelse(trend_30d > 0, "🟢", "🔴"), " ", 
      sprintf("%.0f", abs(trend_30d)), "% ", 
      ifelse(trend_30d > 0, "buy", "sell"), " pressure\n\n",
      
      # Key Insight
      insight
    )
  })
}

# ---------------------------- 3. BENCHMARK ANALYSIS -----------------------------
analyze_token <- function(token_address, token_name, benchmark_prices, token_names) {
  cat(sprintf("Analyzing token: %s (%s)\n", token_name, token_address))
  benchmark_prices_clean <- benchmark_prices %>%
    filter(open_price > 0 & close_price > 0) %>%
    arrange(token_address, time_interval)
  
  # Get the creation date of the analyzed token
  token_creation_date <- benchmark_prices_clean %>%
    filter(token_address == !!token_address) %>%
    summarise(min_date = min(time_interval)) %>%
    pull(min_date)
  
  cat(sprintf("Token creation date: %s\n", as.character(token_creation_date)))
  
  # Filter data to start from token's creation date
  benchmark_prices_clean <- benchmark_prices_clean %>%
    filter(time_interval >= token_creation_date)
  
  token_metrics <- benchmark_prices_clean %>%
    group_by(token_address) %>%
    arrange(time_interval) %>%
    mutate(
      price_vs_median = pmin(pmax(close_price / median(close_price, na.rm = TRUE), 0.01), 100),
      volume_vs_median = pmin(pmax(volume / median(volume, na.rm = TRUE), 0.01), 100),
      daily_return = 100 * (log(close_price) - log(lag(close_price))),
      rolling_volatility = roll_sd(daily_return, 7, align = "right", fill = NA),
      cumulative_return = cumprod(1 + daily_return/100) * 100
    ) %>%
    ungroup()
  
  # 2. Prepare comparison data - explicitly select analyzed token for token_data
  cat("Preparing token data...\n")
  token_data <- token_metrics %>%
    filter(token_address == !!token_address) %>%
    select(time_interval, close_price,
           daily_return, rolling_volatility, 
           price_vs_median, volume_vs_median, cumulative_return)
  
  # Calculate benchmark excluding the analyzed token
  cat("Calculating benchmark (excluding analyzed token)...\n")
  benchmark_avg <- token_metrics %>%
    filter(token_address != !!token_address) %>%  
    group_by(time_interval) %>%
    summarise(
      benchmark_return = mean(daily_return, na.rm = TRUE),
      benchmark_price_vs_median = mean(price_vs_median, na.rm = TRUE),
      cumulative_benchmark = cumprod(1 + benchmark_return/100) * 100,
      .groups = 'drop'
    )
  
  cat("Joining token data with benchmark...\n")
  combined_data <- token_data %>%
    inner_join(benchmark_avg, by = "time_interval") %>%
    mutate(
      raw_relative_strength = daily_return - benchmark_return,
      relative_strength = rollmean(raw_relative_strength, 
                                  k = 7, 
                                  fill = NA, 
                                  align = "right",
                                  na.rm = TRUE)
    ) %>%
    mutate(
      relative_strength = zoo::na.approx(relative_strength, 
                                        na.rm = FALSE, 
                                        rule = 2)
    )
  
  # 3. Core Performance Metrics
  cat("Calculating performance metrics...\n")
  performance_metrics <- combined_data %>%
    summarise(
      # Returns
      `30D Return` = if(n() >= 30) (last(close_price) / nth(close_price, n = nrow(.) - 29) - 1) * 100 else NA,
      `7D Return` = if(n() >= 7) (last(close_price) / nth(close_price, n = nrow(.) - 6) - 1) * 100 else NA,
      
      # Market Comparison
      `Win Rate vs Market` = mean(daily_return > benchmark_return, na.rm = TRUE) * 100,
      
      # Risk Metrics
      `Current Volatility` = if(n() >= 7) mean(tail(rolling_volatility, 7), na.rm = TRUE) else NA,
      `Volatility Trend` = if(n() >= 30) mean(tail(rolling_volatility, 7), na.rm = TRUE) / 
        mean(tail(rolling_volatility, 30), na.rm = TRUE) - 1 else NA,
      
      # Price Levels
      `Price vs Peak` = last(price_vs_median) / max(price_vs_median, na.rm = TRUE) * 100,
      `Current Price Level` = last(price_vs_median)
    )
  
  # 4. Market Signals
  cat("Generating market signals...\n")
  market_signals <- combined_data %>%
    summarise(
      # Momentum
      `Short-term Momentum` = if(n() >= 7) sum(tail(daily_return, 7) > 0, na.rm = TRUE) / 
        sum(!is.na(tail(daily_return, 7))) * 100 else NA,
      `Price Trend` = if(n() >= 30) case_when(
        mean(tail(price_vs_median, 30), na.rm = TRUE) > 
          mean(tail(price_vs_median, 7), na.rm = TRUE) ~ "Cooling",
        TRUE ~ "Heating"
      ) else "Insufficient Data",
      
      # Volume Analysis
      `Volume Trend` = if(n() >= 30) (mean(tail(volume_vs_median, 7), na.rm = TRUE)) / 
        (mean(tail(volume_vs_median, 30), na.rm = TRUE)) - 1 else NA,
      
      # Market Position
      `Market Position` = if(n() >= 30) case_when(
        last(price_vs_median) < quantile(price_vs_median, 0.25, na.rm = TRUE) ~ "Oversold",
        last(price_vs_median) > quantile(price_vs_median, 0.75, na.rm = TRUE) ~ "Overbought",
        TRUE ~ "Neutral"
      ) else "Insufficient Data",
      
      # Risk Assessment
      `Risk Level` = if(n() >= 7) case_when(
        mean(tail(rolling_volatility, 7), na.rm = TRUE) > 
          quantile(rolling_volatility, 0.75, na.rm = TRUE, names = FALSE) ~ "High",
        TRUE ~ "Normal"
      ) else "Insufficient Data"
    )
  
  cat("Analysis complete!\n")
  return(list(
    performance = performance_metrics,
    signals = market_signals,
    raw_data = combined_data
  ))
}

# ---------------------------- 4. VISUALIZATION -----------------------------
create_token_insights <- function(combined_data, token_name) {
  if (nrow(combined_data) == 0 || all(is.na(combined_data$price_vs_median))) {
    warning("No data available for plotting.")
    return(NULL)
  }
  plot_data <- combined_data %>%
    mutate(
      market_momentum = rollmean(benchmark_return, 14, fill = NA, align = "right"),
      token_momentum = rollmean(daily_return, 14, fill = NA, align = "right"),
      rel_momentum = (token_momentum - market_momentum),
      rel_momentum = pmin(pmax(rel_momentum, -40), 40),
      smooth_momentum = rollmean(rel_momentum, 5, fill = NA, align = "right"),
      momentum_direction = rollmean(smooth_momentum - lag(smooth_momentum, 5), 5, fill = NA, align = "right"),
      price_position = rollmean(price_vs_median, 5, fill = NA, align = "right"),
      lower_quartile = quantile(price_vs_median, 0.2, na.rm = TRUE),
      upper_quartile = quantile(price_vs_median, 0.8, na.rm = TRUE),
      position = case_when(
        price_position < lower_quartile & momentum_direction > 0 ~ "Oversold",
        price_position > upper_quartile & momentum_direction < 0 ~ "Overbought",
        TRUE ~ "Neutral"
      )
    )

  plot_data <- plot_data %>% filter(!is.na(smooth_momentum))
  min_date <- min(plot_data$time_interval, na.rm = TRUE)
  max_date <- max(plot_data$time_interval, na.rm = TRUE)

  zone_df <- plot_data %>%
    mutate(next_time = lead(time_interval)) %>%
    filter(!is.na(position) & !is.na(time_interval) & !is.na(next_time))

  momentum_plot <- ggplot() +
    geom_rect(
      data = zone_df,
      aes(
        xmin = time_interval,
        xmax = next_time,
        ymin = -40,
        ymax = 40,
        fill = position
      ),
      alpha = 0.08
    ) +
    geom_ribbon(
      data = plot_data,
      aes(
        x = time_interval,
        y = smooth_momentum,
        ymin = smooth_momentum - rolling_volatility,
        ymax = smooth_momentum + rolling_volatility
      ),
      alpha = 0.15, fill = "#2196F3"
    ) +
    geom_line(
      data = plot_data,
      aes(x = time_interval, y = smooth_momentum),
      color = "#2196F3", size = 1
    ) +
    geom_point(
      data = plot_data,
      aes(x = time_interval, y = smooth_momentum),
      color = "#2196F3", size = 1.5, alpha = 0.7
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
    scale_fill_manual(values = c(
      "Overbought" = "#FF4B4B",
      "Oversold" = "#00C805",
      "Neutral" = "gray95"
    )) +
    scale_x_datetime(
      date_breaks = "2 weeks",
      date_labels = "%b %d",
      expand = c(0.01, 0),
      limits = c(min_date, max_date)
    ) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(-40, 40)
    ) +
    labs(
      title = paste(token_name, "Market Momentum"),
      subtitle = paste0(
        "Momentum Differential: Token's 14-day return trend vs market average\n",
        "Above 0% = Outperforming market | Below 0% = Underperforming market\n",
        "Green zones = Oversold with improving momentum | Red zones = Overbought with weakening momentum"
      ),
      y = "Momentum Differential (%)",
      x = "Date"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, lineheight = 1.2),
      axis.title = element_text(size = 11),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )

  volume_dominance <- combined_data %>%
    mutate(
      buy_volume = ifelse(daily_return > 0, volume_vs_median, 0),
      sell_volume = ifelse(daily_return < 0, volume_vs_median, 0),
      buy_vol_trend = rollsum(buy_volume, 7, fill = NA, align = "right"),
      sell_vol_trend = rollsum(sell_volume, 7, fill = NA, align = "right"),
      vol_dominance = (buy_vol_trend - sell_vol_trend)/(buy_vol_trend + sell_vol_trend),
      price_trend_short = (price_vs_median - lag(price_vs_median, 5))/lag(price_vs_median, 5),
      price_trend_long = (price_vs_median - lag(price_vs_median, 14))/lag(price_vs_median, 14),
      vol_trend_short = rollmean(vol_dominance, 5, fill = NA, align = "right"),
      vol_trend_long = rollmean(vol_dominance, 14, fill = NA, align = "right"),
      divergence = case_when(
        price_trend_short < -0.05 &  
          price_trend_short < price_trend_long &  
          vol_trend_short > vol_trend_long &  
          vol_dominance > 0.3 ~ "Bearish Divergence",
        price_trend_short > 0.05 &  
          price_trend_short > price_trend_long &  
          vol_trend_short < vol_trend_long &  
          vol_dominance < -0.3 ~ "Bullish Divergence",
        vol_dominance > 0.5 & price_trend_short > 0.03 ~ "Strong Uptrend",
        vol_dominance < -0.5 & price_trend_short < -0.03 ~ "Strong Downtrend",
        TRUE ~ "Normal"
      )
    ) %>%
    ggplot(aes(x = time_interval)) +
    geom_col(aes(y = vol_dominance, fill = divergence),
            alpha = 0.8) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "white") +
    scale_fill_manual(values = c(
      "Bullish Divergence" = "#00C805",
      "Bearish Divergence" = "#FF4B4B",
      "Strong Uptrend" = "#FFA726",
      "Strong Downtrend" = "#FF7043",
      "Normal" = "#78909C"
    )) +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(-1, 1)
    ) +
    scale_x_datetime(
      date_breaks = "4 weeks",
      date_labels = "%b %d",
      expand = c(0.02, 0)
    ) +
    labs(
      title = "Volume Dominance & Divergences",
      subtitle = "Shows volume dominance and price-volume divergences\nBars above 0 = Buyers dominant | Below 0 = Sellers dominant",
      y = "Buy vs Sell Dominance",
      x = "Date",
      fill = "Signal"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, lineheight = 1.2),
      axis.title = element_text(size = 11),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      legend.text = element_text(size = 10)
    )

  # Arrange and print both plots
  combined_plot <- grid.arrange(
    momentum_plot,
    volume_dominance,
    ncol = 1,
    heights = c(1, 0.8),
    padding = unit(2, "line")
  )
  print(combined_plot)
  invisible(combined_plot)
}

# ---------------------------- 5. MAIN ANALYSIS RUNNER -----------------------------
# One-stop function to run the entire analysis
run_token_analysis <- function(token_address = NULL, token_name = NULL, data_dir = NULL) {
  # Auto-resolve token name if only address provided
  if(is.null(token_name) && !is.null(token_address)) {
    if(token_address %in% names(TOKEN_NAMES)) {
      token_name <- TOKEN_NAMES[token_address]
      cat(sprintf("Auto-resolved token name: %s\n", token_name))
    } else {
      stop("Token address not found in known tokens and no token name provided")
    }
  }
  
  # Auto-resolve token address if only name provided
  if(is.null(token_address) && !is.null(token_name)) {
    token_matches <- names(TOKEN_NAMES)[TOKEN_NAMES == token_name]
    if(length(token_matches) > 0) {
      token_address <- token_matches[1]
      cat(sprintf("Auto-resolved token address: %s\n", token_address))
    } else {
      stop("Token name not found in known tokens and no token address provided")
    }
  }
  
  # Validate inputs
  if(is.null(token_address) || is.null(token_name)) {
    stop("Both token_address and token_name must be provided or resolvable")
  }
  
  # Initialize analysis and load data
  cat("\n====== INITIALIZING ANALYSIS ======\n")
  analysis_config <- initialize_analysis(token_address, token_name, data_dir)
  
  # Run whale analysis
  cat("\n====== WHALE ANALYSIS ======\n")
  whale_analysis <- analyze_whales(analysis_config$whale_data, token_name)
  whale_summary <- generate_whale_summary(whale_analysis)
  
  # Run token market analysis
  cat("\n====== MARKET ANALYSIS ======\n")
  token_results <- tryCatch({
    analyze_token(
      token_address = token_address,
      token_name = token_name,
      benchmark_prices = analysis_config$benchmark_prices,
      token_names = TOKEN_NAMES
    )
  }, error = function(e) {
    cat("Error during market analysis: ", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(token_results)) {
    cat("Market analysis could not be completed due to errors.\n")
    return(NULL)
  }
  
  # Create visualizations
  cat("\n====== GENERATING VISUALIZATIONS ======\n")
  token_charts <- create_token_insights(token_results$raw_data, token_name)
  
  # Output summary metrics
  cat("\n====== PERFORMANCE METRICS ======\n")
  performance_df <- token_results$performance %>% 
    gather(metric, value) %>%
    mutate(value = sprintf("%.2f", value))
  for(i in 1:nrow(performance_df)) {
    cat(sprintf("%-25s: %s\n", performance_df$metric[i], performance_df$value[i]))
  }
  
  # Output market signals
  cat("\n====== MARKET SIGNALS ======\n")
  signals_df <- token_results$signals %>% 
    gather(metric, value) 
  for(i in 1:nrow(signals_df)) {
    cat(sprintf("%-25s: %s\n", signals_df$metric[i], signals_df$value[i]))
  }
  
  # Output whale summary
  cat("\n====== WHALE INSIGHTS ======\n")
  cat(whale_summary)
  
  # Return all analysis results
  return(list(
    config = analysis_config,
    whale_analysis = whale_analysis,
    market_analysis = token_results,
    charts = token_charts
  ))
}

# ---------------------------- 6. USAGE EXAMPLE -----------------------------
# Example usage 
analysis_results <- run_token_analysis(
  token_address = "DitHyRMQiSDhn5cnKMJV2CDDt6sVct96YrECiM49pump", 
  token_name = "House"
)

