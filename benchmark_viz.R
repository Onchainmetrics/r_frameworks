# ============================================================================
# BENCHMARK VISUALIZATION FRAMEWORK - Advanced Market Analysis & Divergence Detection
# ============================================================================
# Purpose: Combine price action (benchmark.sql) with whale behavior (whales_query.sql)
# to identify bullish/bearish divergences and optimal entry/exit signals
#
# Key Features:
# - Price momentum vs whale behavior divergences
# - Multi-timeframe volatility analysis  
# - Smart money flow vs retail behavior
# - Dynamic support/resistance based on whale positioning
# - Entry/exit signal generation
# ============================================================================

# --- Libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(lubridate)
library(TTR)
library(patchwork)
library(gridExtra)  # For arranging plots
library(viridis)
library(plotly)
library(zoo)
library(stringr)  # For str_detect function

# --- CONFIGURATION ---
setwd("C:/Users/Nitropc/Downloads")

# Token mapping - ADD YOUR TOKEN HERE
TOKEN_MAPPING <- list(
  "HOUSE" = "DitHyRMQiSDhn5cnKMJV2CDDt6sVct96YrECiM49pump",
  "BONK" = "DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263",
  "WIF" = "EKpQGSJtjMFqKZ9KQanSqYXRcF8fBopzLHYxdM65zcjm",
  "FARTCOIN" = "9BB6NFEcjBCtnNLFko2FqVQBq8HHM13kCyYcdQbgpump",
  "AI16Z" = "HeLp6NuQkmYB4pYWo2zYs22mESHXPQYzXbB8n4V98jwC",
  "ARC" = "61V8vBaqAGMpgDQi4JcAwo1dmBGHsyhzodcPqnEVpump",
  "SNAI" = "Hjw6bEcHtbHGpQr8onG3izfJY5DJiWdt7uk2BfdSpump",
  "TRUMP" = "6p6xgHyF7AeE6TZkSmFsko444wqoP15icUSqi2jfGiPN",
  "PNUT" = "2qEHjDLDLbuBgRYvsxhc5D6uDWAivNFZGan56P1tpump",
  "GIGA" = "63LfDmNb3MQ8mw9MtZ2To9bEA2M71kZUUGq5tiJxcqj9",
  "FWOG" = "A8C3xuqscfmyLrte3VmTqrAq8kgMASius9AFNANwpump",
  "UFD" = "eL5fUxj2J4CiQsmW85k5FG9DvuQjjUoBHoQBi2Kpump",
  "DOGEAI" = "9UYAYvVS2cZ3BndbsoG1ScJbjfwyEPGxjE79hh5ipump",
  "MEW" = "MEW1gQWJ3nEXg2qgERiKu7FAFj79PHvQVREQUzScPP5",
  "RETARDIO" = "6ogzHhzdrQr9Pgv6hZ2MNze7UrzBMAFyBBWUYp1Fhitx",
  "FLOYDAI" = "J7tYmq2JnQPvxyhcXpCDrvJnc9R5ts8rv7tgVHDPsw7U"
)

# Main configuration function - SET TOKEN NAME HERE
initialize_benchmark_analysis <- function(token_name, data_dir = NULL) {
  if(is.null(data_dir)) {
    data_dir <- getwd()
  }
  
  # Get contract address from mapping
  if(!token_name %in% names(TOKEN_MAPPING)) {
    stop(sprintf("Token %s not found in TOKEN_MAPPING. Please add it first.", token_name))
  }
  
  token_address <- TOKEN_MAPPING[[token_name]]
  
  cat(sprintf("Initializing benchmark analysis for %s (%s)\n", token_name, token_address))
  cat(sprintf("Data directory: %s\n", data_dir))
  
  # Create file paths based on token name (following your naming convention)
  whale_file <- file.path(data_dir, paste0(token_name, "_whales.csv"))
  benchmark_file <- file.path(data_dir, paste0(token_name, "_benchmark.csv"))
  
  # Verify files exist
  if(!file.exists(whale_file)) {
    stop(sprintf("Whale data file not found: %s", whale_file))
  }
  if(!file.exists(benchmark_file)) {
    stop(sprintf("Benchmark data file not found: %s", benchmark_file))
  }
  
  # Load data files
  whale_data <- read_csv(whale_file)
  benchmark_prices <- read_csv(benchmark_file) %>%
    mutate(time_interval = as.POSIXct(time_interval))
  
  # Filter benchmark data for target token
  token_benchmark_data <- benchmark_prices %>%
    filter(token_address == !!token_address)
  
  if(nrow(token_benchmark_data) == 0) {
    stop(sprintf("No benchmark data found for token %s (address: %s)", token_name, token_address))
  }
  
  cat(sprintf("Loaded %d price points and %d whale records for %s\n", 
              nrow(token_benchmark_data), nrow(whale_data), token_name))
  
  return(list(
    token_name = token_name,
    token_address = token_address,
    benchmark_data = token_benchmark_data,  # Filtered for target token
    full_benchmark_data = benchmark_prices, # Full dataset for comparisons
    whale_data = whale_data
  ))
}

# === SET YOUR TOKEN HERE ===
TOKEN_NAME <- "HOUSE"
config <- initialize_benchmark_analysis(TOKEN_NAME)

# Extract data for analysis
benchmark_data <- config$benchmark_data
whale_data <- config$whale_data
token_name <- config$token_name
token_address <- config$token_address

cat("Analyzing token:", token_name, "(", token_address, ")\n\n")

# ============================================================================
# 1. ENHANCED PRICE METRICS CALCULATION
# ============================================================================

calculate_advanced_metrics <- function(price_data) {
  price_data %>%
    arrange(time_interval) %>%
    mutate(
      # === MOMENTUM INDICATORS ===
      rsi_14 = RSI(close_price, n = 14),
      rsi_7 = RSI(close_price, n = 7),
      
      # === VOLATILITY METRICS ===
      returns = (close_price - lag(close_price)) / lag(close_price),
      rolling_vol_7 = rollapply(returns, 7, sd, na.rm = TRUE, fill = NA, align = "right"),
      rolling_vol_14 = rollapply(returns, 14, sd, na.rm = TRUE, fill = NA, align = "right"),
      vol_percentile = percent_rank(rolling_vol_14),
      
      # === TREND STRENGTH ===
      ema_12 = EMA(close_price, n = 12),
      ema_26 = EMA(close_price, n = 26),
      macd = ema_12 - ema_26,
      macd_signal = EMA(macd, n = 9),
      macd_histogram = macd - macd_signal,
      
      # === PRICE MOMENTUM ===
      momentum_10 = (close_price - lag(close_price, 10)) / lag(close_price, 10),
      momentum_20 = (close_price - lag(close_price, 20)) / lag(close_price, 20),
      
      # === BOLLINGER BANDS (Volatility + Mean Reversion) ===
      bb_mid = SMA(close_price, n = 20),
      bb_sd = rollapply(close_price, 20, sd, na.rm = TRUE, fill = NA, align = "right"),
      bb_upper = bb_mid + (2 * bb_sd),
      bb_lower = bb_mid - (2 * bb_sd),
      bb_position = (close_price - bb_lower) / (bb_upper - bb_lower),
      
      # === VOLUME ANALYSIS ===
      vol_sma = SMA(volume, n = 10),
      vol_ratio = volume / vol_sma,
      
      # === SUPPORT/RESISTANCE LEVELS ===
      pivot_high = ifelse(close_price > lag(close_price, 2) & 
                         close_price > lag(close_price, 1) & 
                         close_price > lead(close_price, 1) & 
                         close_price > lead(close_price, 2), close_price, NA),
      pivot_low = ifelse(close_price < lag(close_price, 2) & 
                        close_price < lag(close_price, 1) & 
                        close_price < lead(close_price, 1) & 
                        close_price < lead(close_price, 2), close_price, NA)
    )
}

# ============================================================================
# 2. WHALE BEHAVIOR AGGREGATION & CLASSIFICATION
# ============================================================================

process_whale_behavior <- function(whale_df, price_df) {
  
  # Aggregate whale metrics by time periods matching price data
  whale_summary <- whale_df %>%
    mutate(
      # Classify whale behavior strength
      behavior_strength = case_when(
        str_detect(behavior_pattern, "STRONG_ACCUMULATING|ALPHA_ACCUMULATING") ~ 5,
        str_detect(behavior_pattern, "ACCUMULATING") ~ 3,
        str_detect(behavior_pattern, "HOLDING|NEUTRAL") ~ 0,
        str_detect(behavior_pattern, "DISTRIBUTING") ~ -3,
        str_detect(behavior_pattern, "STRONG_DISTRIBUTING|ALPHA_DISTRIBUTING") ~ -5,
        str_detect(behavior_pattern, "MIXED") ~ 0,
        TRUE ~ 0
      ),
      
      # Weight by position size
      position_weight = pmin(usd_value / sum(usd_value, na.rm = TRUE), 0.1), # Cap max influence
      weighted_behavior = behavior_strength * position_weight,
      
      # Classify by wallet type
      whale_tier = case_when(
        usd_value >= 1000000 ~ "Mega Whale (>1M)",
        usd_value >= 100000 ~ "Large Whale (100K-1M)", 
        usd_value >= 10000 ~ "Medium Whale (10K-100K)",
        TRUE ~ "Small Holder (<10K)"
      )
    )
  
  # Create whale behavior index over time
  whale_flows <- whale_summary %>%
    summarise(
      total_holders = n(),
      accumulating_whales = sum(behavior_strength > 0, na.rm = TRUE),
      distributing_whales = sum(behavior_strength < 0, na.rm = TRUE),
      whale_sentiment_index = sum(weighted_behavior, na.rm = TRUE),
      avg_whale_behavior = mean(behavior_strength, na.rm = TRUE),
      mega_whale_sentiment = mean(behavior_strength[whale_tier == "Mega Whale (>1M)"], na.rm = TRUE),
      total_whale_value = sum(usd_value, na.rm = TRUE),
      
      # Flow metrics
      net_flow_7d = sum(net_position_7d_usd, na.rm = TRUE),
      net_flow_30d = sum(net_position_30d_usd, na.rm = TRUE),
      whale_momentum = net_flow_7d / abs(net_flow_30d + 1), # Acceleration
      
      # Smart money metrics
      alpha_accumulating = sum(str_detect(behavior_pattern, "ALPHA_ACCUMULATING"), na.rm = TRUE),
      alpha_distributing = sum(str_detect(behavior_pattern, "ALPHA_DISTRIBUTING"), na.rm = TRUE),
      alpha_sentiment = (alpha_accumulating - alpha_distributing) / (alpha_accumulating + alpha_distributing + 1)
    )
  
  return(list(summary = whale_summary, flows = whale_flows))
}

# ============================================================================
# 3. DIVERGENCE DETECTION ALGORITHM
# ============================================================================

detect_divergences <- function(price_df, whale_flows) {
  
  # Calculate key divergence signals
  combined_data <- price_df %>%
    mutate(
      # === PRICE-MOMENTUM DIVERGENCES ===
      price_trend_20 = sign(SMA(momentum_20, n = 5)),
      whale_trend = sign(whale_flows$whale_sentiment_index),
      
      # Bullish divergence: Price declining but whales accumulating
      bullish_divergence = (price_trend_20 < 0 & whale_trend > 0),
      
      # Bearish divergence: Price rising but whales distributing  
      bearish_divergence = (price_trend_20 > 0 & whale_trend < 0),
      
      # === VOLUME-PRICE DIVERGENCES ===
      volume_trend = sign(vol_ratio - 1),
      vol_price_divergence = (sign(momentum_10) != volume_trend),
      
      # === VOLATILITY-WHALE DIVERGENCES ===
      vol_regime = ifelse(vol_percentile > 0.8, "high", 
                         ifelse(vol_percentile < 0.2, "low", "normal")),
      whale_activity_level = ifelse(abs(whale_flows$whale_momentum) > quantile(abs(whale_flows$whale_momentum), 0.8, na.rm=T), 
                                   "high", "normal"),
      
      # === COMPOSITE SIGNALS ===
      # Strong bullish: Price oversold + Whale accumulation + Low volatility
      strong_bull_signal = (rsi_14 < 30 & whale_trend > 0 & vol_regime == "low"),
      
      # Strong bearish: Price overbought + Whale distribution + High volatility  
      strong_bear_signal = (rsi_14 > 70 & whale_trend < 0 & vol_regime == "high"),
      
      # === ENTRY/EXIT LEVELS ===
      dynamic_support = bb_lower + (whale_flows$whale_sentiment_index * 0.01 * close_price),
      dynamic_resistance = bb_upper - (whale_flows$whale_sentiment_index * 0.01 * close_price)
    )
  
  return(combined_data)
}

# ============================================================================
# 4. SIGNAL SCORING & RANKING SYSTEM
# ============================================================================

calculate_signal_scores <- function(data, whale_flows) {
  data %>%
    mutate(
      # Individual component scores (0-100)
      momentum_score = pmax(0, pmin(100, 50 + (momentum_20 * 500))),
      rsi_score = case_when(
        rsi_14 < 20 ~ 90,  # Extremely oversold = bullish
        rsi_14 < 30 ~ 70,  # Oversold = bullish
        rsi_14 > 80 ~ 10,  # Extremely overbought = bearish
        rsi_14 > 70 ~ 30,  # Overbought = bearish
        TRUE ~ 50          # Neutral
      ),
      whale_score = pmax(0, pmin(100, 50 + (whale_flows$whale_sentiment_index * 10))),
      volatility_score = case_when(
        vol_percentile < 0.2 ~ 80,  # Low vol = good for entries
        vol_percentile > 0.8 ~ 20,  # High vol = risky
        TRUE ~ 50
      ),
      
      # Composite signal strength (weighted average)
      composite_score = (
        momentum_score * 0.25 +
        rsi_score * 0.25 + 
        whale_score * 0.35 +  # Whale behavior weighted heavily
        volatility_score * 0.15
      ),
      
      # Signal classification
      signal_strength = case_when(
        composite_score >= 80 ~ "STRONG_BUY",
        composite_score >= 65 ~ "BUY", 
        composite_score >= 55 ~ "WEAK_BUY",
        composite_score <= 20 ~ "STRONG_SELL",
        composite_score <= 35 ~ "SELL",
        composite_score <= 45 ~ "WEAK_SELL",
        TRUE ~ "NEUTRAL"
      ),
      
      # Risk-adjusted signal (account for volatility)
      risk_adjusted_score = composite_score * (1 - (rolling_vol_14 * 2))
    )
}

# ============================================================================
# 5. ENHANCED VISUALIZATION FUNCTIONS (Replacing benchmark.R approach)
# ============================================================================

# Main integrated analysis chart (replaces the complex dashboard)
create_integrated_analysis <- function(data, token_name, whale_flows, whale_summary) {
  
  # Prepare plot data with key signals
  plot_data <- data %>%
    mutate(
      # Market momentum differential (like benchmark.R)
      market_momentum = rollmean(momentum_20, 10, fill = NA, align = "right"),
      smooth_momentum = pmin(pmax(market_momentum, -0.4), 0.4),
      
      # Signal zones (like benchmark.R oversold/overbought logic)
      signal_zone = case_when(
        composite_score >= 80 ~ "Strong Buy Zone",
        composite_score >= 65 ~ "Buy Zone", 
        composite_score >= 55 ~ "Weak Buy Zone",
        composite_score <= 20 ~ "Strong Sell Zone",
        composite_score <= 35 ~ "Sell Zone",
        composite_score <= 45 ~ "Weak Sell Zone",
        TRUE ~ "Neutral Zone"
      ),
      
      # Price position relative to range
      price_position = (close_price - min(close_price, na.rm = TRUE)) / 
                      (max(close_price, na.rm = TRUE) - min(close_price, na.rm = TRUE))
    )
  
  # Create time zones for background coloring
  zone_df <- plot_data %>%
    mutate(next_time = lead(time_interval)) %>%
    filter(!is.na(signal_zone) & !is.na(time_interval) & !is.na(next_time))
  
  # Main momentum chart (inspired by benchmark.R)
  momentum_chart <- ggplot() +
    geom_rect(
      data = zone_df,
      aes(
        xmin = time_interval,
        xmax = next_time,
        ymin = -40,
        ymax = 40,
        fill = signal_zone
      ),
      alpha = 0.15
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
    geom_line(
      data = plot_data,
      aes(x = time_interval, y = smooth_momentum * 100),
      color = "#2196F3", size = 1.2
    ) +
    geom_point(
      data = filter(plot_data, strong_bull_signal),
      aes(x = time_interval, y = smooth_momentum * 100),
      color = "#00C805", size = 3, alpha = 0.8
    ) +
    geom_point(
      data = filter(plot_data, strong_bear_signal),
      aes(x = time_interval, y = smooth_momentum * 100),
      color = "#FF4B4B", size = 3, alpha = 0.8
    ) +
    scale_fill_manual(values = c(
      "Strong Buy Zone" = "#00C805",
      "Buy Zone" = "#4CAF50", 
      "Weak Buy Zone" = "#8BC34A",
      "Neutral Zone" = "gray95",
      "Weak Sell Zone" = "#FF9800",
      "Sell Zone" = "#FF5722",
      "Strong Sell Zone" = "#FF4B4B"
    )) +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      limits = c(-40, 40)
    ) +
    labs(
      title = paste("$", token_name, "- Momentum Analysis & Signal Zones"),
      subtitle = paste0(
        "Current Signal: ", tail(plot_data$signal_strength, 1), " (Score: ", 
        round(tail(plot_data$composite_score, 1), 1), "/100) | ",
        "Whale Sentiment: ", ifelse(whale_flows$whale_sentiment_index > 0, "Bullish", "Bearish")
      ),
      y = "Momentum Score (%)",
      x = "Date"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(momentum_chart)
}

# Whale behavior integration (enhanced version of benchmark.R whale analysis)
create_whale_integration_chart <- function(whale_data, token_name) {
  
  # Process whale data similar to benchmark.R but with divergence focus
  whale_behavior_summary <- whale_data %>%
    filter(behavior_pattern != "MIXED") %>%
    mutate(
      behavior_strength = case_when(
        str_detect(behavior_pattern, "STRONG_ACCUMULATING|ALPHA_ACCUMULATING") ~ 5,
        str_detect(behavior_pattern, "ACCUMULATING") ~ 3,
        str_detect(behavior_pattern, "HOLDING|NEUTRAL") ~ 0,
        str_detect(behavior_pattern, "DISTRIBUTING") ~ -3,
        str_detect(behavior_pattern, "STRONG_DISTRIBUTING|ALPHA_DISTRIBUTING") ~ -5,
        TRUE ~ 0
      ),
      flow_7d_adj = case_when(
        behavior_pattern == "EXITED" ~ -abs(net_position_7d_usd),
        TRUE ~ net_position_7d_usd
      )
    ) %>%
    group_by(behavior_pattern) %>%
    summarise(
      flow_7d = sum(flow_7d_adj),
      wallet_count = n(),
      avg_position = mean(usd_value, na.rm = TRUE),
      total_value = sum(usd_value, na.rm = TRUE)
    ) %>%
    mutate(
      flow_impact = flow_7d / sum(abs(flow_7d)) * 100,
      behavior_pattern = factor(behavior_pattern, 
                               levels = behavior_pattern[order(abs(flow_7d), decreasing = TRUE)])
    )
  
  # Create whale flows chart (like benchmark.R)
  whale_chart <- ggplot(whale_behavior_summary, 
                       aes(x = reorder(behavior_pattern, abs(flow_7d)), y = flow_7d/1000)) +
    geom_bar(stat = "identity", 
             aes(fill = flow_7d > 0),
             width = 0.7) +
    scale_fill_manual(values = c("#FF4B4B", "#00C805")) +
    geom_text(aes(
      label = sprintf("%d whales\n$%.0fK avg", 
                     wallet_count, 
                     avg_position/1000)
    ),
    hjust = ifelse(whale_behavior_summary$flow_7d > 0, -0.1, 1.1),
    size = 3) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::dollar_format(prefix="$", suffix="K"),
      limits = c(min(whale_behavior_summary$flow_7d/1000) * 1.3,
                max(whale_behavior_summary$flow_7d/1000) * 1.3)
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    ) +
    labs(
      title = paste("$", token_name, "- Whale Flows (7d) 🐋"),
      subtitle = "Recent whale positioning changes",
      y = "Net Flow (USD)",
      x = NULL
    )
  
  return(whale_chart)
}

# Enhanced signal analysis with detailed explanations
create_enhanced_signal_summary <- function(data, whale_flows, token_name) {
  
  latest_data <- data %>% slice_tail(n = 1)
  
  # Get component scores
  momentum_score <- latest_data$momentum_score
  rsi_score <- latest_data$rsi_score  
  whale_score <- latest_data$whale_score
  volatility_score <- latest_data$volatility_score
  composite_score <- latest_data$composite_score
  signal_strength <- latest_data$signal_strength
  
  # Generate detailed explanation
  signal_explanation <- paste0(
    "🎯 SIGNAL ANALYSIS - $", token_name, "\n",
    paste0(rep("=", 50), collapse = ""), "\n\n",
    
    "📊 CURRENT SIGNAL: ", signal_strength, " (", round(composite_score, 1), "/100)\n\n",
    
    "💡 WHAT THIS MEANS:\n",
    case_when(
      signal_strength == "STRONG_BUY" ~ "🟢 High-conviction entry opportunity. All indicators align bullishly.",
      signal_strength == "BUY" ~ "🟢 Good entry opportunity. Majority of indicators are positive.",
      signal_strength == "WEAK_BUY" ~ "🟡 Modest bullish bias. Consider smaller position or wait for confirmation.",
      signal_strength == "STRONG_SELL" ~ "🔴 High-conviction exit signal. All indicators align bearishly.",
      signal_strength == "SELL" ~ "🔴 Exit signal. Majority of indicators are negative.",
      signal_strength == "WEAK_SELL" ~ "🟡 Modest bearish bias. Consider taking profits or reducing exposure.",
      TRUE ~ "⚪ No clear directional bias. Wait for better setup."
    ), "\n\n",
    
    "🔍 COMPONENT BREAKDOWN:\n",
    "• Momentum (25%): ", round(momentum_score, 1), "/100 - ",
    case_when(
      momentum_score >= 70 ~ "Strong upward price momentum",
      momentum_score >= 55 ~ "Positive momentum building",
      momentum_score <= 30 ~ "Strong downward momentum", 
      momentum_score <= 45 ~ "Negative momentum developing",
      TRUE ~ "Sideways/neutral momentum"
    ), "\n",
    
    "• RSI Signal (25%): ", round(rsi_score, 1), "/100 - ",
    case_when(
      rsi_score >= 80 ~ "Extremely oversold - bounce likely",
      rsi_score >= 60 ~ "Oversold conditions",
      rsi_score <= 20 ~ "Extremely overbought - pullback likely",
      rsi_score <= 40 ~ "Overbought conditions", 
      TRUE ~ "Neutral RSI levels"
    ), "\n",
    
    "• Whale Behavior (35%): ", round(whale_score, 1), "/100 - ",
    case_when(
      whale_score >= 70 ~ "Strong whale accumulation signal",
      whale_score >= 55 ~ "Whales showing accumulation bias",
      whale_score <= 30 ~ "Strong whale distribution signal",
      whale_score <= 45 ~ "Whales showing distribution bias",
      TRUE ~ "Mixed whale sentiment"
    ), "\n",
    
    "• Volatility (15%): ", round(volatility_score, 1), "/100 - ",
    case_when(
      volatility_score >= 70 ~ "Low volatility - good for entries",
      volatility_score <= 30 ~ "High volatility - increased risk",
      TRUE ~ "Normal volatility environment"
    ), "\n\n",
    
    "⚠️  RISK ASSESSMENT:\n",
    "• Risk-Adjusted Score: ", round(latest_data$risk_adjusted_score, 1), "/100\n",
    "• Current Volatility: ", round(latest_data$vol_percentile * 100, 1), "th percentile\n",
    if(latest_data$vol_percentile > 0.8) "🔴 High volatility regime - use smaller positions" 
    else if(latest_data$vol_percentile < 0.2) "🟢 Low volatility regime - favorable for entries"
    else "🟡 Normal volatility regime"
  )
  
  return(signal_explanation)
}

# Enhanced divergence analysis with interpretation
create_enhanced_divergence_analysis <- function(data, token_name) {
  
  # Identify significant divergences  
  divergences <- data %>%
    filter(bullish_divergence | bearish_divergence) %>%
    mutate(
      divergence_type = ifelse(bullish_divergence, "Bullish", "Bearish"),
      forward_return_7d = (lead(close_price, 7) - close_price) / close_price,
      forward_return_14d = (lead(close_price, 14) - close_price) / close_price
    )
  
  if(nrow(divergences) == 0) {
    return("📈 DIVERGENCE ANALYSIS - No significant divergences detected in current data")
  }
  
  # Performance analysis
  divergence_stats <- divergences %>%
    group_by(divergence_type) %>%
    summarise(
      count = n(),
      avg_7d_return = mean(forward_return_7d, na.rm = TRUE),
      avg_14d_return = mean(forward_return_14d, na.rm = TRUE), 
      success_rate_7d = mean(forward_return_7d > 0, na.rm = TRUE),
      success_rate_14d = mean(forward_return_14d > 0, na.rm = TRUE),
      max_7d_gain = max(forward_return_7d, na.rm = TRUE),
      max_7d_loss = min(forward_return_7d, na.rm = TRUE)
    )
  
  # Generate explanation
  divergence_explanation <- paste0(
    "📈 DIVERGENCE ANALYSIS - $", token_name, "\n",
    paste0(rep("=", 50), collapse = ""), "\n\n"
  )
  
  for(i in 1:nrow(divergence_stats)) {
    div_type <- divergence_stats$divergence_type[i]
    count <- divergence_stats$count[i]
    success_7d <- divergence_stats$success_rate_7d[i] * 100
    avg_return_7d <- divergence_stats$avg_7d_return[i] * 100
    max_gain <- divergence_stats$max_7d_gain[i] * 100
    max_loss <- divergence_stats$max_7d_loss[i] * 100
    
    signal_quality <- case_when(
      success_7d >= 60 ~ "🟢 High Quality",
      success_7d >= 40 ~ "🟡 Medium Quality", 
      TRUE ~ "🔴 Low Quality"
    )
    
    divergence_explanation <- paste0(divergence_explanation,
      "🔍 ", toupper(div_type), " DIVERGENCE SIGNALS:\n",
      "• Signal Count: ", count, " occurrences\n",
      "• Success Rate (7d): ", round(success_7d, 1), "% ", signal_quality, "\n",
      "• Average Return (7d): ", sprintf("%+.1f%%", avg_return_7d), "\n",
      "• Best Case: ", sprintf("%+.1f%%", max_gain), " | Worst Case: ", sprintf("%+.1f%%", max_loss), "\n",
      
      "💡 INTERPRETATION: ",
      if(div_type == "Bullish") {
        if(success_7d >= 60) "Strong contrarian signal - price weakness with whale accumulation often leads to rebounds"
        else if(success_7d >= 40) "Moderate signal - whales accumulating but price may need more time to respond"  
        else "Weak signal - whale accumulation not translating to price strength yet"
      } else {
        if(success_7d >= 60) "Strong warning signal - price strength with whale distribution often precedes corrections"
        else if(success_7d >= 40) "Moderate caution - whales distributing but price momentum may continue short-term"
        else "Weak signal - whale distribution not impacting price action significantly"
      }, "\n\n"
    )
  }
  
  return(divergence_explanation)
}

# ============================================================================
# 6. MAIN EXECUTION PIPELINE
# ============================================================================

run_benchmark_analysis <- function(token_name, benchmark_data, whale_data) {
  
  cat("=== BENCHMARK ANALYSIS PIPELINE ===\n")
  cat("Target Token: $", token_name, "\n\n")
  
  # Use all benchmark data (single token analysis)
  price_data <- benchmark_data
  
  if(nrow(price_data) == 0) {
    stop("No price data found for token: ", token_name)
  }
  
  # Calculate advanced metrics
  cat("1. Calculating advanced price metrics...\n")
  price_enhanced <- calculate_advanced_metrics(price_data)
  
  # Process whale behavior
  cat("2. Processing whale behavior data...\n")
  whale_processed <- process_whale_behavior(whale_data, price_enhanced)
  
  # Detect divergences
  cat("3. Detecting divergences and signals...\n")
  signals_data <- detect_divergences(price_enhanced, whale_processed$flows)
  
  # Calculate signal scores
  cat("4. Calculating signal scores...\n")
  final_data <- calculate_signal_scores(signals_data, whale_processed$flows)
  
  # Generate enhanced visualizations and summaries
  cat("5. Creating enhanced analysis and visualizations...\n")
  
  # Create integrated analysis chart (main visualization)
  momentum_chart <- create_integrated_analysis(final_data, token_name, whale_processed$flows, whale_processed$summary)
  
  # Create whale behavior chart
  whale_chart <- create_whale_integration_chart(whale_data, token_name)
  
  # Generate detailed summaries
  enhanced_signal_summary <- create_enhanced_signal_summary(final_data, whale_processed$flows, token_name)
  enhanced_divergence_analysis <- create_enhanced_divergence_analysis(final_data, token_name)
  
  # Display results
  cat("\n")
  cat(enhanced_signal_summary)
  cat("\n\n")
  cat(enhanced_divergence_analysis)
  
  # Display visualizations
  print(momentum_chart)
  print(whale_chart)
  
  # Return comprehensive results
  return(list(
    data = final_data,
    momentum_chart = momentum_chart,
    whale_chart = whale_chart,
    signal_summary = enhanced_signal_summary,
    divergence_analysis = enhanced_divergence_analysis,
    whale_summary = whale_processed$summary
  ))
}

# ============================================================================
# 7. EXECUTE ANALYSIS
# ============================================================================

# Run analysis for the configured token
results <- run_benchmark_analysis(token_name, benchmark_data, whale_data)

# Save processed data for future use with token name
output_file_data <- paste0(token_name, "_benchmark_analysis_results.csv")
output_file_whale <- paste0(token_name, "_whale_behavior_processed.csv")

write_csv(results$data, output_file_data)
write_csv(results$whale_summary, output_file_whale)

cat("\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")
cat("🎉 ENHANCED BENCHMARK ANALYSIS COMPLETE FOR $", token_name, "\n")
cat(paste0(rep("=", 60), collapse = ""), "\n\n")

cat("📊 ANALYSIS COMPONENTS GENERATED:\n")
cat("• Enhanced Signal Analysis with detailed explanations\n")  
cat("• Divergence Performance Analysis with success rates\n")
cat("• Integrated Momentum Chart with signal zones\n")
cat("• Whale Behavior Integration Chart\n")
cat("• Risk-adjusted scoring system\n\n")

cat("💾 DATA EXPORTS:\n")
cat("• Processed analysis data:", output_file_data, "\n")
cat("• Whale behavioral data:", output_file_whale, "\n\n")

cat("🔧 ACCESS RESULTS:\n")
cat("• results$signal_summary - Detailed signal explanation\n")
cat("• results$divergence_analysis - Divergence performance analysis\n") 
cat("• results$momentum_chart - Main momentum visualization\n")
cat("• results$whale_chart - Whale behavior visualization\n")
cat("• results$data - Full processed dataset\n\n")

# ============================================================================
# 8. USAGE INSTRUCTIONS
# ============================================================================
cat("🔄 TO ANALYZE DIFFERENT TOKEN:\n")
cat("1. Update TOKEN_NAME variable (around line 105)\n")
cat("2. Ensure files exist: {TOKEN_NAME}_whales.csv and {TOKEN_NAME}_benchmark.csv\n")
cat("3. Add token to TOKEN_MAPPING if not already present\n")
cat("4. Re-run the script\n\n")
cat("📝 Example: TOKEN_NAME <- \"BONK\" analyzes BONK using BONK_whales.csv and BONK_benchmark.csv\n")