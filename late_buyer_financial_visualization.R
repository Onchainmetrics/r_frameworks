# Late Buyer Financial Metrics Visualization
# This script creates comprehensive financial analysis visualizations for late buyer entities
# Prerequisites: Run late_buyer_financial_metrics.sql first and export as CSV

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)
library(RColorBrewer)
library(ggpubr)
library(viridis)
library(ggrepel)  # For geom_text_repel

# Load the financial metrics data (exported from Dune query)
setwd("C:/Users/Nitropc/Downloads")
late_buyer_financials <- read.csv("late_buyer_financial_metrics.csv", stringsAsFactors = FALSE)

# Data preprocessing
late_buyer_financials <- late_buyer_financials %>%
  mutate(
    # Convert to numeric (in case of import issues)
    across(c(total_invested_usd, total_portfolio_value_usd, realized_pnl_usd, 
             unrealized_pnl_usd, total_trading_pnl_usd, trading_roi_percent), as.numeric),
    
    # Create profit/loss categories
    pnl_category = case_when(
      total_trading_pnl_usd > 50000 ~ "High Profit (>$50K)",
      total_trading_pnl_usd > 0 ~ "Small Profit ($0-$50K)", 
      total_trading_pnl_usd > -50000 ~ "Small Loss ($0-$50K)",
      TRUE ~ "High Loss (>$50K)"
    ),
    pnl_category = factor(pnl_category, levels = c("High Profit (>$50K)", "Small Profit ($0-$50K)", 
                                                  "Small Loss ($0-$50K)", "High Loss (>$50K)")),
    
    # ROI categories
    roi_category = case_when(
      trading_roi_percent > 100 ~ "Moonshot (>100%)",
      trading_roi_percent > 25 ~ "Great (25-100%)",
      trading_roi_percent > 0 ~ "Positive (0-25%)",
      trading_roi_percent > -50 ~ "Moderate Loss (0 to -50%)",
      TRUE ~ "Heavy Loss (<-50%)"
    ),
    roi_category = factor(roi_category, levels = c("Moonshot (>100%)", "Great (25-100%)", 
                                                  "Positive (0-25%)", "Moderate Loss (0 to -50%)", 
                                                  "Heavy Loss (<-50%)")),
    
    # Position composition categories
    composition_category = case_when(
      transferred_pct >= 90 ~ "Mostly Transferred (90%+)",
      transferred_pct >= 50 ~ "Mixed Portfolio (50-90%)",
      transferred_pct >= 10 ~ "Mostly Purchased (10-50%)",
      TRUE ~ "All Purchased (<10%)"
    ),
    
    # Entity type for cleaner display
    entity_type = ifelse(grepl("Cluster", entity_name), "Cluster", "Individual"),
    
    # Investment size categories
    investment_tier = case_when(
      total_invested_usd >= 500000 ~ "Whale (≥$500K)",
      total_invested_usd >= 100000 ~ "Large (≥$100K)",
      total_invested_usd >= 10000 ~ "Medium (≥$10K)",
      total_invested_usd > 0 ~ "Small (<$10K)",
      TRUE ~ "No Trades"
    )
  )

cat("=== LATE BUYER FINANCIAL ANALYSIS SUMMARY ===\n")
cat("Total wallets analyzed:", nrow(late_buyer_financials), "\n")
cat("Total invested across all wallets:", dollar(sum(late_buyer_financials$total_invested_usd, na.rm = TRUE)), "\n")
cat("Total current portfolio value:", dollar(sum(late_buyer_financials$total_portfolio_value_usd, na.rm = TRUE)), "\n")
cat("Total trading PnL:", dollar(sum(late_buyer_financials$total_trading_pnl_usd, na.rm = TRUE)), "\n")

# --- 1. Portfolio Composition: Purchased vs Transferred ---
portfolio_data <- late_buyer_financials %>%
  select(wallet_short, entity_type, purchased_tokens_value_usd, transferred_tokens_value_usd, transferred_pct) %>%
  pivot_longer(cols = c(purchased_tokens_value_usd, transferred_tokens_value_usd), 
               names_to = "token_source", values_to = "usd_value") %>%
  mutate(
    token_source = case_when(
      token_source == "purchased_tokens_value_usd" ~ "Purchased",
      token_source == "transferred_tokens_value_usd" ~ "Transferred"
    ),
    token_source = factor(token_source, levels = c("Purchased", "Transferred"))
  ) %>%
  arrange(desc(transferred_pct))

p1 <- ggplot(portfolio_data, aes(x = reorder(wallet_short, transferred_pct), y = usd_value, fill = token_source)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("Purchased" = "#2E86AB", "Transferred" = "#A23B72")) +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Portfolio Composition: Purchased vs Transferred Tokens",
    subtitle = "Blue = Bought from market, Pink = Received via transfers",
    x = "Wallet",
    y = "Portfolio Value (USD)",
    fill = "Token Source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 9)
  )

# --- 2. Trading Performance: Investment vs Current Value ---
performance_data <- late_buyer_financials %>%
  filter(total_invested_usd > 0) %>%  # Only show wallets that actually traded
  select(wallet_short, total_invested_usd, purchased_tokens_value_usd, total_trading_pnl_usd) %>%
  pivot_longer(cols = c(total_invested_usd, purchased_tokens_value_usd), 
               names_to = "metric", values_to = "usd_value") %>%
  mutate(
    metric = case_when(
      metric == "total_invested_usd" ~ "Total Invested",
      metric == "purchased_tokens_value_usd" ~ "Current Value"
    ),
    metric = factor(metric, levels = c("Total Invested", "Current Value"))
  )

p2 <- ggplot(performance_data, aes(x = reorder(wallet_short, -usd_value), y = usd_value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Total Invested" = "#3D5A80", "Current Value" = "#EE6C4D")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Trading Performance: Money In vs Current Worth",
    subtitle = "Blue = Amount Invested, Red = Current Value of Purchased Tokens",
    x = "Wallet",
    y = "USD Value",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top"
  )

# --- 3. Profit/Loss Waterfall ---
pnl_colors <- c("High Profit (>$50K)" = "#27AE60", "Small Profit ($0-$50K)" = "#52C41A", 
                "Small Loss ($0-$50K)" = "#FFA940", "High Loss (>$50K)" = "#FF4D4F")

p3 <- ggplot(late_buyer_financials, aes(x = reorder(wallet_short, -total_trading_pnl_usd), y = total_trading_pnl_usd)) +
  geom_col(aes(fill = pnl_category), alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_fill_manual(values = pnl_colors) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Total Trading PnL by Late Buyer",
    subtitle = "Green = Profit, Red = Loss (only from actual trading, excludes transferred tokens)",
    x = "Wallet", 
    y = "Total Trading PnL (USD)",
    fill = "PnL Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top"
  )

# --- 4. ROI vs Investment Size Scatter ---
traders_only <- late_buyer_financials %>% filter(total_invested_usd > 0)

p4 <- ggplot(traders_only, aes(x = total_invested_usd, y = trading_roi_percent)) +
  geom_point(aes(size = total_balance_millions, color = transferred_pct), alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text_repel(aes(label = wallet_short), size = 2.5, max.overlaps = 15) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_size_continuous(range = c(2, 8), name = "Holdings (M)") +
  scale_color_viridis_c(name = "Transferred %", option = "plasma") +
  labs(
    title = "Trading ROI vs Investment Size",
    subtitle = "Bubble size = Total holdings, Color = % from transfers",
    x = "Total Invested (USD)",
    y = "Trading ROI (%)",
    caption = "Only includes wallets that actually traded"
  ) +
  theme_minimal() +
  guides(size = guide_legend(override.aes = list(alpha = 0.7)))

# --- 5. Realized vs Unrealized PnL ---
pnl_breakdown <- late_buyer_financials %>%
  filter(total_invested_usd > 0) %>%
  select(wallet_short, realized_pnl_usd, unrealized_pnl_usd) %>%
  pivot_longer(cols = c(realized_pnl_usd, unrealized_pnl_usd), 
               names_to = "pnl_type", values_to = "pnl_value") %>%
  mutate(
    pnl_type = case_when(
      pnl_type == "realized_pnl_usd" ~ "Realized PnL",
      pnl_type == "unrealized_pnl_usd" ~ "Unrealized PnL"
    )
  )

p5 <- ggplot(pnl_breakdown, aes(x = reorder(wallet_short, pnl_value), y = pnl_value, fill = pnl_type)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Realized PnL" = "#8E44AD", "Unrealized PnL" = "#F39C12")) +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  labs(
    title = "Realized vs Unrealized PnL",
    subtitle = "Purple = Already Sold, Orange = Paper Gains/Losses", 
    x = "Wallet",
    y = "PnL (USD)",
    fill = "PnL Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 8))

# --- 6. Behavioral Analysis ---
behavior_counts <- late_buyer_financials %>%
  count(behavior_type) %>%
  mutate(percentage = n/sum(n)*100)

p6 <- ggplot(behavior_counts, aes(x = reorder(behavior_type, -n), y = n, fill = behavior_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", round(percentage,1), "%)")), 
            vjust = -0.2, size = 4, fontweight = "bold") +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(
    title = "Late Buyer Trading Behavior Distribution",
    subtitle = "Classification based on buy/sell activity patterns",
    x = "Behavior Type",
    y = "Count",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# --- 7. Investment Tier Analysis ---
investment_summary <- late_buyer_financials %>%
  group_by(investment_tier) %>%
  summarise(
    count = n(),
    avg_invested = mean(total_invested_usd, na.rm = TRUE),
    avg_current_value = mean(purchased_tokens_value_usd, na.rm = TRUE),
    avg_roi = mean(trading_roi_percent, na.rm = TRUE),
    avg_transferred_pct = mean(transferred_pct, na.rm = TRUE),
    total_invested = sum(total_invested_usd, na.rm = TRUE),
    .groups = "drop"
  )

p7 <- ggplot(investment_summary, aes(x = reorder(investment_tier, -total_invested), y = avg_roi)) +
  geom_col(aes(fill = investment_tier), alpha = 0.8) +
  geom_text(aes(label = paste0(count, " wallets")), vjust = -0.3, size = 3) +
  scale_fill_viridis_d(option = "turbo") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Average ROI by Investment Tier",
    subtitle = "Number shows wallet count in each tier",
    x = "Investment Tier",
    y = "Average Trading ROI (%)",
    fill = "Tier"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Display plots in manageable groups to avoid viewport issues

# Group 1: Portfolio Analysis
cat("=== PORTFOLIO COMPOSITION & TRADING PERFORMANCE ===\n")
plot1_group <- (p1 | p2)
print(plot1_group)

# Group 2: PnL Analysis  
cat("\n=== PROFIT/LOSS & ROI ANALYSIS ===\n")
plot2_group <- (p3 | p4)
print(plot2_group)

# Group 3: Detailed Breakdown
cat("\n=== DETAILED PNL & BEHAVIORAL ANALYSIS ===\n") 
plot3_group <- (p5 | p6)
print(plot3_group)

# Group 4: Investment Tier Analysis
cat("\n=== INVESTMENT TIER ANALYSIS ===\n")
print(p7)

# --- Summary Statistics ---
cat("\n=== DETAILED FINANCIAL SUMMARY ===\n")
cat("Wallets with trading activity:", sum(late_buyer_financials$total_invested_usd > 0), "out of", nrow(late_buyer_financials), "\n")
cat("Total invested across all wallets:", dollar(sum(late_buyer_financials$total_invested_usd, na.rm = TRUE)), "\n")
cat("Total current portfolio value:", dollar(sum(late_buyer_financials$total_portfolio_value_usd, na.rm = TRUE)), "\n")
cat("Total value from transfers:", dollar(sum(late_buyer_financials$transferred_tokens_value_usd, na.rm = TRUE)), "\n")
cat("Total trading PnL:", dollar(sum(late_buyer_financials$total_trading_pnl_usd, na.rm = TRUE)), "\n")

profitable <- late_buyer_financials %>% filter(total_trading_pnl_usd > 0)
unprofitable <- late_buyer_financials %>% filter(total_trading_pnl_usd <= 0)

cat("\n=== PERFORMANCE BREAKDOWN ===\n")
cat("Profitable wallets:", nrow(profitable), "out of", nrow(late_buyer_financials), "\n")
if(nrow(profitable) > 0) {
  cat("Average profit per profitable wallet:", dollar(mean(profitable$total_trading_pnl_usd, na.rm = TRUE)), "\n")
  cat("Average ROI for profitable wallets:", round(mean(profitable$trading_roi_percent, na.rm = TRUE), 1), "%\n")
}
if(nrow(unprofitable) > 0) {
  cat("Average loss per unprofitable wallet:", dollar(mean(unprofitable$total_trading_pnl_usd, na.rm = TRUE)), "\n")
  cat("Average ROI for unprofitable wallets:", round(mean(unprofitable$trading_roi_percent, na.rm = TRUE), 1), "%\n")
}

cat("\n=== TOKEN SOURCE ANALYSIS ===\n")
cat("Average percentage from transfers:", round(mean(late_buyer_financials$transferred_pct, na.rm = TRUE), 1), "%\n")
wallets_mostly_transferred <- sum(late_buyer_financials$transferred_pct >= 50)
cat("Wallets with >50% transferred tokens:", wallets_mostly_transferred, "out of", nrow(late_buyer_financials), "\n")

print("\n=== TOP PERFORMERS ===")
top_performers <- late_buyer_financials %>%
  filter(total_invested_usd > 1000) %>%  # Minimum $1K investment
  arrange(desc(trading_roi_percent)) %>%
  head(5) %>%
  select(wallet_short, total_invested_usd, total_trading_pnl_usd, trading_roi_percent, behavior_type)
print(top_performers)

print("\n=== BIGGEST LOSERS ===")
biggest_losers <- late_buyer_financials %>%
  filter(total_invested_usd > 1000) %>%  # Minimum $1K investment
  arrange(trading_roi_percent) %>%
  head(5) %>%
  select(wallet_short, total_invested_usd, total_trading_pnl_usd, trading_roi_percent, behavior_type)
print(biggest_losers)

# --- 8. LATE BUYER FLOW HEATMAPS ---
cat("\n=== CREATING LATE BUYER FLOW HEATMAPS ===\n")

# Check if flow columns exist in the data
if(!"flow_7d_usd" %in% colnames(late_buyer_financials)) {
  cat("ERROR: Flow columns not found in late_buyer_financials!\n")
  cat("Please re-run the late_buyer_financial_metrics.sql query with the updated version that includes flow calculations.\n")
  cat("Required columns: flow_7d_usd, flow_30d_usd, flow_90d_usd\n")
  cat("Current columns:", paste(colnames(late_buyer_financials), collapse = ", "), "\n")
} else {
  cat("Flow columns found. Proceeding with heatmap creation...\n")
  
  # Prepare flow data using actual flow columns from the SQL query
  late_buyer_flow_data <- late_buyer_financials %>%
    mutate(
      entity_name_clean = ifelse(grepl("Cluster", entity_name), entity_name, paste0(substr(wallet_address, 1, 8), "...")),
      is_cluster = grepl("Cluster", entity_name)
    ) %>%
    filter(!is.na(flow_7d_usd) & !is.na(flow_30d_usd) & !is.na(flow_90d_usd))
  # --- 8A. Late Buyer Cluster Heatmap ---
  late_buyer_clusters <- late_buyer_flow_data %>%
    filter(is_cluster == TRUE) %>%
    arrange(desc(total_portfolio_value_usd)) %>%
    head(8)  # Top clusters

  if(nrow(late_buyer_clusters) > 0) {
    cat("Late buyer clusters found:", nrow(late_buyer_clusters), "\n")
    
    # Create cluster flow heatmap data
    cluster_flows <- late_buyer_clusters %>%
    select(entity_name_clean, flow_7d_usd, flow_30d_usd, flow_90d_usd) %>%
    pivot_longer(cols = ends_with("_usd"), names_to = "period", values_to = "flow") %>%
    mutate(
      period = case_when(
        period == "flow_7d_usd" ~ "7D",
        period == "flow_30d_usd" ~ "30D",
        period == "flow_90d_usd" ~ "90D"
      )
    )
  
  p8a <- ggplot(cluster_flows, aes(x = period, y = reorder(entity_name_clean, flow), fill = flow)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#E74C3C", mid = "white", high = "#27AE60",
      midpoint = 0, name = "Net Flow\n(USD)",
      labels = scales::dollar_format()
    ) +
    geom_text(aes(label = scales::dollar(flow, scale = 1e-3, suffix = "K")), 
              size = 3, fontface = "bold") +
    labs(
      title = "Late Buyer Cluster Activity Heatmap",
      subtitle = "Green = Net Buying, Red = Net Selling",
      x = "Time Period",
      y = "Late Buyer Cluster"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
      axis.text.y = element_text(size = 10)
    )
  
    cat("\n=== LATE BUYER CLUSTER HEATMAP ===\n")
    print(p8a)
    
  } else {
    cat("No late buyer clusters found in the data.\n")
  }

  # --- 8B. Late Buyer Individual Heatmap ---
  late_buyer_individuals <- late_buyer_flow_data %>%
  filter(is_cluster == FALSE) %>%
  mutate(
    # Create notability score similar to insider analysis
    notability_score = total_portfolio_value_usd + 
                      abs(pmin(flow_30d_usd, 0)) + # Absolute value of negative flows (selling)
                      abs(pmin(flow_90d_usd, 0)) + # Captures big sellers
                      (total_invested_usd * 0.1)   # Include investment history
  ) %>%
  arrange(desc(notability_score)) %>%
  head(8)  # Top 8 most notable individuals

  cat("Late buyer individuals found:", nrow(late_buyer_individuals), "\n")

  print("=== TOP 8 MOST NOTABLE LATE BUYER INDIVIDUALS ===")
  individual_summary <- late_buyer_individuals %>%
  select(entity_name_clean, total_portfolio_value_usd, flow_7d_usd, flow_30d_usd, 
         flow_90d_usd, total_invested_usd, behavior_type, notability_score) %>%
  mutate(across(c(total_portfolio_value_usd, flow_7d_usd, flow_30d_usd, flow_90d_usd, 
                  total_invested_usd, notability_score), ~scales::dollar(.)))
  print(individual_summary)

  # Create individual flow heatmap data
  individual_flows <- late_buyer_individuals %>%
  select(entity_name_clean, flow_7d_usd, flow_30d_usd, flow_90d_usd) %>%
  pivot_longer(cols = ends_with("_usd"), names_to = "period", values_to = "flow") %>%
  mutate(
    period = case_when(
      period == "flow_7d_usd" ~ "7D",
      period == "flow_30d_usd" ~ "30D",
      period == "flow_90d_usd" ~ "90D"
    )
  )

p8b <- ggplot(individual_flows, aes(x = period, y = reorder(entity_name_clean, flow), fill = flow)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#27AE60",
    midpoint = 0, name = "Net Flow\n(USD)",
    labels = scales::dollar_format()
  ) +
  geom_text(aes(label = scales::dollar(flow, scale = 1e-3, suffix = "K")), 
            size = 3, fontface = "bold") +
  labs(
    title = "Top Late Buyer Individual Traders Activity Heatmap",
    subtitle = "Most notable individuals by holdings + selling activity",
    x = "Time Period",
    y = "Individual (Address)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.text.y = element_text(size = 10)
  )

  cat("\n=== LATE BUYER INDIVIDUAL HEATMAP ===\n")
  print(p8b)

  # Export late buyer flow data for reference
  write.csv(late_buyer_clusters, "late_buyer_cluster_flows.csv", row.names = FALSE)
  write.csv(late_buyer_individuals, "late_buyer_individual_flows.csv", row.names = FALSE)

  cat("\n=== LATE BUYER HEATMAP ANALYSIS COMPLETE ===\n")
  cat("Now you have 4 separate heatmap visualizations:\n")
  cat("1. Insider clusters (in insider_activity_analysis.R)\n")
  cat("2. Insider individuals (in insider_activity_analysis.R)\n")
  cat("3. Late buyer clusters (above)\n")
  cat("4. Late buyer individuals (above)\n")
}