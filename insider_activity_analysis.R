
# --- Insider Activity Analysis ---
# Analyzing patterns in clusters vs individuals and their trading behavior
#
# IMPORTANT: Column Mapping for Complete Timeline Query
# ================================================
# The insider_supply_timeline.sql query uses these column names:
# - addresses_included (new debugging column - should always be 322)
# - insider_supply_pct (same)
# - insider_balance_millions (same)
# - active_insiders (same) 
# - active_traders (same)
# - total_daily_volume (same)
# - supply_pct_7d_avg (was: insider_supply_pct_7d_avg) 
# - daily_change (was: daily_supply_change)
# - trend_indicator (was: supply_level_indicator) - now includes RISING/FALLING/STABLE
# 
# This R script automatically handles the column name mapping for backward compatibility.
# ================================================

library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(ggpubr)
library(patchwork)
library(tidyr)
library(viridis)

# --- 1. Load the Activity Data ---
setwd("C:/Users/Nitropc/Downloads")
activity_data <- fread("TROLL_insiders_activity_clean.csv")



# --- 1.5. Filter Configuration ---
# Add addresses to exclude from analysis (composable for future filtering)
exclude_addresses_insiders <- c(
  # Add specific addresses you want to filter out
  # "WALLET_ADDRESS_TO_EXCLUDE_1",
  # "WALLET_ADDRESS_TO_EXCLUDE_2",
  # Example: Known bots, contracts, or problematic addresses
  "FhRiiVdT5Q2NDb6szFyETdyZK9Ui2e84Mi9qPNhV8tTK"
)

# Apply filters
if (length(exclude_addresses_insiders) > 0) {
  cat("Filtering out", length(exclude_addresses_insiders), "addresses...\n")
  activity_data <- activity_data %>%
    filter(!address %in% exclude_addresses_insiders)
  cat("Remaining addresses after filtering:", nrow(activity_data), "\n")
}

# --- 1.6. Filter for Significant Current Holdings ---
# Remove wallets with very small current holdings to focus on relevant entities
significant_supply_threshold <- 0.001  # 0.001% of supply minimum

cat("Original dataset size:", nrow(activity_data), "\n")
activity_data <- activity_data %>%
  filter(supply_pct_numeric >= significant_supply_threshold)
cat("After filtering for significant holders (>=", significant_supply_threshold, "%):", nrow(activity_data), "\n")
cat("Supply coverage of filtered dataset:", round(sum(activity_data$supply_pct_numeric, na.rm = TRUE), 2), "%\n")

# Clean and prepare data
activity_data <- activity_data %>%
  mutate(
    # Create simplified entity categories
    entity_type = case_when(
      classification == "Individual" ~ "Individual",
      classification == "Clustered" ~ "Cluster",
      TRUE ~ classification
    ),
    # Create USD value categories
    holding_category = case_when(
      current_holding_usd >= 50000 ~ "Large (>50k)",
      current_holding_usd >= 10000 ~ "Medium (10k-50k)", 
      current_holding_usd >= 1000 ~ "Small (1k-10k)",
      TRUE ~ "Minimal (<1k)"
    ),
    # Enhanced behavior classification - no mixed/neutral category
    behavior_simple = case_when(
      # Clear accumulating patterns
      behavior_pattern %in% c("STRONG_ACCUMULATING", "ACCUMULATING") ~ "Accumulating",
      
      # Clear distributing patterns  
      behavior_pattern %in% c("STRONG_DISTRIBUTING", "DISTRIBUTING") ~ "Distributing",
      
      # Clear holding pattern
      behavior_pattern == "HOLDING" ~ "Holding",
      
      # For MIXED/NEUTRAL, use proportional analysis
      # If 30d net flow is significant relative to current holdings, classify as trading
      # Otherwise, classify as holding
      abs(net_flow_30d_usd) > (current_holding_usd * 0.25) & net_flow_30d_usd > 0 ~ "Accumulating",
      abs(net_flow_30d_usd) > (current_holding_usd * 0.25) & net_flow_30d_usd < 0 ~ "Distributing",
      
      # If they have significant selling in 90d but minor relative to holdings
      abs(net_flow_90d_usd) > (current_holding_usd * 0.15) & net_flow_90d_usd < 0 ~ "Distributing",
      abs(net_flow_90d_usd) > (current_holding_usd * 0.15) & net_flow_90d_usd > 0 ~ "Accumulating",
      
      # If they have sold significant amounts historically but are now holding
      total_sold_usd > (current_holding_usd * 0.5) & abs(net_flow_30d_usd) < (current_holding_usd * 0.1) ~ "Holding",
      
      # If net flows are very small relative to holdings (less than 10%), classify as holding
      abs(net_flow_30d_usd) < (current_holding_usd * 0.1) & 
      abs(net_flow_90d_usd) < (current_holding_usd * 0.2) ~ "Holding",
      
      # Default based on dominant flow direction over 90d
      net_flow_90d_usd > 1000 ~ "Accumulating",
      net_flow_90d_usd < -1000 ~ "Distributing",
      
      # Final fallback to holding
      TRUE ~ "Holding"
    )
  )

# --- 2. Summary Statistics ---
cat("=== DATASET OVERVIEW ===\n")
cat("Total addresses:", nrow(activity_data), "\n")
cat("Individuals:", sum(activity_data$classification == "Individual"), "\n")
cat("Clustered addresses:", sum(activity_data$classification == "Clustered"), "\n")
cat("Unique clusters:", length(unique(activity_data$entity_name[activity_data$classification == "Clustered"])), "\n")

# --- 3. Behavior Pattern Analysis by Entity Type ---
behavior_summary <- activity_data %>%
  group_by(entity_type, behavior_simple) %>%
  summarise(
    count = n(),
    total_holding_usd = sum(current_holding_usd, na.rm = TRUE),
    median_holding_usd = median(current_holding_usd, na.rm = TRUE),
    max_holding_usd = max(current_holding_usd, na.rm = TRUE),
    total_7d_flow = sum(net_flow_7d_usd, na.rm = TRUE),
    total_30d_flow = sum(net_flow_30d_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(entity_type, desc(count))

print("=== BEHAVIOR PATTERNS BY ENTITY TYPE ===")
print(behavior_summary)

# --- 4. Visualization 1: Behavior Distribution ---
p1 <- ggplot(activity_data, aes(x = entity_type, fill = behavior_simple)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_fill_viridis_d(name = "Behavior") +
  labs(
    title = "Trading Behavior: Clusters vs Individuals",
    subtitle = "Proportion of each behavior pattern",
    x = "Entity Type",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "top"
  )

print(p1)

# --- 5. Visualization 2: Flow Analysis by Entity Type ---
flow_data <- activity_data %>%
  select(entity_type, entity_name, net_flow_7d_usd, net_flow_30d_usd, net_flow_90d_usd) %>%
  pivot_longer(cols = starts_with("net_flow"), names_to = "period", values_to = "flow_usd") %>%
  mutate(
    period = case_when(
      period == "net_flow_7d_usd" ~ "7 Days",
      period == "net_flow_30d_usd" ~ "30 Days", 
      period == "net_flow_90d_usd" ~ "90 Days"
    ),
    period = factor(period, levels = c("7 Days", "30 Days", "90 Days"))
  )

p2 <- ggplot(flow_data, aes(x = period, y = flow_usd, fill = entity_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Individual" = "#E74C3C", "Cluster" = "#3498DB")) +
  labs(
    title = "Net Flow Distribution: Clusters vs Individuals",
    subtitle = "Positive = Net Buying, Negative = Net Selling",
    x = "Time Period",
    y = "Net Flow (USD)",
    fill = "Entity Type"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "top"
  )

print(p2)

# --- 6. Top Clusters Analysis ---
cluster_analysis <- activity_data %>%
  filter(classification == "Clustered") %>%
  group_by(entity_name) %>%
  summarise(
    wallet_count = n(),
    total_supply_pct = sum(supply_pct_numeric, na.rm = TRUE),
    total_holding_usd = sum(current_holding_usd, na.rm = TRUE),
    net_flow_7d = sum(net_flow_7d_usd, na.rm = TRUE),
    net_flow_30d = sum(net_flow_30d_usd, na.rm = TRUE),
    net_flow_90d = sum(net_flow_90d_usd, na.rm = TRUE),
    total_sold_usd = sum(total_sold_usd, na.rm = TRUE),
    dominant_behavior = names(sort(table(behavior_simple), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(total_supply_pct)) %>%
  head(15)

print("=== TOP 15 CLUSTERS BY SUPPLY PERCENTAGE ===")
print(cluster_analysis)

# --- 7. Visualization 3: Top Clusters Behavior ---
# Define consistent color palette for behaviors
behavior_colors <- c(
  "Accumulating" = "#27AE60",      # Green
  "Distributing" = "#E74C3C",      # Red  
  "Holding" = "#3498DB",           # Blue
  "Mixed" = "#F39C12",             # Orange
  "Unknown" = "#95A5A6"            # Gray
)

p3 <- ggplot(cluster_analysis, aes(x = reorder(entity_name, -total_supply_pct), y = total_supply_pct)) +
  geom_bar(aes(fill = dominant_behavior), stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(sprintf("%.2f%%", total_supply_pct), "\n(", wallet_count, " wallets)")),
            vjust = -0.2, size = 3, fontface = "bold") +
  scale_fill_manual(values = behavior_colors, name = "Dominant Behavior") +
  labs(
    title = "Top Clusters by Supply Holdings",
    subtitle = "Showing dominant trading behavior and wallet count",
    x = "Cluster",
    y = "Total Supply %"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p3)

# --- 7.5. Visualization 3b: Top Individual Traders Behavior ---
individual_analysis <- activity_data %>%
  filter(classification == "Individual") %>%
  arrange(desc(supply_pct_numeric)) %>%
  head(10) %>%  # Top 10 individuals
  mutate(
    display_name = paste0(substr(address, 1, 8), "..."),
    # Ensure we have behavior data
    behavior_simple = ifelse(is.na(behavior_simple), "Unknown", behavior_simple)
  )

print("=== TOP 10 INDIVIDUALS BY SUPPLY PERCENTAGE ===")
individual_summary <- individual_analysis %>%
  select(display_name, supply_pct_numeric, current_holding_usd, behavior_simple, 
         net_flow_7d_usd, net_flow_30d_usd, total_sold_usd) %>%
  mutate(across(ends_with("_usd"), ~scales::dollar(.)))
print(individual_summary)

p3b <- ggplot(individual_analysis, aes(x = reorder(display_name, -supply_pct_numeric), y = supply_pct_numeric)) +
  geom_bar(aes(fill = behavior_simple), stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f%%", supply_pct_numeric)),
            vjust = -0.2, size = 3, fontface = "bold") +
  scale_fill_manual(values = behavior_colors, name = "Trading Behavior") +
  labs(
    title = "Top Individual Traders by Supply Holdings",
    subtitle = "Showing trading behavior and supply percentage held",
    x = "Individual Address",
    y = "Supply %"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p3b)

# --- 8. Flow Heatmap for Top Clusters ---
cluster_flows <- cluster_analysis %>%
  select(entity_name, net_flow_7d, net_flow_30d, net_flow_90d) %>%
  pivot_longer(cols = starts_with("net_flow"), names_to = "period", values_to = "flow") %>%
  mutate(
    period = case_when(
      period == "net_flow_7d" ~ "7D",
      period == "net_flow_30d" ~ "30D",
      period == "net_flow_90d" ~ "90D"
    )
  )

p4 <- ggplot(cluster_flows, aes(x = period, y = reorder(entity_name, flow), fill = flow)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#27AE60",
    midpoint = 0, name = "Net Flow\n(USD)",
    labels = scales::dollar_format()
  ) +
  geom_text(aes(label = scales::dollar(flow, scale = 1e-3, suffix = "K")), 
            size = 3, fontface = "bold") +
  labs(
    title = "Cluster Trading Activity Heatmap",
    subtitle = "Green = Net Buying, Red = Net Selling",
    x = "Time Period",
    y = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.text.y = element_text(size = 10)
  )

print(p4)

# --- 8.5. Top Individual Traders Analysis and Heatmap ---
# Select top 8 most notable individuals based on current holdings + selling activity
top_individuals <- activity_data %>%
  filter(classification == "Individual") %>%
  mutate(
    # Create a "notability score" combining holdings and selling activity
    notability_score = current_holding_usd + 
                      abs(pmin(net_flow_30d_usd, 0)) + # Absolute value of negative flows only
                      abs(pmin(net_flow_90d_usd, 0)) + # Captures big sellers
                      (total_sold_usd * 0.1), # Include total selling history
    # Create display name with first 8 characters
    display_name = paste0(substr(address, 1, 8), "...")
  ) %>%
  arrange(desc(notability_score)) %>%
  head(8)

print("=== TOP 8 MOST NOTABLE INDIVIDUALS ===")
individual_summary <- top_individuals %>%
  select(display_name, current_holding_usd, net_flow_7d_usd, net_flow_30d_usd, 
         net_flow_90d_usd, total_sold_usd, behavior_simple, notability_score) %>%
  mutate(across(ends_with("_usd"), ~scales::dollar(.)))
print(individual_summary)

# Create heatmap for top individuals
individual_flows <- top_individuals %>%
  select(display_name, net_flow_7d_usd, net_flow_30d_usd, net_flow_90d_usd) %>%
  pivot_longer(cols = ends_with("_usd"), names_to = "period", values_to = "flow") %>%
  mutate(
    period = case_when(
      period == "net_flow_7d_usd" ~ "7D",
      period == "net_flow_30d_usd" ~ "30D",
      period == "net_flow_90d_usd" ~ "90D"
    )
  )

p4b <- ggplot(individual_flows, aes(x = period, y = reorder(display_name, flow), fill = flow)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E74C3C", mid = "white", high = "#27AE60",
    midpoint = 0, name = "Net Flow\n(USD)",
    labels = scales::dollar_format()
  ) +
  geom_text(aes(label = scales::dollar(flow, scale = 1e-3, suffix = "K")), 
            size = 3, fontface = "bold") +
  labs(
    title = "Top Individual Traders Activity Heatmap",
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

print(p4b)

# --- 8.5. Insider Supply Evolution Over Time ---
cat("\n=== LOADING INSIDER SUPPLY TIMELINE DATA ===\n")

# Check if timeline data exists
timeline_file <- "insider_supply_timeline.csv"
if(file.exists(timeline_file)) {
  cat("Loading insider supply timeline data...\n")
  
  supply_timeline <- read.csv(timeline_file, stringsAsFactors = FALSE) %>%
    mutate(
      trade_date = as.Date(trade_date),
      # Ensure numeric columns - updated for complete timeline query
      across(c(addresses_included, insider_supply_pct, insider_balance_millions, 
               active_insiders, active_traders, total_daily_volume, 
               supply_pct_7d_avg, daily_change), as.numeric)
    ) %>%
    # Rename columns for backward compatibility with existing R code
    rename(
      insider_supply_pct_7d_avg = supply_pct_7d_avg,
      daily_supply_change = daily_change,
      supply_level_indicator = trend_indicator
    ) %>%
    arrange(trade_date) %>%
    # IMPORTANT: Exclude the latest trade_date row (likely incomplete data)
    filter(trade_date < max(trade_date))
  
  cat("Timeline data loaded:", nrow(supply_timeline), "days of data (excluding latest incomplete day)\n")
  cat("Date range:", min(supply_timeline$trade_date), "to", max(supply_timeline$trade_date), "\n")
  cat("Latest date excluded:", as.character(max(read.csv(timeline_file, stringsAsFactors = FALSE)$trade_date)), "(potentially incomplete)\n")
  
  # NEW: Validate address coverage
  if("addresses_included" %in% names(supply_timeline)) {
    cat("Address coverage validation:\n")
    cat("Expected addresses: 322\n")
    cat("Actual addresses per day: min=", min(supply_timeline$addresses_included), 
        ", max=", max(supply_timeline$addresses_included), "\n")
    if(all(supply_timeline$addresses_included == supply_timeline$addresses_included[1])) {
      cat("✓ Consistent address coverage across all days\n")
    } else {
      cat("⚠ WARNING: Inconsistent address coverage detected!\n")
    }
  }
  
  # VALIDATION: Check if current supply from timeline matches activity_data
  activity_total_supply <- sum(activity_data$supply_pct_numeric, na.rm = TRUE)
  timeline_current_supply <- tail(supply_timeline$insider_supply_pct, 1)
  
  # Show what columns we have for debugging
  cat("Timeline columns available:", paste(names(supply_timeline), collapse = ", "), "\n")
  
  # --- LOAD PRICE HISTORY DATA ---
  cat("\n=== LOADING PRICE HISTORY DATA ===\n")
  price_file <- "token_price_history.csv"
  
  if(file.exists(price_file)) {
    cat("Loading price history data...\n")
    
    price_history <- read.csv(price_file, stringsAsFactors = FALSE) %>%
      mutate(
        trade_date = as.Date(trade_date),
        across(c(price_usd, price_low, price_high, market_cap_millions, market_cap_low_millions,
                 market_cap_high_millions, daily_volume_usd, daily_trades, market_cap_change_pct,
                 price_change_pct, market_cap_7d_avg_millions, market_cap_30d_avg_millions), as.numeric)
      ) %>%
      arrange(trade_date) %>%
      # Also exclude latest date to match supply timeline
      filter(trade_date < max(trade_date))
    
    cat("Price data loaded:", nrow(price_history), "days of data\n")
    cat("Price date range:", min(price_history$trade_date), "to", max(price_history$trade_date), "\n")
    
    # Join supply timeline with price data
    supply_price_combined <- supply_timeline %>%
      left_join(price_history, by = "trade_date")
    
    # Validate join
    missing_price_days <- sum(is.na(supply_price_combined$price_usd))
    cat("Days with missing price data:", missing_price_days, "/", nrow(supply_price_combined), "\n")
    
    if(missing_price_days == 0) {
      cat("✓ Perfect price-supply data alignment\n")
    } else if(missing_price_days < nrow(supply_price_combined) * 0.1) {
      cat("⚠ Minor price data gaps - analysis will proceed with available data\n")
    } else {
      cat("⚠ WARNING: Significant price data missing!\n")
    }
    
  } else {
    cat("Price history file not found. Run token_price_history.sql first.\n")
    cat("Expected file:", price_file, "\n")
    supply_price_combined <- supply_timeline  # Use supply data only
  }
  
  cat("\n=== SUPPLY VALIDATION ===\n")
  cat("Activity data total supply:", round(activity_total_supply, 2), "%\n")
  cat("Timeline current supply:", round(timeline_current_supply, 2), "%\n")
  cat("Difference:", round(abs(activity_total_supply - timeline_current_supply), 2), "percentage points\n")
  
  if(abs(activity_total_supply - timeline_current_supply) > 1) {
    cat("WARNING: Large discrepancy detected! Check SQL query logic.\n")
  }
  
  # Current vs Peak/Bottom analysis
  current_supply <- tail(supply_timeline$insider_supply_pct, 1)
  peak_supply <- max(supply_timeline$insider_supply_pct, na.rm = TRUE)
  bottom_supply <- min(supply_timeline$insider_supply_pct, na.rm = TRUE)
  peak_date <- supply_timeline$trade_date[which.max(supply_timeline$insider_supply_pct)]
  bottom_date <- supply_timeline$trade_date[which.min(supply_timeline$insider_supply_pct)]
  
  cat("\n=== INSIDER SUPPLY EVOLUTION ANALYSIS ===\n")
  cat("Analysis period:", as.character(min(supply_timeline$trade_date)), "to", as.character(max(supply_timeline$trade_date)), 
      "(", nrow(supply_timeline), "days total)\n")
  cat("Current insider supply:", round(current_supply, 2), "%\n")
  cat("Peak insider supply:", round(peak_supply, 2), "% on", as.character(peak_date), "\n")
  cat("Bottom insider supply:", round(bottom_supply, 2), "% on", as.character(bottom_date), "\n")
  cat("Supply range (peak - bottom):", round(peak_supply - bottom_supply, 2), "percentage points\n")
  
  # Create main supply evolution chart with price overlay (if available)
  if(exists("price_history") && "price_usd" %in% names(supply_price_combined)) {
    
    # Combined supply and price chart
    p5 <- ggplot(supply_price_combined, aes(x = trade_date)) +
      # Price line (secondary axis)
      geom_line(aes(y = price_usd * max(insider_supply_pct, na.rm = TRUE) / max(price_usd, na.rm = TRUE)), 
                color = "#3498DB", size = 1, alpha = 0.8) +
      
      # Supply lines (primary axis)
      geom_line(aes(y = insider_supply_pct), color = "#E74C3C", alpha = 0.3, size = 0.5) +
      geom_line(aes(y = insider_supply_pct_7d_avg), color = "#E74C3C", size = 1.2) +
      
      # Peak and bottom markers
      geom_point(data = subset(supply_price_combined, supply_level_indicator == "PEAK"), 
                 aes(y = insider_supply_pct), color = "#8B0000", size = 3, shape = 17) +
      geom_point(data = subset(supply_price_combined, supply_level_indicator == "BOTTOM"), 
                 aes(y = insider_supply_pct), color = "#006400", size = 3, shape = 17) +
      
      # Additional trend markers
      geom_point(data = subset(supply_price_combined, supply_level_indicator == "RISING"), 
                 aes(y = insider_supply_pct), color = "#27AE60", size = 1, alpha = 0.6) +
      geom_point(data = subset(supply_price_combined, supply_level_indicator == "FALLING"), 
                 aes(y = insider_supply_pct), color = "#E74C3C", size = 1, alpha = 0.6) +
      
      # Current level horizontal line
      geom_hline(yintercept = current_supply, linetype = "dashed", color = "#34495E", alpha = 0.7) +
      
      # Dual y-axis setup
      scale_y_continuous(
        name = "Insider Supply Percentage",
        labels = percent_format(scale = 1),
        sec.axis = sec_axis(
          ~ . * max(supply_price_combined$market_cap_millions, na.rm = TRUE) / max(supply_price_combined$insider_supply_pct, na.rm = TRUE),
          name = "Market Cap (Millions USD)",
          labels = function(x) paste0("$", round(x, 1), "M")
        )
      ) +
      
      scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
      
      labs(
        title = "Insider Supply Evolution vs Market Cap",
        subtitle = paste("Supply: Current:", round(current_supply, 2), "% | Peak:", round(peak_supply, 2), 
                        "% | Market Cap: Current: $", round(tail(supply_price_combined$market_cap_millions, 1), 1), "M"),
        x = "Date",
        caption = "Red line = Insider supply (7d avg), Blue line = Market cap, Triangles = supply peaks/bottoms"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        axis.title.y.right = element_text(color = "#3498DB"),
        axis.title.y.left = element_text(color = "#E74C3C"),
        legend.position = "none"
      )
    
  } else {
    # Fallback to supply-only chart if no price data
    p5 <- ggplot(supply_timeline, aes(x = trade_date)) +
      # Main supply line with 7-day average (supply-only fallback)
      geom_line(aes(y = insider_supply_pct), color = "#E74C3C", alpha = 0.3, size = 0.5) +
      geom_line(aes(y = insider_supply_pct_7d_avg), color = "#E74C3C", size = 1.2) +
      
      # Peak and bottom markers
      geom_point(data = subset(supply_timeline, supply_level_indicator == "PEAK"), 
                 aes(y = insider_supply_pct), color = "#8B0000", size = 3, shape = 17) +
      geom_point(data = subset(supply_timeline, supply_level_indicator == "BOTTOM"), 
                 aes(y = insider_supply_pct), color = "#006400", size = 3, shape = 17) +
      
      # Additional trend markers
      geom_point(data = subset(supply_timeline, supply_level_indicator == "RISING"), 
                 aes(y = insider_supply_pct), color = "#27AE60", size = 1, alpha = 0.6) +
      geom_point(data = subset(supply_timeline, supply_level_indicator == "FALLING"), 
                 aes(y = insider_supply_pct), color = "#E74C3C", size = 1, alpha = 0.6) +
      
      # Current level horizontal line
      geom_hline(yintercept = current_supply, linetype = "dashed", color = "#34495E", alpha = 0.7) +
      
      scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
      scale_y_continuous(labels = percent_format(scale = 1), 
                        sec.axis = sec_axis(~., name = "Supply %", labels = percent_format(scale = 1))) +
      
      labs(
        title = "Insider Supply Evolution Over Time",
        subtitle = paste("Current:", round(current_supply, 2), "% | Peak:", round(peak_supply, 2), 
                        "% | Range:", round(peak_supply - bottom_supply, 2), "pp"),
        x = "Date",
        y = "Insider Supply Percentage",
        caption = "Red line = 7-day average, Light red = daily values, Triangles = peaks/bottoms"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  }
  
  # Create enhanced volume chart with price correlation (if available)
  if(exists("price_history") && "price_usd" %in% names(supply_price_combined)) {
    p5b <- ggplot(supply_price_combined, aes(x = trade_date)) +
      # Volume bars colored by price change
      geom_col(aes(y = total_daily_volume, fill = price_change_pct > 0), alpha = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"),
                       labels = c("Price Up", "Price Down")) +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Daily Trading Volume by Price Direction",
        x = "Date",
        y = "Volume (USD)",
        fill = "Price Change"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
  } else {
    p5b <- ggplot(supply_timeline, aes(x = trade_date)) +
      geom_col(aes(y = total_daily_volume), fill = "#3498DB", alpha = 0.6) +
      scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Daily Trading Volume",
        x = "Date",
        y = "Volume (USD)"
      ) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Create price correlation chart (if available) or supply direction chart
  if(exists("price_history") && "price_usd" %in% names(supply_price_combined)) {
    
    # Price vs Supply Change Correlation
    p5c <- ggplot(supply_price_combined, aes(x = trade_date)) +
      geom_line(aes(y = price_change_pct), color = "#3498DB", size = 1) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.3) +
      
      # Highlight significant price moves
      geom_point(data = subset(supply_price_combined, abs(price_change_pct) > 20), 
                 aes(y = price_change_pct), color = "#E74C3C", size = 2) +
      
      scale_x_date(date_labels = "%m/%d", date_breaks = "1 week") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(
        title = "Daily Price Changes",
        subtitle = "Red dots = major price moves (>20%)",
        x = "Date", 
        y = "Price Change (%)"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
  } else {
    # Fallback to supply direction chart
    p5c <- ggplot(supply_timeline, aes(x = trade_date, y = total_daily_volume)) +
      geom_col(aes(fill = daily_supply_change < 0), alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "#27AE60", "TRUE" = "#E74C3C"), 
                       labels = c("Supply Increased", "Supply Decreased")) +
      scale_x_date(date_labels = "%m/%d", date_breaks = "1 week") +
      scale_y_continuous(labels = dollar_format()) +
      labs(
        title = "Daily Trading Volume with Supply Direction",
        x = "Date", 
        y = "Trading Volume (USD)",
        fill = "Supply Change"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
  }
  
  cat("\n=== INSIDER SUPPLY TIMELINE VISUALIZATIONS ===\n")
  print(p5)
  
  # Combined volume and flow analysis
  cat("\n=== TRADING ACTIVITY ANALYSIS ===\n")
  combined_volume_flow <- (p5b / p5c)
  print(combined_volume_flow)
  
  # Supply and Activity Summary Table  
  cat("\n=== SUPPLY & ACTIVITY SUMMARY TABLE ===\n")
  # Use available columns from the complete timeline query
  available_cols <- intersect(c("trade_date", "insider_supply_pct", "daily_supply_change", 
                                "total_daily_volume", "active_traders", "supply_level_indicator"), 
                             names(supply_timeline))
  
  supply_flow_summary <- supply_timeline %>%
    select(all_of(available_cols)) %>%
    arrange(desc(trade_date)) %>%  # Most recent first
    mutate(
      # Add activity indicator based on volume and supply change
      activity_type = case_when(
        total_daily_volume > 100000 & daily_supply_change < -0.1 ~ "Heavy Distribution",
        total_daily_volume > 50000 & daily_supply_change < 0 ~ "Distribution",
        total_daily_volume > 50000 & daily_supply_change > 0 ~ "Accumulation", 
        total_daily_volume > 10000 ~ "Moderate Activity",
        TRUE ~ "Low Activity"
      ),
      # Format values for readability (handle missing columns)
      supply_pct_formatted = paste0(round(insider_supply_pct, 2), "%"),
      supply_change_formatted = paste0(ifelse(daily_supply_change >= 0, "+", ""), 
                                       round(daily_supply_change, 4), "pp"),
      volume_formatted = scales::dollar(round(total_daily_volume, 0)),
      traders_active = paste0(active_traders, " traders")
    ) %>%
    head(15)  # Show last 15 days
  
  # Print summary with available columns
  print(supply_flow_summary %>% 
        select(trade_date, supply_pct_formatted, supply_change_formatted, 
               volume_formatted, traders_active, activity_type, supply_level_indicator) %>%
        rename(
          "Date" = trade_date,
          "Supply %" = supply_pct_formatted, 
          "Daily Change" = supply_change_formatted,
          "Volume" = volume_formatted,
          "Active Traders" = traders_active,
          "Activity Type" = activity_type,
          "Trend" = supply_level_indicator
        ))
  
  # Recent trend analysis (last 30 days)
  recent_data <- supply_timeline %>%
    filter(trade_date >= max(trade_date) - 30) %>%
    arrange(trade_date)
  
  if(nrow(recent_data) >= 7) {
    recent_trend <- lm(insider_supply_pct ~ as.numeric(trade_date), data = recent_data)
    trend_slope <- coefficients(recent_trend)[2]
    
    cat("\n=== RECENT TREND ANALYSIS (Last 30 Days) ===\n")
    cat("Supply trend:", ifelse(trend_slope > 0, "INCREASING", "DECREASING"), "\n")
    cat("Daily change rate:", round(trend_slope, 4), "percentage points per day\n")
    cat("Estimated weekly change:", round(trend_slope * 7, 2), "percentage points\n")
    
    # Calculate volatility
    recent_volatility <- sd(recent_data$daily_supply_change, na.rm = TRUE)
    cat("Recent volatility (std dev):", round(recent_volatility, 3), "percentage points\n")
    
    # Additional diagnostics about data availability
    cat("\n=== TIMELINE DATA QUALITY CHECK ===\n")
    cat("Total days of data:", nrow(supply_timeline), "\n")
    cat("Missing insider_supply_pct values:", sum(is.na(supply_timeline$insider_supply_pct)), "\n")
    cat("Missing daily_supply_change values:", sum(is.na(supply_timeline$daily_supply_change)), "\n")
    cat("Missing addresses_included values:", sum(is.na(supply_timeline$addresses_included)), "\n")
    cat("Address coverage consistency:", ifelse(length(unique(supply_timeline$addresses_included)) == 1, "✓ GOOD", "⚠ INCONSISTENT"), "\n")
  }
  
  cat("\n=== SUPPLY TIMELINE ANALYSIS COMPLETE ===\n")
  cat("Analysis completed without CSV export.\n")
  
} else {
  cat("Insider supply timeline data not found!\n")
  cat("Please run the insider_supply_timeline.sql query in Dune Analytics first.\n")
  cat("Expected file:", timeline_file, "\n")
  cat("\nThe complete timeline query outputs these columns:\n")
  cat("addresses_included, insider_supply_pct, insider_balance_millions, active_insiders,\n")
  cat("active_traders, total_daily_volume, supply_pct_7d_avg, daily_change, trend_indicator\n")
  cat("\nThe market cap query outputs: market_cap_millions, market_cap_change_pct, etc.\n")
}

# --- 9. Individual vs Cluster Comparison ---
entity_comparison <- activity_data %>%
  group_by(entity_type) %>%
  summarise(
    count = n(),
    avg_supply_pct = mean(supply_pct_numeric, na.rm = TRUE),
    median_supply_pct = median(supply_pct_numeric, na.rm = TRUE),
    total_supply_pct = sum(supply_pct_numeric, na.rm = TRUE),
    avg_holding_usd = mean(current_holding_usd, na.rm = TRUE),
    median_holding_usd = median(current_holding_usd, na.rm = TRUE),
    net_buyers = sum(net_flow_30d_usd > 1000, na.rm = TRUE),
    net_sellers = sum(net_flow_30d_usd < -1000, na.rm = TRUE),
    holders = sum(abs(net_flow_30d_usd) <= 1000, na.rm = TRUE),
    .groups = "drop"
  )

print("=== INDIVIDUALS VS CLUSTERS COMPARISON ===")
print(entity_comparison)



# --- 11. Export Summary Tables ---
write_csv(behavior_summary, "behavior_analysis_summary.csv")
write_csv(cluster_analysis, "top_clusters_analysis.csv")
write_csv(entity_comparison, "individuals_vs_clusters_comparison.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Summary files exported:\n")
cat("- behavior_analysis_summary.csv\n")
cat("- top_clusters_analysis.csv\n") 
cat("- individuals_vs_clusters_comparison.csv\n")