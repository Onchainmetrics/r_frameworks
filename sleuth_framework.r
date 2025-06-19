# --- 1. Load Libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(reshape2)

# --- 2. Load Data ---
setwd("C:/Users/Nitropc/Downloads")
transfers <- fread("house_all_transfers_clean.csv")
transfers <- transfers[from_owner != "" & to_owner != ""]
transfers <- unique(transfers, by = c("from_owner", "to_owner", "amount", "block_time"))
transfers$from_owner <- trimws(transfers$from_owner)
transfers$to_owner <- trimws(transfers$to_owner)
transfers$block_time <- as.POSIXct(transfers$block_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

early_buyers_df <- fread("house_buyers_7d.csv")
# Filter for unique_recipients <= 75
early_buyers_filtered <- early_buyers_df[unique_recipients <= 75]
cutoff_99 <- quantile(early_buyers_filtered$total_buy_transactions, 0.99, na.rm = TRUE)
early_buyers_final <- early_buyers_filtered[total_buy_transactions <= cutoff_99]
early_buyers <- trimws(early_buyers_final$address)

#current balances
current_balances <- fread("house_all_balances.csv")
current_balances$address <- trimws(current_balances$address)



# This will split every string by comma and flatten into a single vector
parsed_lists <- lapply(early_buyers_final$recipient_addresses, function(x) {
  if (is.na(x) || x == "") return(character(0))
  x_clean <- gsub("\\[|\\]|\"|'", "", x)
  # Split on any whitespace (space, tab, newline)
  addrs <- unlist(strsplit(x_clean, "\\s+"))
  addrs <- trimws(addrs)
  addrs[addrs != "" & addrs != "NA"]
})
all_recipients <- unique(unlist(parsed_lists))

##build descendant set
#exclude multisig
# Add all known special addresses to this vector
exclude_addresses <- c("DodwnsRtPbkzJHC4AcoXdmT92GUUsRDQ6FxJfM4UDcem")
# Descendant tracing
descendant_set <- early_buyers
new_descendants <- early_buyers

for (i in 1:100) {
  # Find next descendants, but exclude special addresses
  next_descendants <- unique(transfers[from_owner %in% new_descendants, to_owner])
  next_descendants <- setdiff(next_descendants, c(descendant_set, exclude_addresses))
  if (length(next_descendants) == 0) break
  descendant_set <- c(descendant_set, next_descendants)
  new_descendants <- next_descendants
}
descendant_set <- unique(descendant_set)

##classify current holders
current_balances <- current_balances %>%
  mutate(
    group = case_when(
      address %in% early_buyers ~ "Early Buyer",
      address %in% descendant_set & !(address %in% early_buyers) ~ "Descendant",
      TRUE ~ "Late Buyer"
    )
  )

summary_table <- current_balances %>%
  group_by(group) %>%
  summarise(
    wallets = n(),
    total_supply = sum(current_balance),
    .groups = "drop"
  ) %>%
  mutate(
    supply_pct = total_supply / sum(total_supply) * 100,
    wallet_pct = wallets / sum(wallets) * 100
  )

print(summary_table)


##compare current holdings of early + descendants with original early
# Get current holdings for Early Buyer and Descendant from summary_table
current_early_and_descendants <- summary_table %>%
  filter(group %in% c("Early Buyer", "Descendant")) %>%
  summarise(total = sum(total_supply)) %>%
  pull(total)

retention_pct <- current_early_and_descendants / total_net_buys_early_buyers * 100
cat(sprintf(
  "Early buyers + descendants currently hold %.2f%% of their original net buys (%.2fM of %.2fM tokens).\n",
  retention_pct,
  current_early_and_descendants / 1e6,
  total_net_buys_early_buyers / 1e6
))
#comparison table
total_supply <- 1e9  # 1,000,000,000 tokens
# Net buys of early buyers (as % of total supply)
net_buys_pct <- total_net_buys_early_buyers / total_supply * 100

# Current holding of early buyers + descendants (as % of total supply)
current_early_and_descendants_pct <- current_early_and_descendants / total_supply * 100

comparison_table_pct <- data.frame(
  metric = c(
    "Net buys of early buyers (% of total supply)",
    "Current holding (early + descendants, % of total supply)",
    "Retention (% of original net buys)"
  ),
  value = c(
    sprintf("%.2f%%", net_buys_pct),
    sprintf("%.2f%%", current_early_and_descendants_pct),
    sprintf("%.2f%%", retention_pct)
  )
)

print(comparison_table_pct, row.names = FALSE)

###INSIDER SECTION
##very early buyers
insiders_df <- fread("house_buyers_cheap.csv")
#remove the jup apepro bot wallet
# Exclude the bot wallet from your insiders
insiders_final <- insiders_df[unique_recipients <= 7]
insider_addresses <- trimws(insiders_final$address)

descendant_set_insiders <- insider_addresses
new_descendants_insiders <- insider_addresses

for (i in 1:100) {
  next_descendants <- unique(transfers[from_owner %in% new_descendants_insiders, to_owner])
  next_descendants <- setdiff(next_descendants, descendant_set_insiders)
  if (length(next_descendants) == 0) break
  descendant_set_insiders <- c(descendant_set_insiders, next_descendants)
  new_descendants_insiders <- next_descendants
}
descendant_set_insiders <- unique(descendant_set_insiders)

current_balances <- current_balances %>%
  mutate(
    group_insiders = case_when(
      address %in% insider_addresses ~ "Insider",
      address %in% descendant_set_insiders & !(address %in% insider_addresses) ~ "Insider Descendant",
      TRUE ~ "Other"
    )
  )

summary_table_insiders <- current_balances %>%
  group_by(group_insiders) %>%
  summarise(
    wallets = n(),
    total_supply = sum(current_balance),
    .groups = "drop"
  ) %>%
  mutate(
    supply_pct = total_supply / 1e9 * 100,
    wallet_pct = wallets / sum(wallets) * 100
  )

print(summary_table_insiders)

#standard naming
summary_table_insiders <- summary_table_insiders %>%
  mutate(
    group = case_when(
      group_insiders == "Insider" ~ "Early Buyer",
      group_insiders == "Insider Descendant" ~ "Descendant",
      group_insiders == "Other" ~ "Late Buyer"
    )
  ) %>%
  select(group, wallets, total_supply, supply_pct, wallet_pct)

#print
print(summary_table_insiders)

##compare the 2 approaches:
# Add a cohort label to each table
summary_table_8d <- summary_table
summary_table_8d <- summary_table_8d %>%
  mutate(cohort = "8-Day Early Buyers")

summary_table_insiders <- summary_table_insiders %>%
  mutate(cohort = "Insiders(?)")

# Combine
comparison_df <- bind_rows(summary_table_8d, summary_table_insiders)

library(ggplot2)
library(tidyr)
library(patchwork)
library(ggpubr)
# Prepare data for stacked bar
plot_stacked <- comparison_df %>%
  select(cohort, group, supply_pct)

# Make sure group order is: Early Buyer, Descendant, Late Buyer
plot_stacked$group <- factor(plot_stacked$group, levels = c("Early Buyer", "Descendant", "Late Buyer"))

# Elegant color palette
palette <- c("Early Buyer" = "#FFD700", "Descendant" = "#1A73E8", "Late Buyer" = "#6C757D")
# Prepare a summary table for display
summary_for_table <- comparison_df %>%
  select(cohort, group, supply_pct) %>%
  tidyr::pivot_wider(names_from = cohort, values_from = supply_pct) %>%
  mutate(across(where(is.numeric), ~sprintf("%.2f%%", .)))

# Prepare the table for display
summary_for_table <- comparison_df %>%
  select(cohort, group, supply_pct) %>%
  tidyr::pivot_wider(names_from = cohort, values_from = supply_pct) %>%
  mutate(across(where(is.numeric), ~sprintf("%.2f%%", .)))

# Add cohort labels
colnames(summary_for_table) <- c(
  "Group",
  "8-Day Early Buyers (Cohort A)",
  "Insiders(?) (Cohort B)"
)

print(summary_for_table)
# Plot
p <- ggplot(plot_stacked, aes(x = cohort, y = supply_pct, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.5) +
  scale_fill_manual(values = palette) +
  labs(
    title = "Token Supply Distribution by group",
    subtitle = "Cohort A --> Early Buyer: Anyone who bought in the first 8 days.\nCohort B -->Insiders (?): Anyone who bought before Lexapro.\nA Descendant is a wallet linked to Early Buyer/Insider\nSee table below for the percentage of supply each group currently holds.",
    x = NULL, y = "Percent of Total Supply", fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Prepare the table

table_plot <- ggtexttable(summary_for_table, rows = NULL, theme = ttheme("minimal"))

# Now you can use patchwork

combined <- p / table_plot + plot_layout(heights = c(3, 1))
print(combined)
