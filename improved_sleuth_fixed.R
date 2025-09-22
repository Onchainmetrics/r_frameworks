# --- 
title: "improved_sleuth_fixed"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# ===============================================================================
# FILTERING STRATEGY EXPLANATION
# ===============================================================================
# 
# IMPORTANT: This framework uses different filtering strategies for different purposes:
#
# 1. CSV EXPORTS: NO FILTERING - All addresses are included regardless of current holdings
#    - Ensures addresses that may have sold large amounts recently are retained
#    - Critical for accurate supply evolution timeline analysis in Dune queries
#    - Files: insider_wallets_comprehensive.csv, late_buyer_clusters_comprehensive.csv,
#             comprehensive_three_tier_analysis.csv
#
# 2. VISUALIZATIONS & SUMMARY TABLES: FILTERED - Only significant holders (>= 0.001% supply)
#    - Prevents charts from being cluttered with empty/tiny wallets
#    - Used for network graphs, bar charts, and printed summary tables
#    - Does not affect the exported CSV data
#
# This approach solves the critical issue where important addresses were being excluded
# from supply evolution calculations simply because they had small current holdings.
# ===============================================================================

# --- 1. Load Libraries ---
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(reshape2)
library(ggpubr)
library(patchwork)
library(tidyr)
library(igraph)
library(ggraph)
# Ensure ggfx is available for blur effects
if (!requireNamespace("ggfx", quietly = TRUE)) {
  install.packages("ggfx", repos = "https://cloud.r-project.org")
}
library(ggfx)
# --- 2. Load Data ---
setwd("C:/Users/Nitropc/Downloads")


early_buyers_df <- fread("USELESS_insiders.csv")
early_buyers <- trimws(early_buyers_df$address)
early_buyers_final <- early_buyers_df

insiders_df <- fread("USELESS_insiders.csv")
insider_addresses <- trimws(insiders_df$address)
insider_addresses <- insider_addresses[insider_addresses %in% early_buyers]
current_balances <- fread("USELESS_all_balances.csv")
current_balances$address <- trimws(current_balances$address)

# --- 2.5. Total Supply Configuration ---
# Use available supply from the dataset (accounts for burns, lost tokens, etc.)
total_supply <- sum(current_balances$current_balance)
cat("Using available supply from dataset:", format(total_supply, scientific = FALSE), "tokens\n")
cat("This accounts for burned tokens and inaccessible wallets\n\n")

# --- 2. Exclude AMMs, contracts, etc. ---
exclude_addresses <- c(
  "BAX9M9a5FVy5cNiewwnuwkVDzhSg9psZnb4fJ9r677tN",
  "67C4rdUriP9EFbUo7CeoiFhM52Jgu9LZpe37Jk2k1tHZ",
  "6TYDxGmVxkBPBmEfnmLXx6jVff9LknsjRHqdTjVyZmG8",
  "HLnpSz9h2S4hiLQ43rnSD9XkcUThA7B8hQMKmDaiTLcC",
  "36m3V3vggs2peoZhn9xz2XoJv6YNX1pFKfag9aRZDnqN",
  "6DemDwdCaLuuHdBfbojEwxNScQGEYY6p2Ux4TEkPBfyx",
  "8FM9Cd28PNFaHTJiEQE96HCgfyQZ2E3kk3rEvVtenZE",
  "DDFfKazbGVgXsd4FvAzYiEftvmBMThG7ZPorQofE4bEb",
  "6qs6RBvKxaw1ncWrXyiZt4RCju8w2RWe3vY1WFVK5wuH",
  "GGztQqQ6pCPaJQnNpXBgELr5cs3WwDakRbh1iEMzjgSJ",
  "4xDsmeTWPNjgSVSS1VTfzFq3iHZhp77ffPkAmkZkdu71",
  "6U91aKa8pmMxkJwBCfPTmUEfZi6dHe7DcFq2ALvB2tbB",
  "BQ72nSv9f3PRyRKCBnHLVrerrv37CYTHm5VSGQDV",
  "2MFoS3MPtvyQ4Wh4M9pdfPjz6UhVoNbFbGJAskCPCj3h",
  "9nnLbotNTcUhvbrsA6Mdkx45Sm82G35zo28AqUvjExn8",
  "6LXutJvKUw8Q5ue2gCgKHQdAN4suWW8awzFVC6XCguFx",
  "CapuXNQoDviLvU1PxFiVRNzAgsou9kETXNJm2SXZyaKuJraVRtf",
  "7HkzG4LYyCJSrD3gopPQv3VVzQQKbHBZcm9fbjj5fuaH",
  "8psNvWTrdNTiVRNzAgsou9kETXNJm2SXZyaKuJraVRtf",
  "ZG98FUCjb8mJ824Gbs6RsgVmr1FhXb2oNiJHa2dwmPd",
  "FE9qe5NodLxD6aHwfAtNiY7cCbCG4znHVFGDnSSRpnS8",
  "HV1KXxWFaSeriyFvXyx48FqG9BoFbfinB8njCJonqP7K",
  "7dGrdJRYtsNR8UYxZ3TnifXGjGc9eRYLq9sELwYpuuUu",
  "AorF9MEjaEhKHnSH4P972L3PBpbC6q39k3C4oJkJAE5v",
  "AJnaak1zJHhsnYq3WNzGJQGphBUB7jPW99XRRJJryDex5",
  "BgQuQf1xNoiHsLYJr3dUWLf7DEe4ANvbcW5fty7oSRtJ",
  "j1oxqtEHFn7rUkdABJLmtVtz5fFmHFs4tCG3fWJnkHX",
  "GpMZbSM2GgvTKHJirzeGfMFoaZ8UR2X7F4v8vHTvxFbL",
  "2QfBNK2WDwSLoUQRb1zAnp3KM12N9hQ8q6ApwUMnWW2T",
  "8ztFxjFPfVUtEf4SLSapcFj8GW2dxyUA9no2bLPq7H7V",
  "BBXyTX5UfbASibLRo3iaptuwF5846njxm7M4xFQTQz3d",
  "C7hyRzjt3b5n6qDQW9VzpGQdTH49QW7BJKMaZefZqBFa",
  "5asKWJfxCYEGyC8Yocyi9KGVQyhi8rmTbzP72pgajUnF",
  "5YET3YapxD6to6rqPqTWB3R9pSbURy6yduuUtoZkzoPX",
  "CZiW56PZLNetanRu9kfCvxL2qS6KvKENWAYSZTaVb9vV",
  "HsQGWEh3ib6w59rBh5n1jXmi8VXFBqKEjxozL6PGfcgb",
  "8pY1AukbuPgUE3EetyLa59rFLMimJGT94ZzbMEZcQF4w",
  "3s31FNGZCiNsavo5wWEd32mmMUg5EJ2TZJ5vyKhyYgmX",
  "ER9qRhHSysuY1yk9GjogNyomFGebf7Zf2bEp3k9hVqb4",
  "GTvLGt46BqXi1v6cvSivXEybPAKBEoSGnLKDLZC8Q6SZ",
  "A7FMMgue4aZmPLLoutVtbC7gJcyqkHybUieiaDg9aaVE",
  "WLHv2UAZm6z4KyaaELi5pjdbJh6RESMva1Rnn8pJVVh",
  "Fvh8z34VTK9hyvbkbaBQ8hmrzEAvfYjy4ev4oU6WA9Jn",
  "932fX4SwBirUHzUzjHkE8AmXibxJiJ9xCBmwRsZYWEdd",
  "9Ux4vtd8juEH4NMF4ae3tXYpKBdeCUUipX8z3EficKme",
  "HFqp6ErWHY6Uzhj8rFyjYuDya2mXUpYEk8VW75K9PSiY",
  "DzqaoAX1tdzoSTs1mn1ytKBwTptACYEs6pcRdqiJYNxu",
  "2vW5UPVyEW54MNYDRRnrm5LRiZRpZPY5QSBT3r8UxcBf",
  "GP8StUXNYSZjPikyRsvkTbvRV1GBxMErb59cpeCJnDf1",
  "3LoAYHuSd7Gh8d7RTFnhvYtiTiefdZ5ByamU42vkzd76",
  "3CgvbiM3op4vjrrjH2zcrQUwsqh5veNVRjFCB9N6sRoD",
  "69yhtoJR4JYPPABZcSNkzuqbaFbwHsCkja1sP1Q2aVT5",
  "Edq4M4PDPmBwonMU5SKWyZZ4aZbisYb7cX1TuQiggEiQ",
  "5byS2sGSupT8Sb4z9VQNzwHCQm12XMcYrnM1oNeqAYgt",
  "38K48AddR83Bdbm94H2pASyX3vQCh4QX7NoGRP8ZmWp4"
  )
current_balances <- current_balances %>%
  filter(!address %in% exclude_addresses)

# --- 3. Load Transfers ---
transfers <- fread("USELESS_all_transfers.csv")
transfers <- transfers[from_owner != "" & to_owner != ""]
transfers <- unique(transfers, by = c("from_owner", "to_owner", "amount", "block_time"))
transfers$from_owner <- trimws(transfers$from_owner)
transfers$to_owner <- trimws(transfers$to_owner)

# --- 3.5. Load Exchange Wallets and Filter from Transfers ---
# Load exchange wallet information early to prevent descendant contamination
exchange_wallets_early <- tryCatch({
  read_csv("CEX_wallets.csv", show_col_types = FALSE)
}, error = function(e) {
  cat("WARNING: CEX_wallets.csv not found. Descendant analysis may include exchange contamination.\n")
  cat("To prevent this, place CEX_wallets.csv in the working directory.\n")
  data.frame(address = character(0), cex_name = character(0), distinct_name = character(0))
})

exchange_addresses_early <- exchange_wallets_early$address
cat("Filtering", length(exchange_addresses_early), "exchange addresses from transfer analysis\n")

# Filter exchange wallets from transfers to prevent descendant contamination
if (length(exchange_addresses_early) > 0) {
  transfers_original_count <- nrow(transfers)
  transfers <- transfers %>%
    filter(!from_owner %in% exchange_addresses_early & !to_owner %in% exchange_addresses_early)
  
  transfers_filtered_count <- nrow(transfers)
  cat("Removed", transfers_original_count - transfers_filtered_count, "transfers involving exchanges\n")
  cat("Remaining transfers for descendant analysis:", transfers_filtered_count, "\n")
} else {
  cat("No exchange addresses loaded - descendant analysis will include all transfers\n")
}
cat("This prevents exchange wallets from corrupting insider descendant networks\n\n")

# --- 4. Descendant Tracing Function ---
find_descendants <- function(seed_addresses, transfers, exclude_addresses) {
  descendant_set <- seed_addresses
  new_descendants <- seed_addresses
  for (i in 1:100) {
    next_descendants <- unique(transfers[from_owner %in% new_descendants, to_owner])
    next_descendants <- setdiff(next_descendants, c(descendant_set, exclude_addresses))
    if (length(next_descendants) == 0) break
    descendant_set <- c(descendant_set, next_descendants)
    new_descendants <- next_descendants
  }
  unique(descendant_set)
}

# --- 5. Find Descendants for Each Cohort ---
descendant_set_early <- find_descendants(early_buyers, transfers, exclude_addresses)
descendant_set_insider <- find_descendants(insider_addresses, transfers, exclude_addresses)

# --- 6. Assign Groups for Each Cohort ---
current_balances_early <- current_balances %>%
  mutate(
    group = case_when(
      address %in% early_buyers ~ "Early Buyer",
      address %in% descendant_set_early & !(address %in% early_buyers) ~ "Descendant",
      TRUE ~ "Late Buyer"
    )
  )

current_balances_insider <- current_balances %>%
  mutate(
    group = case_when(
      address %in% insider_addresses ~ "Insider",
      address %in% descendant_set_insider & !(address %in% insider_addresses) ~ "Insider Descendant",
      TRUE ~ "Late Buyer"
    )
  )

# --- 7. Build Summary Tables ---
summary_table_early <- current_balances_early %>%
  group_by(group) %>%
  summarise(
    wallets = n(),
    group_balance = sum(current_balance),  # Renamed to avoid confusion
    .groups = "drop"
  ) %>%
  mutate(
    supply_pct = group_balance / total_supply * 100,  # Use the global total_supply variable
    wallet_pct = wallets / sum(wallets) * 100
  ) %>%
  select(group, wallets, total_supply = group_balance, supply_pct, wallet_pct)  # Rename back for compatibility

summary_table_insider <- current_balances_insider %>%
  group_by(group) %>%
  summarise(
    wallets = n(),
    group_balance = sum(current_balance),  # Renamed to avoid confusion
    .groups = "drop"
  ) %>%
  mutate(
    supply_pct = group_balance / total_supply * 100,  # Use the global total_supply variable
    wallet_pct = wallets / sum(wallets) * 100
  ) %>%
  select(group, wallets, total_supply = group_balance, supply_pct, wallet_pct)  # Rename back for compatibility



# --- 8. Print Results ---
print(summary_table_early)
print(summary_table_insider)

cat("Total wallets (Early Buyer cohort):", sum(summary_table_early$wallets), "\n")
cat("Total wallets (Insider cohort):", sum(summary_table_insider$wallets), "\n")

# DIAGNOSTIC: Track late buyer supply through the pipeline
cat("\n=== LATE BUYER SUPPLY TRACKING DIAGNOSTIC ===\n")
late_buyer_supply_early <- summary_table_insider$total_supply[summary_table_insider$group == "Late Buyer"]
late_buyer_pct_early <- summary_table_insider$supply_pct[summary_table_insider$group == "Late Buyer"]
late_buyer_count_early <- summary_table_insider$wallets[summary_table_insider$group == "Late Buyer"]

cat("Late Buyer supply from summary table:", round(late_buyer_pct_early, 4), "%\n")
cat("Late Buyer count from summary table:", late_buyer_count_early, "addresses\n")
cat("This will be compared with later calculations to identify where supply is lost.\n")

##VISUALIZATIONS



# --- Early Buyer Cohort ---
summary_table_early$group <- factor(summary_table_early$group, levels = c("Early Buyer", "Descendant", "Late Buyer"))
palette <- c("Early Buyer" = "#FFD700", "Descendant" = "#1A73E8", "Late Buyer" = "#6C757D")

summary_for_table_early <- summary_table_early %>%
  select(group, supply_pct) %>%
  mutate(supply_pct = sprintf("%.2f%%", supply_pct)) %>%
  rename("Group" = group, "Percent of Total Supply" = supply_pct)

p_early <- ggplot(summary_table_early, aes(x = group, y = supply_pct, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.5) +
  scale_fill_manual(values = palette) +
  labs(
    title = "Token Supply Distribution by Group",
    subtitle = "Early Buyers who bought before ~450k.\nA Descendant is a wallet linked to an Early Buyer.",
    x = NULL, y = "Percent of Total Supply", fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 15),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

table_plot_early <- ggtexttable(summary_for_table_early, rows = NULL, theme = ttheme("minimal"))
combined_early <- p_early / table_plot_early + plot_layout(heights = c(3, 1))
print(combined_early)

# --- Insider Cohort ---
summary_table_insider$group <- factor(
  summary_table_insider$group,
  levels = c("Insider", "Insider Descendant", "Late Buyer")
)
palette_insider <- c("Insider" = "#FFD700", "Insider Descendant" = "#1A73E8", "Late Buyer" = "#6C757D")

summary_for_table_insider <- summary_table_insider %>%
  select(group, supply_pct) %>%
  mutate(supply_pct = sprintf("%.2f%%", supply_pct)) %>%
  rename("Group" = group, "Percent of Total Supply" = supply_pct)

p_insider <- ggplot(summary_table_insider, aes(x = group, y = supply_pct, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.5) +
  scale_fill_manual(values = palette_insider) +
  labs(
    title = "USELESS Supply Distribution by Group",
    subtitle = "Insiders who bought in the first 2 hours.\nA Descendant is a wallet linked to an Insider.",
    x = NULL, y = "Percent of Total Supply", fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 16, margin = margin(t = 10)),
    axis.text.y = element_text(size = 15),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

table_plot_insider <- ggtexttable(summary_for_table_insider, rows = NULL, theme = ttheme("minimal"))
combined_insider <- p_insider / table_plot_insider + plot_layout(heights = c(3, 1))
print(combined_insider)


# --- Prepare Data for Comparison ---
summary_table_early <- summary_table_early %>% mutate(cohort = "Early Buyers ~450k")
summary_table_insider <- summary_table_insider %>% mutate(cohort = "Insiders (Very Early)")

# For comparison, use consistent group names for both tables
comparison_df <- bind_rows(
  summary_table_early %>%
    mutate(group = recode(group, "Early Buyer" = "Early Buyer", "Descendant" = "Descendant", "Late Buyer" = "Late Buyer")),
  summary_table_insider %>%
    mutate(group = recode(group, "Insider" = "Early Buyer", "Insider Descendant" = "Descendant", "Late Buyer" = "Late Buyer"))
)

# Ensure group order
comparison_df$group <- factor(comparison_df$group, levels = c("Early Buyer", "Descendant", "Late Buyer"))

# --- Stacked Bar Plot ---
palette <- c("Early Buyer" = "#FFD700", "Descendant" = "#1A73E8", "Late Buyer" = "#6C757D")

p_compare <- ggplot(comparison_df, aes(x = cohort, y = supply_pct, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.5) +
  scale_fill_manual(values = palette) +
  labs(
    title = "Token Supply Distribution by Group",
    subtitle = "Cohort A: Early Buyers (~450k)\nCohort B: Insiders (very early)\nA Descendant is a wallet linked to Early Buyer/Insider.\nSee table below for the percentage of supply each group currently holds.",
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

# --- Prepare the Table for Display ---
summary_for_table_compare <- comparison_df %>%
  select(cohort, group, supply_pct) %>%
  tidyr::pivot_wider(names_from = cohort, values_from = supply_pct) %>%
  mutate(across(where(is.numeric), ~sprintf("%.2f%%", .)))

colnames(summary_for_table_compare) <- c(
  "Group",
  "Early Buyers (Cohort A)",
  "Insiders (Cohort B)"
)

table_plot_compare <- ggtexttable(summary_for_table_compare, rows = NULL, theme = ttheme("minimal"))

# --- Combine Plot and Table ---
combined_compare <- p_compare / table_plot_compare + plot_layout(heights = c(3, 1))
print(combined_compare)

# Create descendant_holdings and eb_holdings for the current early buyer cohort
descendant_holdings <- current_balances_early %>%
  filter(group == "Descendant") %>%
  select(address, current_balance) %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / 1e9 * 100, 6)  # Adjust 1e9 if your total supply is different
  ) %>%
  select(address, supply_pct) %>%
  as_tibble()

eb_holdings <- current_balances_early %>%
  filter(group == "Early Buyer") %>%
  select(address, current_balance) %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / 1e9 * 100, 6)
  ) %>%
  select(address, supply_pct) %>%
  as_tibble()

# -- CLUSTERING SECTION

# --- 1. Build Edge List ---
# ENHANCED: Build edge list from actual transfers table to capture ALL relevant connections
# Original approach only captured early_buyer -> descendant, but missed descendant -> other connections

# Get all addresses of interest (early buyers + descendants)
all_relevant_addresses <- unique(c(insider_addresses, descendant_set_insider))

# Build comprehensive edge list from transfers table
edge_list <- transfers %>%
  filter(
    from_owner %in% all_relevant_addresses & 
    to_owner %in% all_relevant_addresses &
    !from_owner %in% exclude_addresses &
    !to_owner %in% exclude_addresses
  ) %>%
  group_by(from_owner, to_owner) %>%
  summarise(
    transfer_count = n(),
    total_amount = sum(as.numeric(amount), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(transfer_count >= 1) %>%  # At least 1 transfer
  select(from = from_owner, to = to_owner, weight = transfer_count)

cat("Enhanced edge list connections found:", nrow(edge_list), "\n")

# Also create the original edge list for comparison/debugging
original_edge_list <- early_buyers_final %>%
  select(early_buyer = address, recipient_addresses) %>%
  filter(!is.na(recipient_addresses) & recipient_addresses != "") %>%
  rowwise() %>%
  mutate(
    recipients = list(
      trimws(unlist(strsplit(gsub("\\[|\\]|\"|'", "", recipient_addresses), "\\s+")))
    )
  ) %>%
  unnest(recipients) %>%
  filter(recipients != "" & recipients != "NA") %>%
  select(from = early_buyer, to = recipients)

cat("Original edge list connections:", nrow(original_edge_list), "\n")
  
# Note: Hub exclusion already applied in the transfers query above

# --- 2. Build igraph Object ---
# Use only the basic edge structure for clustering (from, to), weights can be used later if needed
edge_list_for_graph <- edge_list %>% select(from, to)
g <- graph_from_data_frame(edge_list_for_graph, directed = TRUE)

# --- 3. Add Node Attributes (supply_pct, type) ---
all_holdings <- bind_rows(
  descendant_holdings %>% mutate(type = "Descendant"),
  eb_holdings %>% mutate(type = "Early Buyer")
)
V(g)$supply_pct <- all_holdings$supply_pct[match(V(g)$name, all_holdings$address)]
V(g)$type <- all_holdings$type[match(V(g)$name, all_holdings$address)]

# --- 4. Community Detection (Louvain) ---
clusters <- cluster_louvain(as.undirected(g))
V(g)$cluster <- clusters$membership

# --- 5. Visualize Clusters ---
ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(color = as.factor(cluster), size = supply_pct), show.legend = TRUE) +
  scale_size_continuous(range = c(2, 10), name = "Supply %") +
  labs(title = "Wallet Network: Early Buyers and Descendants",
       subtitle = "Clusters colored, node size = supply % held") +
  theme_void()

# --- 6. Summarize Clusters by Supply ---
cluster_summary <- data.frame(
  address = V(g)$name,
  cluster = V(g)$cluster,
  supply_pct = V(g)$supply_pct
) %>%
  group_by(cluster) %>%
  summarise(
    n_wallets = n(),
    total_supply_pct = sum(supply_pct, na.rm = TRUE)
  ) %>%
  arrange(desc(total_supply_pct))
print(cluster_summary)

# --- 7. Outlier/Hub Detection ---
deg <- degree(g, mode = "all")
top_hubs <- head(sort(deg, decreasing = TRUE), 20)
print(top_hubs)  # Manually review these addresses for exclusion

# --- 8. Remove Hubs and Recompute Components ---
exclude_in_graph <- exclude_addresses[exclude_addresses %in% V(g)$name]
g_no_hubs <- igraph::delete_vertices(g, exclude_in_graph)
# Fill excluded vector with addresses to exclude after review


comps_no_hubs <- igraph::components(g_no_hubs)
V(g_no_hubs)$component <- comps_no_hubs$membership

address_component_df_no_hubs <- data.frame(
  address = V(g_no_hubs)$name,
  component = V(g_no_hubs)$component,
  supply_pct = V(g_no_hubs)$supply_pct
)

# --- 9. Filter for Significant Holders (For Visualization Only) ---
filtered_addresses_supply <- address_component_df_no_hubs %>%
  filter(!is.na(supply_pct) & supply_pct >= 0.001) %>%
  arrange(component, desc(supply_pct))

# Note: This filtering is only applied to visualizations and clustering analysis
# All addresses (including small holdings) are preserved in CSV exports

# --- FIXED SECTION: Combine Clusters with Individual Holdings ---

# Get all insider/descendant addresses with their holdings
all_insider_descendant_holdings <- bind_rows(
  descendant_holdings %>% mutate(type = "Descendant"),
  eb_holdings %>% mutate(type = "Early Buyer")
) %>%
  arrange(desc(supply_pct))

# Identify which addresses are in clusters vs individual
clustered_addresses <- address_component_df_no_hubs$address
individual_addresses <- setdiff(all_insider_descendant_holdings$address, clustered_addresses)

# Create cluster summaries
cluster_summaries <- filtered_addresses_supply %>%
  group_by(component) %>%
  summarise(
    n_wallets = n(),
    total_supply_pct = sum(supply_pct, na.rm = TRUE),
    type = "Cluster"
  ) %>%
  arrange(desc(total_supply_pct)) %>%
  mutate(
    entity_id = paste0("Cluster_", component),
    entity_name = paste0("Cluster ", row_number())
  )

# Create individual summaries (for addresses not in clusters) - filter for display only
individual_summaries <- all_insider_descendant_holdings %>%
  filter(address %in% individual_addresses & supply_pct >= min_entity_supply_threshold) %>%
  mutate(
    entity_id = address,
    entity_name = paste0("Individual: ", substr(address, 1, 8), "..."),
    n_wallets = 1,
    total_supply_pct = supply_pct,
    type = "Individual"
  ) %>%
  select(entity_id, entity_name, n_wallets, total_supply_pct, type)

# Combine clusters and individuals for visualization
min_entity_supply_threshold <- 0.001  # Minimum 0.001% supply to show in visualization

combined_entities <- bind_rows(
  cluster_summaries %>% select(entity_id, entity_name, n_wallets, total_supply_pct, type),
  individual_summaries
) %>%
  filter(total_supply_pct >= min_entity_supply_threshold) %>%  # Filter for visualization only
  arrange(desc(total_supply_pct))

# Note: This filtering is only for visualization purposes
# All addresses are preserved in the comprehensive CSV exports

# Determine how many entities to show (but cap at reasonable number for readability)
n_entities_to_show <- min(nrow(combined_entities), 25)  # Show all significant entities, max 25
combined_entities <- combined_entities %>% head(n_entities_to_show)

print(paste("=== TOP", n_entities_to_show, "SIGNIFICANT ENTITIES (>=", min_entity_supply_threshold, "% supply) ==="))
print(combined_entities)

# --- FIXED VISUALIZATION: Combined Clusters + Individuals ---
combined_entities <- combined_entities %>%
  mutate(
    display_name = ifelse(
      type == "Cluster",
      paste0(entity_name, "\n(", n_wallets, " wallets)"),
      paste0("Individual\n", substr(gsub("Individual: ", "", entity_name), 1, 8), "...")
    ),
    fill_color = ifelse(type == "Cluster", "#2C3E50", "#E74C3C")
  )

total_top_supply <- sum(combined_entities$total_supply_pct)
subtitle_text <- sprintf("Top %d significant entities control %.2f%% of total supply", n_entities_to_show, total_top_supply)

p_combined <- ggplot(combined_entities, aes(x = reorder(display_name, -total_supply_pct), y = total_supply_pct)) +
  geom_bar(aes(fill = type), stat = "identity", width = 0.7, alpha = 0.92) +
  scale_fill_manual(values = c("Cluster" = "#2C3E50", "Individual" = "#E74C3C")) +
  geom_text(aes(label = sprintf("%.2f%%", total_supply_pct)),
            vjust = -0.5, size = 4, fontface = "bold", color = "#34495E") +
  labs(
    title = "Insider Holdings: Clusters + Individual Addresses",
    subtitle = subtitle_text,
    x = NULL,
    y = "Total Supply (%)",
    fill = "Type"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_combined)

# --- FIXED: Updated Top Clusters Analysis ---
# Now showing combined view instead of just clusters
top_entities_by_supply <- combined_entities %>%
  head(22)

print("=== FIXED: TOP ENTITIES INCLUDING INDIVIDUALS ===")
print(top_entities_by_supply)

# --- 12. Check if a Specific Address is in a Cluster ---
query_address <- "584apKZB85789eCoB5Krr1cdUbftG8ES1WjGKgCHQRVU"
address_info <- address_component_df_no_hubs %>%
  filter(address == query_address)

# Also check if it's an individual holder
individual_info <- all_insider_descendant_holdings %>%
  filter(address == query_address)

if (nrow(address_info) == 0 && nrow(individual_info) == 0) {
  cat("Address not found in clusters or individual holdings.\n")
} else if (nrow(address_info) > 0) {
  cat("Address", query_address, "is in cluster (component):", address_info$component, "\n")
  cat("Current supply held:", address_info$supply_pct, "%\n")
} else if (nrow(individual_info) > 0) {
  cat("Address", query_address, "is an individual holder (not clustered)\n")
  cat("Current supply held:", individual_info$supply_pct, "%\n")
}

# --- 13. Fetch Wallets-supply tibble by component ---
# Set the component number you want to analyze
component_number <- 10  # Change this to your component of interest

# Extract addresses and supply_pct for this component, sorted by supply descending
addresses_supply_in_component <- address_component_df_no_hubs %>%
  filter(component == component_number) %>%
  arrange(desc(supply_pct)) %>%
  as_tibble() %>% 
  head(20)

# Print the result
print(addresses_supply_in_component)

# --- ENHANCED EXPORT: Single Comprehensive CSV with Cluster Information ---

# Create cluster mapping for all addresses
cluster_mapping <- address_component_df_no_hubs %>%
  select(address, component) %>%
  rename(cluster_id = component)

# Export comprehensive insider data with cluster information
# FIXED: Start with ALL addresses from insider/descendant classification, including zero-balance ones
# BUT exclude addresses in the exclude_addresses vector (AMMs, contracts, etc.)
all_insider_descendant_addresses <- unique(c(insider_addresses, descendant_set_insider))

# IMPORTANT: Remove excluded addresses (AMMs, contracts) but keep all other insiders/descendants
all_insider_descendant_addresses <- setdiff(all_insider_descendant_addresses, exclude_addresses)

cat("\n=== INSIDER/DESCENDANT ADDRESS FILTERING ===\n")
cat("Total insider addresses (before exclusion):", length(insider_addresses), "\n")
cat("Total descendant addresses (before exclusion):", length(descendant_set_insider) - length(insider_addresses), "\n")
cat("Addresses removed due to exclude_addresses:", length(intersect(unique(c(insider_addresses, descendant_set_insider)), exclude_addresses)), "\n")
cat("Final addresses for export:", length(all_insider_descendant_addresses), "\n")

# Get current balances for ALL addresses (including those with zero balance)
all_balances_with_zeros <- current_balances %>%
  filter(address %in% all_insider_descendant_addresses) %>%
  # Add any missing addresses with zero balance (addresses that sold everything)
  bind_rows(
    data.frame(
      address = setdiff(all_insider_descendant_addresses, current_balances$address),
      current_balance = 0
    )
  ) %>%
  mutate(
    # Classify based on original insider vs descendant status
    group = case_when(
      address %in% insider_addresses ~ "Insider",
      address %in% descendant_set_insider ~ "Insider Descendant"
    )
  )

insider_wallets_comprehensive <- all_balances_with_zeros %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / sum(current_balances$current_balance) * 100, 6),
    wallet_type = case_when(
      group == "Insider" ~ "Original_Insider",
      group == "Insider Descendant" ~ "Insider_Descendant"
    )
  ) %>%
  # Add cluster information
  left_join(cluster_mapping, by = "address") %>%
  mutate(
    classification = case_when(
      !is.na(cluster_id) ~ "Clustered",
      is.na(cluster_id) ~ "Individual"
    ),
    cluster_id = ifelse(is.na(cluster_id), "N/A", as.character(cluster_id))
  ) %>%
  select(address, wallet_type, current_balance, supply_pct, classification, cluster_id) %>%
  arrange(desc(supply_pct))

# DIAGNOSTIC: Show what we recovered
cat("\n=== EXPORT RECOVERY DIAGNOSTIC ===\n")
cat("Total insider addresses (after exclusion filtering):", sum(all_insider_descendant_addresses %in% insider_addresses), "\n")
cat("Total descendant addresses (after exclusion filtering):", sum(all_insider_descendant_addresses %in% descendant_set_insider) - sum(all_insider_descendant_addresses %in% insider_addresses), "\n")
cat("Total addresses in export:", nrow(insider_wallets_comprehensive), "\n")
cat("Addresses with zero balance (likely sold everything):", sum(insider_wallets_comprehensive$current_balance == 0), "\n")
cat("Addresses with current holdings:", sum(insider_wallets_comprehensive$current_balance > 0), "\n")
cat("This should now include all insider/descendants except those in exclude_addresses.\n")


# Export single comprehensive list with all metadata (ALL ADDRESSES - NO FILTERING)
# This ensures addresses that may have sold recently are retained for supply evolution analysis
write_csv(insider_wallets_comprehensive, "insider_wallets_comprehensive.csv")

# --- FILTER FOR SIGNIFICANT HOLDERS ONLY FOR VISUALIZATIONS ---
# Keep separate filtered version for summary tables and visualizations
significant_threshold <- 0.001  # 0.001% of supply minimum

insider_wallets_for_display <- insider_wallets_comprehensive %>%
  filter(supply_pct >= significant_threshold)

cat("All insider addresses exported:", nrow(insider_wallets_comprehensive), "\n")
cat("Addresses for visualization (>=", significant_threshold, "% supply):", nrow(insider_wallets_for_display), "\n")

# Print summary of export (ALL ADDRESSES)
cat("\n=== EXPORT SUMMARY (All Insider/Descendant Addresses) ===\n")
export_summary <- insider_wallets_comprehensive %>%
  group_by(classification, wallet_type) %>%
  summarise(
    count = n(),
    total_supply = sum(supply_pct),
    .groups = "drop"
  )
print(export_summary)

# Print summary for display/visualization purposes
cat("\n=== DISPLAY SUMMARY (Significant Holders Only) ===\n")
export_summary_display <- insider_wallets_for_display %>%
  group_by(classification, wallet_type) %>%
  summarise(
    count = n(),
    total_supply = sum(supply_pct),
    .groups = "drop"
  )
print(export_summary_display)

# Summary statistics
cat("\n=== INSIDER/EARLY BUYER SUMMARY ===\n")
cat("Total clustered addresses:", length(clustered_addresses), "\n")
cat("Total individual addresses:", length(individual_addresses), "\n")
cat("Top individual holder supply %:", max(individual_summaries$total_supply_pct, na.rm = TRUE), "\n")
cat("Top cluster supply %:", max(cluster_summaries$total_supply_pct, na.rm = TRUE), "\n")

# ===============================================================================
# EXCHANGE WALLET DETECTION AND CLASSIFICATION
# ===============================================================================

cat("\n=== USING PREVIOUSLY LOADED EXCHANGE WALLET DATA ===\n")

# Use exchange wallet information loaded earlier (to prevent descendant contamination)
exchange_wallets <- exchange_wallets_early
exchange_addresses <- exchange_addresses_early
cat("Using", length(exchange_addresses), "exchange wallet addresses (loaded earlier)\n")

if (length(exchange_addresses) > 0) {
  exchange_summary <- exchange_wallets %>%
    group_by(cex_name) %>%
    summarise(wallet_count = n(), .groups = "drop") %>%
    arrange(desc(wallet_count))
  
  cat("Exchange breakdown:\n")
  print(exchange_summary)
}

# ===============================================================================
# LATE BUYER CLUSTERING SECTION (RETAIL ONLY)
# ===============================================================================

cat("\n=== STARTING LATE BUYER CLUSTER ANALYSIS (RETAIL ONLY) ===\n")

# Separate late buyers into Exchange vs Retail categories
late_buyers_all <- current_balances_insider %>%
  filter(group == "Late Buyer") %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = current_balance / total_supply * 100,
    # Classify as Exchange or Retail
    category = ifelse(address %in% exchange_addresses, "Exchange", "Retail")
  )

# Display exchange vs retail breakdown
cat("\n=== LATE BUYER CATEGORY BREAKDOWN ===\n")
category_summary <- late_buyers_all %>%
  group_by(category) %>%
  summarise(
    count = n(),
    total_supply_pct = sum(supply_pct),
    avg_supply_pct = mean(supply_pct),
    median_supply_pct = median(supply_pct),
    max_supply_pct = max(supply_pct),
    .groups = "drop"
  )
print(category_summary)

# DIAGNOSTIC: Check for supply calculation discrepancy
cat("\n=== SUPPLY CALCULATION DIAGNOSTIC ===\n")
original_late_buyer_supply <- sum(current_balances_insider$current_balance[current_balances_insider$group == "Late Buyer"]) / total_supply * 100
new_calculation_supply <- sum(late_buyers_all$supply_pct, na.rm = TRUE)

cat("Late Buyer supply from summary table:", round(late_buyer_pct_early, 4), "%\n")
cat("Late Buyer supply recalculated here:", round(original_late_buyer_supply, 4), "%\n")
cat("New calculation supply (Exchange + Retail):", round(new_calculation_supply, 4), "%\n")

# Check for discrepancies
summary_vs_recalc_diff <- late_buyer_pct_early - original_late_buyer_supply
recalc_vs_new_diff <- original_late_buyer_supply - new_calculation_supply

cat("Summary table vs recalculation difference:", round(summary_vs_recalc_diff, 4), "%\n")
cat("Recalculation vs new calculation difference:", round(recalc_vs_new_diff, 4), "%\n")

if (abs(summary_vs_recalc_diff) > 0.1) {
  cat("WARNING: Discrepancy between summary table and recalculation!\n")
  cat("This suggests different total_supply values were used.\n")
}

if (abs(recalc_vs_new_diff) > 0.1) {
  cat("WARNING: Significant supply calculation discrepancy detected!\n")
  cat("This suggests addresses or balances are being lost in the process.\n")
}

# Extract exchange wallets for separate analysis
late_buyers_exchange <- late_buyers_all %>%
  filter(category == "Exchange") %>%
  left_join(exchange_wallets %>% select(address, cex_name, distinct_name), by = "address")

if (nrow(late_buyers_exchange) > 0) {
  cat("\n=== EXCHANGE WALLETS IN LATE BUYERS ===\n")
  exchange_holdings <- late_buyers_exchange %>%
    arrange(desc(supply_pct)) %>%
    select(address, cex_name, distinct_name, supply_pct, current_balance) %>%
    mutate(
      address_short = paste0(substr(address, 1, 8), "..."),
      supply_pct = round(supply_pct, 4)
    )
  print(exchange_holdings)
  
  # Exchange summary by CEX
  exchange_by_cex <- late_buyers_exchange %>%
    group_by(cex_name) %>%
    summarise(
      wallet_count = n(),
      total_supply_pct = sum(supply_pct),
      avg_supply_pct = mean(supply_pct),
      .groups = "drop"
    ) %>%
    arrange(desc(total_supply_pct))
  
  cat("\n=== EXCHANGE HOLDINGS BY CEX ===\n")
  print(exchange_by_cex)
}

# Extract retail late buyers for clustering analysis
late_buyers <- late_buyers_all %>%
  filter(category == "Retail")

cat("\n=== RETAIL LATE BUYER FILTERING ===\n")
cat("Original late buyers (all):", nrow(late_buyers_all), "\n")
cat("Exchange wallets removed:", nrow(late_buyers_exchange), "\n") 
cat("Retail late buyers for clustering:", nrow(late_buyers), "\n")
cat("Exchange supply excluded:", round(sum(late_buyers_exchange$supply_pct, na.rm = TRUE), 4), "%\n")
cat("Retail supply for analysis:", round(sum(late_buyers$supply_pct, na.rm = TRUE), 4), "%\n")

# Keep all late buyers for export, create separate filtered version for clustering
late_buyers_all_for_export <- late_buyers  # Keep all addresses for export

# Apply minimum threshold only for clustering analysis (visualization)
late_buyers_for_clustering <- late_buyers %>%
  filter(supply_pct >= 0.001) %>%  # Only significant holders for clustering
  as_tibble()

cat("All Retail Late Buyers (for export):", nrow(late_buyers_all_for_export), "\n")
cat("Retail Late Buyers for clustering (>0.001%):", nrow(late_buyers_for_clustering), "\n")
cat("Retail Late Buyer supply coverage (all):", round(sum(late_buyers_all_for_export$supply_pct), 2), "%\n")
cat("Retail Late Buyer supply coverage (clustering):", round(sum(late_buyers_for_clustering$supply_pct), 2), "%\n")

# --- 2. Build Late Buyer Transfer Network ---
# Find transfers between late buyers (use clustering subset for network analysis)
late_buyer_addresses <- late_buyers_for_clustering$address

# Note: We use the filtered version for clustering to avoid noise in network analysis
# but the full version will be used for exports to retain all addresses

late_buyer_transfers <- transfers %>%
  filter(
    from_owner %in% late_buyer_addresses & 
    to_owner %in% late_buyer_addresses &
    !from_owner %in% exclude_addresses &
    !to_owner %in% exclude_addresses
  ) %>%
  group_by(from_owner, to_owner) %>%
  summarise(
    transfer_count = n(),
    total_amount = sum(as.numeric(amount), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(transfer_count >= 1) %>%  # At least 1 transfer
  select(from = from_owner, to = to_owner, weight = transfer_count)

cat("Late buyer transfer connections found:", nrow(late_buyer_transfers), "\n")

# --- 3. Build Late Buyer Network Graph ---
if (nrow(late_buyer_transfers) > 0) {
  
  g_late <- graph_from_data_frame(late_buyer_transfers, directed = TRUE)
  
  # Add node attributes
  late_buyer_supply_map <- setNames(late_buyers_for_clustering$supply_pct, late_buyers_for_clustering$address)
  V(g_late)$supply_pct <- late_buyer_supply_map[V(g_late)$name]
  V(g_late)$type <- "Late Buyer"
  
  # --- 4. Community Detection for Late Buyers ---
  clusters_late <- cluster_louvain(as.undirected(g_late))
  V(g_late)$cluster <- clusters_late$membership
  
  cat("Late buyer clusters found:", max(clusters_late$membership), "\n")
  
  # --- 5. Visualize Late Buyer Network ---
  p_late_network <- ggraph(g_late, layout = "fr") +
    geom_edge_link(alpha = 0.3, aes(width = weight)) +
    geom_node_point(aes(color = as.factor(cluster), size = supply_pct), show.legend = TRUE) +
    scale_size_continuous(range = c(1, 8), name = "Supply %") +
    scale_edge_width_continuous(range = c(0.5, 2), name = "Transfers") +
    labs(
      title = "Late Buyer Network Clustering",
      subtitle = "Connections based on token transfers, clusters colored by community detection",
      color = "Cluster"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30")
    )
  
  print(p_late_network)
  
  # --- 6. Analyze Late Buyer Clusters ---
  late_cluster_summary <- data.frame(
    address = V(g_late)$name,
    cluster = V(g_late)$cluster,
    supply_pct = V(g_late)$supply_pct
  ) %>%
    group_by(cluster) %>%
    summarise(
      n_wallets = n(),
      total_supply_pct = sum(supply_pct, na.rm = TRUE),
      avg_supply_pct = mean(supply_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_supply_pct))
  
  print("=== LATE BUYER CLUSTERS BY SUPPLY ===")
  print(late_cluster_summary)
  
  # --- 7. Get individual late buyers not in clusters ---
  late_clustered_addresses <- V(g_late)$name
  late_individual_addresses <- setdiff(late_buyers$address, late_clustered_addresses)
  
  # --- 8. Combined Late Buyer Analysis (Clusters + Individuals) ---
  late_cluster_entities <- late_cluster_summary %>%
    mutate(
      entity_id = paste0("LateBuyer_Cluster_", cluster),
      entity_name = paste0("Late Buyer Cluster ", row_number()),
      type = "Cluster"
    ) %>%
    select(entity_id, entity_name, n_wallets, total_supply_pct, type)
  
  late_individual_entities <- late_buyers_for_clustering %>%
    filter(address %in% late_individual_addresses) %>%
    mutate(
      entity_id = address,
      entity_name = paste0("Late Individual: ", substr(address, 1, 8), "..."),
      n_wallets = 1,
      total_supply_pct = supply_pct,
      type = "Individual"
    ) %>%
    select(entity_id, entity_name, n_wallets, total_supply_pct, type)
  
  # Combine and get top 15
  late_combined_entities <- bind_rows(
    late_cluster_entities,
    late_individual_entities
  ) %>%
    arrange(desc(total_supply_pct)) %>%
    head(15)
  
  print("=== TOP 15 LATE BUYER ENTITIES ===")
  print(late_combined_entities)
  
  # --- 9. Visualization: Late Buyer Entities ---
  late_combined_entities <- late_combined_entities %>%
    mutate(
      display_name = ifelse(
        type == "Cluster",
        paste0(gsub("Late Buyer ", "", entity_name), "\n(", n_wallets, " wallets)"),
        paste0("Individual\n", substr(gsub("Late Individual: ", "", entity_name), 1, 8), "...")
      )
    )
  
  total_late_supply <- sum(late_combined_entities$total_supply_pct)
  late_subtitle_text <- sprintf("Top 15 late buyer entities control %.2f%% of total supply", total_late_supply)
  
  p_late_combined <- ggplot(late_combined_entities, aes(x = reorder(display_name, -total_supply_pct), y = total_supply_pct)) +
    geom_bar(aes(fill = type), stat = "identity", width = 0.7, alpha = 0.92) +
    scale_fill_manual(values = c("Cluster" = "#8E44AD", "Individual" = "#F39C12")) +
    geom_text(aes(label = sprintf("%.3f%%", total_supply_pct)),
              vjust = -0.5, size = 3.5, fontface = "bold", color = "#34495E") +
    labs(
      title = "Late Buyer Holdings: Clusters + Individual Addresses",
      subtitle = late_subtitle_text,
      x = NULL,
      y = "Total Supply (%)",
      fill = "Type"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 11),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(p_late_combined)
  
  # --- TWITTER MARKETING VERSION (Same Data, Blurred Details) ---
  # Create blurred version that shows same entities but hides specifics
  
  late_combined_entities_twitter <- late_combined_entities %>%
    mutate(
      # Blur the exact percentages but keep relative sizes
      blurred_percentage = sprintf("%.3f%%", total_supply_pct),
      
      # Create blurred display names
      display_name_twitter = ifelse(
        type == "Cluster",
        paste0("Cluster ", row_number(), "\n(", n_wallets, " wallets)"),
        paste0("Individual ", row_number(), "\n", substr(gsub("Late Individual: ", "", entity_name), 1, 8), "...")
      ),
      # Actual axis labels to blur (wallet counts or short address)
      axis_label_actual = ifelse(
        type == "Cluster",
        paste0("Cluster ", row_number(), "\n(", n_wallets, " wallets)"),
        paste0(substr(gsub("Late Individual: ", "", entity_name), 1, 8), "...")
      )
    )
  
  # Create Twitter version with same data structure
  p_late_twitter <- ggplot(late_combined_entities_twitter, aes(x = reorder(display_name_twitter, -total_supply_pct), y = total_supply_pct)) +
    geom_bar(aes(fill = type), stat = "identity", width = 0.7, alpha = 0.92) +
    scale_fill_manual(values = c("Cluster" = "#8E44AD", "Individual" = "#F39C12")) +
    
    # Show blurred percentages instead of exact ones
    ggfx::with_blur(geom_text(aes(label = blurred_percentage), vjust = -0.5, size = 3.5, fontface = "bold", color = "#34495E"), sigma = 5) +
    
    # Add blurred x-axis labels with actual counts/addresses while hiding axis text
    ggfx::with_blur(
      geom_text(
        data = late_combined_entities_twitter,
        aes(x = reorder(display_name_twitter, -total_supply_pct), y = 0, label = axis_label_actual),
        vjust = 1.3, size = 3.2, fontface = "bold", color = "#2C3E50"
      ),
      sigma = 6
    ) +
    
    # Add watermark
    annotate("text", x = Inf, y = Inf, label = "@YourTwitterHandle", 
             hjust = 1.1, vjust = 1.1, size = 5, alpha = 0.7, color = "#34495E", fontface = "bold") +
    
    labs(
      title = "🔍 Late Buyer Network Analysis",
      subtitle = "Cluster detection reveals coordinated buying patterns\n📊 Want exact percentages & addresses? DM for full analysis",
      x = "Anonymous Entities (Real Data Available)",
      y = "Token Holdings (%)",
      fill = "Entity Type",
      caption = "📧 Professional analysis available | Contact for complete reports with exact data"
    ) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7F8C8D", margin = margin(b = 20)),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "#95A5A6", face = "italic"),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 11),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#FAFAFA", color = NA),
      plot.margin = margin(t = 10, r = 20, b = 30, l = 20)
    )
  
  cat("\n=== TWITTER MARKETING VERSION CREATED ===\n")
  cat("Same entities and relative sizes, but with blurred specifics\n")
  cat("Perfect for showcasing analysis power while protecting client data\n")
  
  print(p_late_twitter)
  
  # --- 10. Export Late Buyer Cluster Data ---
  late_cluster_mapping <- data.frame(
    address = V(g_late)$name,
    late_cluster_id = V(g_late)$cluster
  )
  
  # Create comprehensive export using ALL late buyers (not just clustering subset)
  late_buyer_comprehensive <- late_buyers_all_for_export %>%
    left_join(late_cluster_mapping, by = "address") %>%
    mutate(
      classification = case_when(
        !is.na(late_cluster_id) ~ "Clustered",
        is.na(late_cluster_id) ~ "Individual"
      ),
      late_cluster_id = ifelse(is.na(late_cluster_id), "N/A", as.character(late_cluster_id)),
      wallet_type = "Late_Buyer"
    ) %>%
    select(address, wallet_type, current_balance, supply_pct, classification, late_cluster_id) %>%
    arrange(desc(supply_pct))
  
  # Export late buyer data
  write_csv(late_buyer_comprehensive, "late_buyer_clusters_comprehensive.csv")
  
  # --- 11. Summary Statistics ---
  cat("\n=== LATE BUYER CLUSTER SUMMARY ===\n")
  cat("Total late buyer clusters:", max(clusters_late$membership), "\n")
  cat("Late buyers in clusters:", length(late_clustered_addresses), "\n")
  cat("Individual late buyers:", length(late_individual_addresses), "\n")
  cat("Largest late buyer cluster supply %:", max(late_cluster_summary$total_supply_pct, na.rm = TRUE), "\n")
  cat("Top individual late buyer supply %:", ifelse(length(late_individual_addresses) > 0, 
                                                    max(late_individual_entities$total_supply_pct, na.rm = TRUE), 0), "\n")
  
  # Export summary
  late_export_summary <- late_buyer_comprehensive %>%
    group_by(classification) %>%
    summarise(
      count = n(),
      total_supply = sum(supply_pct),
      .groups = "drop"
    )
  print(late_export_summary)
  
} else {
  cat("No significant transfer connections found between late buyers.\n")
  
  # Still create individual late buyer analysis (use clustering subset for display)
  late_individual_entities <- late_buyers_for_clustering %>%
    head(15) %>%
    mutate(
      entity_name = paste0("Late Individual: ", substr(address, 1, 8), "...")
    )
  
  print("=== TOP 15 INDIVIDUAL LATE BUYERS ===")
  print(late_individual_entities %>% select(entity_name, supply_pct, current_balance))
  
  # Export individual late buyers (use ALL late buyers, not just clustering subset)
  late_buyer_comprehensive <- late_buyers_all_for_export %>%
    mutate(
      classification = "Individual",
      late_cluster_id = "N/A",
      wallet_type = "Late_Buyer"
    ) %>%
    select(address, wallet_type, current_balance, supply_pct, classification, late_cluster_id)
  
  write_csv(late_buyer_comprehensive, "late_buyer_clusters_comprehensive.csv")
  
  # Create simple entities list for Twitter marketing function
  late_combined_entities_global <<- late_individual_entities %>%
    head(15) %>%
    mutate(
      display_name = paste0("Individual\n", substr(gsub("Late Individual: ", "", entity_name), 1, 8), "...")
    )
  
}

# ===============================================================================
# COMPREHENSIVE THREE-TIER EXPORT
# ===============================================================================

cat("\n=== CREATING COMPREHENSIVE THREE-TIER EXPORT ===\n")

# Combine all three categories into one comprehensive dataset
comprehensive_analysis <- bind_rows(
  # 1. Insider/Descendant data
  insider_wallets_comprehensive %>%
    mutate(
      category = "Insider/Descendant",
      cex_name = NA_character_,
      distinct_name = NA_character_
    ) %>%
    select(address, category, wallet_type, current_balance, supply_pct, classification, 
           cluster_id, cex_name, distinct_name),
  
  # 2. Retail Late Buyer data (if clustering was successful)
  if (exists("late_buyer_comprehensive") && nrow(late_buyer_comprehensive) > 0) {
    late_buyer_comprehensive %>%
      mutate(
        category = "Retail Late Buyer",
        cluster_id = late_cluster_id,
        cex_name = NA_character_,
        distinct_name = NA_character_
      ) %>%
      select(address, category, wallet_type, current_balance, supply_pct, classification,
             cluster_id, cex_name, distinct_name)
  } else {
    # Empty dataframe with correct structure if no late buyer clustering
    data.frame(
      address = character(0), category = character(0), wallet_type = character(0),
      current_balance = numeric(0), supply_pct = numeric(0), classification = character(0),
      cluster_id = character(0), cex_name = character(0), distinct_name = character(0)
    )
  },
  
  # 3. Exchange data
  if (nrow(late_buyers_exchange) > 0) {
    late_buyers_exchange %>%
      mutate(
        category = "Exchange/Institutional",
        wallet_type = "Exchange",
        classification = "Individual",
        cluster_id = "N/A"
      ) %>%
      select(address, category, wallet_type, current_balance, supply_pct, classification,
             cluster_id, cex_name, distinct_name)
  } else {
    # Empty dataframe with correct structure if no exchanges found
    data.frame(
      address = character(0), category = character(0), wallet_type = character(0),
      current_balance = numeric(0), supply_pct = numeric(0), classification = character(0),
      cluster_id = character(0), cex_name = character(0), distinct_name = character(0)
    )
  }
) %>%
  arrange(desc(supply_pct))

# Export comprehensive analysis
write_csv(comprehensive_analysis, "comprehensive_three_tier_analysis.csv")

# Final summary statistics
cat("\n=== FINAL THREE-TIER ANALYSIS SUMMARY ===\n")
final_summary <- comprehensive_analysis %>%
  group_by(category) %>%
  summarise(
    address_count = n(),
    total_supply_pct = round(sum(supply_pct, na.rm = TRUE), 4),
    avg_supply_pct = round(mean(supply_pct, na.rm = TRUE), 6),
    median_supply_pct = round(median(supply_pct, na.rm = TRUE), 6),
    max_supply_pct = round(max(supply_pct, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(total_supply_pct))

print(final_summary)

# Exchange-specific summary if exchanges were found
if (nrow(late_buyers_exchange) > 0) {
  cat("\n=== EXCHANGE BREAKDOWN ===\n")
  exchange_breakdown <- comprehensive_analysis %>%
    filter(category == "Exchange/Institutional") %>%
    group_by(cex_name) %>%
    summarise(
      wallet_count = n(),
      total_supply_pct = round(sum(supply_pct, na.rm = TRUE), 4),
      avg_supply_pct = round(mean(supply_pct, na.rm = TRUE), 6),
      .groups = "drop"
    ) %>%
    arrange(desc(total_supply_pct))
  
  print(exchange_breakdown)
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Files exported (ALL ADDRESSES - NO FILTERING FOR CURRENT HOLDINGS):\n")
cat("- insider_wallets_comprehensive.csv (Insider/Descendant analysis)\n")
if (exists("late_buyer_comprehensive") && nrow(late_buyer_comprehensive) > 0) {
  cat("- late_buyer_clusters_comprehensive.csv (Retail late buyer clustering)\n")
}
cat("- comprehensive_three_tier_analysis.csv (Complete three-tier analysis)\n")
cat("\nIMPORTANT: All CSV exports include addresses regardless of current holdings.\n")
cat("This ensures addresses that may have sold large amounts recently are retained\n")
cat("for accurate supply evolution timeline analysis.\n")
cat("\nFiltering (supply_pct >= 0.001%) is only applied to visualizations and summary tables.\n")

# Coverage analysis
total_analyzed_supply <- sum(comprehensive_analysis$supply_pct, na.rm = TRUE)
cat("\nTotal supply coverage:", round(total_analyzed_supply, 2), "%\n")

if (total_analyzed_supply < 95) {
  cat("NOTE: Coverage below 95% - consider lowering minimum supply thresholds\n")
}


# === STANDALONE SECTION: LATE BUYER CLUSTER EXPLORATION ===
# This section can be run independently after the main late buyer clustering analysis
# Prerequisites: 
# - late_buyer_clusters_comprehensive.csv (generated by late buyer clustering)
# - Variable 'g_late' (the late buyer network graph) must exist
# - Variable 'late_buyer_transfers' must exist
# - Variable 'late_buyer_supply_map' must exist
# - Variable 'late_buyers' dataframe must exist

cat("\n=== LATE BUYER CLUSTER EXPLORATION TOOL ===\n")

# Configuration: Set the cluster number you want to analyze
late_cluster_number <- 1  # Change this to your cluster of interest

# Check if required data exists
if (!exists("g_late")) {
  stop("Required variable 'g_late' not found. Please run the main late buyer clustering analysis first.")
}

if (!exists("late_buyer_transfers")) {
  stop("Required variable 'late_buyer_transfers' not found. Please run the main late buyer clustering analysis first.")
}

if (!exists("late_buyer_supply_map")) {
  stop("Required variable 'late_buyer_supply_map' not found. Please run the main late buyer clustering analysis first.")
}

if (!exists("late_buyers_for_clustering")) {
  stop("Required variable 'late_buyers_for_clustering' not found. Please run the main late buyer clustering analysis first.")
}

# Extract addresses and supply_pct for the specified late buyer cluster, sorted by supply descending
late_addresses_supply_in_cluster <- data.frame(
  address = V(g_late)$name,
  cluster = V(g_late)$cluster,
  supply_pct = V(g_late)$supply_pct
) %>%
  filter(cluster == late_cluster_number) %>%
  left_join(late_buyers_for_clustering %>% select(address, current_balance), by = "address") %>%
  arrange(desc(supply_pct)) %>%
  as_tibble()

# Display cluster information
cat("\n=== LATE BUYER CLUSTER", late_cluster_number, "DETAILS ===\n")
cat("Cluster size:", nrow(late_addresses_supply_in_cluster), "addresses\n")
cat("Total cluster supply:", round(sum(late_addresses_supply_in_cluster$supply_pct, na.rm = TRUE), 4), "%\n")
cat("Largest holder in cluster:", round(max(late_addresses_supply_in_cluster$supply_pct, na.rm = TRUE), 4), "%\n")

print("=== LATE BUYER CLUSTER MEMBERS ===")
print(late_addresses_supply_in_cluster)

# Visualize the specific late buyer cluster
if (nrow(late_addresses_supply_in_cluster) > 0) {
  
  # Create subgraph for this specific cluster
  cluster_addresses <- late_addresses_supply_in_cluster$address
  cluster_edges <- late_buyer_transfers %>%
    filter(from %in% cluster_addresses & to %in% cluster_addresses)
  
  if (nrow(cluster_edges) > 0) {
    g_cluster <- graph_from_data_frame(cluster_edges, directed = TRUE)
    
    # Add node attributes for visualization
    V(g_cluster)$supply_pct <- late_buyer_supply_map[V(g_cluster)$name]
    V(g_cluster)$display_name <- paste0(substr(V(g_cluster)$name, 1, 8), "...")
    
    # Create network visualization
    p_cluster_viz <- ggraph(g_cluster, layout = "fr") +
      geom_edge_link(alpha = 0.6, aes(width = weight), color = "#8E44AD") +
      geom_node_point(aes(size = supply_pct), color = "#8E44AD", alpha = 0.8) +
      geom_node_text(aes(label = display_name), 
                     size = 3, color = "black", fontface = "bold",
                     nudge_y = 0.1, check_overlap = TRUE) +
      scale_size_continuous(range = c(3, 12), name = "Supply %") +
      scale_edge_width_continuous(range = c(0.5, 3), name = "Transfers") +
      labs(
        title = paste("Late Buyer Cluster", late_cluster_number, "Network"),
        subtitle = paste("Total supply:", round(sum(late_addresses_supply_in_cluster$supply_pct, na.rm = TRUE), 4), 
                        "% |", nrow(late_addresses_supply_in_cluster), "addresses"),
        caption = "Node size = supply %, Edge width = transfer frequency"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50"),
        legend.position = "bottom"
      )
    
    print(p_cluster_viz)
    
  } else {
    cat("No transfer connections found within cluster", late_cluster_number, "- showing bar chart instead\n")
  }
  
  # Alternative visualization: Bar chart of cluster members by supply
  late_addresses_supply_in_cluster <- late_addresses_supply_in_cluster %>%
    mutate(display_name = paste0(substr(address, 1, 8), "..."))
  
  p_cluster_bar <- ggplot(late_addresses_supply_in_cluster, 
                         aes(x = reorder(display_name, -supply_pct), y = supply_pct)) +
    geom_bar(stat = "identity", fill = "#8E44AD", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = sprintf("%.4f%%", supply_pct)), 
              vjust = -0.3, size = 3.5, fontface = "bold") +
    labs(
      title = paste("Late Buyer Cluster", late_cluster_number, "- Holdings Distribution"),
      subtitle = paste("Total cluster supply:", round(sum(late_addresses_supply_in_cluster$supply_pct, na.rm = TRUE), 4), "%"),
      x = "Cluster Member",
      y = "Supply Percentage (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  
  print(p_cluster_bar)
  
} else {
  cat("No addresses found in cluster", late_cluster_number, "\n")
}

cat("\n=== CLUSTER EXPLORATION COMPLETE ===\n")
cat("To explore a different cluster, change 'late_cluster_number' and re-run this section.\n")

