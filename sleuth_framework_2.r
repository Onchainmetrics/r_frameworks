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
  next_descendants <- setdiff(next_descendants, c(descendant_set_insiders, exclude_addresses))
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

##print addy-supply table for descendants
descendant_holdings <- current_balances %>%
  filter(group == "Descendant") %>%
  select(address, current_balance) %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / 1e9 * 100, 6)  # Round to 6 decimal places
  ) %>%
  select(address, supply_pct) %>%
  as_tibble()

print(descendant_holdings)

##print addy-supply table for early buyers
eb_holdings <- current_balances %>%
  filter(group == "Early Buyer") %>%
  select(address, current_balance) %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / 1e9 * 100, 6)  # Round to 6 decimal places
  ) %>%
  select(address, supply_pct) %>%
  as_tibble()

print(eb_holdings)

##print addy-supply table for insiders
insider_holdings <- current_balances %>%
  filter(address %in% insider_addresses) %>%   # Only insiders, not descendants
  select(address, current_balance) %>%
  mutate(
    supply_pct = round(current_balance / 1e9 * 100, 6)  # 1e9 = total supply, adjust if different
  ) %>%
  arrange(desc(supply_pct)) %>%
  as_tibble()

print(insider_holdings)
##print addy-supply table for insider descendants
insider_descendant_holdings <- current_balances %>%
  filter(group_insiders == "Insider Descendant") %>%
  select(address, current_balance) %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / 1e9 * 100, 6)  # Round to 6 decimal places
  ) %>%
  select(address, supply_pct) %>%
  as_tibble()

print(insider_descendant_holdings)
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

###FOR SINGLE COHORT

# --- Visualize Supply Distribution for a Single Cohort ---

library(ggplot2)
library(ggpubr)
library(dplyr)
library(patchwork)

# Ensure group order for consistent coloring
summary_table$group <- factor(summary_table$group, levels = c("Early Buyer", "Descendant", "Late Buyer"))

# Elegant color palette
palette <- c("Early Buyer" = "#FFD700", "Descendant" = "#1A73E8", "Late Buyer" = "#6C757D")

# Prepare summary table for display
summary_for_table <- summary_table %>%
  select(group, supply_pct) %>%
  mutate(supply_pct = sprintf("%.2f%%", supply_pct)) %>%
  rename("Group" = group, "Percent of Total Supply" = supply_pct)

# Bar plot
p <- ggplot(summary_table, aes(x = group, y = supply_pct, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "white", size = 0.5) +
  scale_fill_manual(values = palette) +
  labs(
    title = "Token Supply Distribution by Group",
    subtitle = "Based on your chosen cutoff for Early Buyers.\nA Descendant is a wallet linked to an Early Buyer.",
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

# Table plot
table_plot <- ggtexttable(summary_for_table, rows = NULL, theme = ttheme("minimal"))

# Combine with patchwork
combined <- p / table_plot + plot_layout(heights = c(3, 1))
print(combined)

##wallet clustering

library(igraph)
library(tidyr)
library(ggraph)
library(dplyr)
# Parse recipient_addresses into a long format edge list
edge_list <- early_buyers_final %>%
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

# Optionally, add descendant-to-descendant edges from your transfers data
# For a richer network, you can also add all transfer edges:
# transfer_edges <- transfers %>% select(from = from_owner, to = to_owner)
# edge_list <- bind_rows(edge_list, transfer_edges)

# Build the igraph object
g <- graph_from_data_frame(edge_list, directed = TRUE)

##add node attributes (supply_pct)
# Combine all addresses with their supply_pct
all_holdings <- bind_rows(
  descendant_holdings %>% mutate(type = "Descendant"),
  eb_holdings %>% mutate(type = "Early Buyer")
)

# Add supply_pct as a node attribute
V(g)$supply_pct <- all_holdings$supply_pct[match(V(g)$name, all_holdings$address)]
V(g)$type <- all_holdings$type[match(V(g)$name, all_holdings$address)]

# Use Louvain community detection (works on undirected graphs)
clusters <- cluster_louvain(as.undirected(g))
V(g)$cluster <- clusters$membership



ggraph(g, layout = "fr") +  # 'fr' = Fruchterman-Reingold layout
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(color = as.factor(cluster), size = supply_pct), show.legend = TRUE) +
  scale_size_continuous(range = c(2, 10), name = "Supply %") +
  labs(title = "Wallet Network: Early Buyers and Descendants",
       subtitle = "Clusters colored, node size = supply % held") +
  theme_void()

# For each cluster, sum the supply_pct
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

# Extract addresses and supply_pct for cluster n
cluster_number <- 2  # change this to your cluster of interest

addresses_supply_in_cluster <- data.frame(
  address = V(g)$name[V(g)$cluster == cluster_number],
  supply_pct = V(g)$supply_pct[V(g)$cluster == cluster_number]
) %>%
  arrange(desc(supply_pct))  # sort by supply descending

print(addresses_supply_in_cluster)

##find true connection through components
# Find connected components in the graph
comps <- components(g)

# comps$membership is a named vector: address -> component number
# comps$csize is the size of each component

# Add component info to each node
V(g)$component <- comps$membership

# To get a data frame of addresses and their component
address_component_df <- data.frame(
  address = V(g)$name,
  component = V(g)$component
)

# To see how many components and their sizes
table(address_component_df$component)

# For example, to get addresses in component 1:
addresses_in_component1 <- address_component_df$address[address_component_df$component == 1]

# To get all components as a list:
components_list <- split(address_component_df$address, address_component_df$component)

# If you have supply_pct as a node attribute:
address_component_df$supply_pct <- V(g)$supply_pct

# Summarize supply per component
component_supply_summary <- address_component_df %>%
  group_by(component) %>%
  summarise(
    n_wallets = n(),
    total_supply_pct = sum(supply_pct, na.rm = TRUE)
  ) %>%
  arrange(desc(total_supply_pct))

print(component_supply_summary)

#find potential hub
# Get degree (number of connections) for each node
deg <- degree(g, mode = "all")
# Sort by degree, descending
head(sort(deg, decreasing = TRUE), 10)

##check highest degree connections and filter out smart contracts/amms/bots
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
  "BQ72nSv9f3PRyRKCBnHLVrerrv37CYTHm5h3s9VSGQDV",
  "2MFoS3MPtvyQ4Wh4M9pdfPjz6UhVoNbFbGJAskCPCj3h",
  "9nnLbotNTcUhvbrsA6Mdkx45Sm82G35zo28AqUvjExn8",
  "6LXutJvKUw8Q5ue2gCgKHQdAN4suWW8awzFVC6XCguFx",
  "CapuXNQoDviLvU1PxFiizLgPNQCxrsag1uMeyk6zLVps",
  "7HkzG4LYyCJSrD3gopPQv3VVzQQKbHBZcm9fbjj5fuaH",
  "8psNvWTrdNTiVRNzAgsou9kETXNJm2SXZyaKuJraVRtf",
  "ZG98FUCjb8mJ824Gbs6RsgVmr1FhXb2oNiJHa2dwmPd",
  "FE9qe5NodLxD6aHwfAtNiY7cCbCG4znHVFGDnSSRpnS8"
)
#remove them
g_no_hubs <- igraph::delete_vertices(g, exclude_addresses)

#recalculate components
comps_no_hubs <- igraph::components(g_no_hubs)
V(g_no_hubs)$component <- comps_no_hubs$membership

#create new addy-component df
address_component_df_no_hubs <- data.frame(
  address = V(g_no_hubs)$name,
  component = V(g_no_hubs)$component,
  supply_pct = V(g_no_hubs)$supply_pct
)

#calculate degrees for all nodes
deg_no_hubs <- degree(g_no_hubs, mode = "all")
# Find the component number with the most wallets
largest_component <- address_component_df_no_hubs$component[which.max(table(address_component_df_no_hubs$component))]
addresses_in_largest <- address_component_df_no_hubs$address[address_component_df_no_hubs$component == largest_component]
#get degrees and sort
deg_largest <- deg_no_hubs[addresses_in_largest]
sorted_deg_largest <- sort(deg_largest, decreasing = TRUE)
print(head(sorted_deg_largest, 20))  # Top 20 by degree
#summarize supply per component (true clusters)
component_supply_summary_no_hubs <- address_component_df_no_hubs %>%
  group_by(component) %>%
  summarise(
    n_wallets = n(),
    total_supply_pct = sum(supply_pct, na.rm = TRUE)
  ) %>%
  arrange(desc(total_supply_pct))



# Top clusters by supply
print(component_supply_summary_no_hubs %>% head(10))

#extract addresses and supply
# Set the component number you want to analyze
component_number <- 17  # Change this to your component of interest

# Extract addresses and supply_pct for this component, sorted by supply descending
addresses_supply_in_component <- address_component_df_no_hubs %>%
  filter(component == component_number) %>%
  arrange(desc(supply_pct))

# Print the result
print(addresses_supply_in_component)


# Filter to keep only addresses with supply_pct >= 0.001 and not NA
filtered_addresses_supply <- address_component_df_no_hubs %>%
  filter(!is.na(supply_pct) & supply_pct >= 0.001) %>%
  arrange(component, desc(supply_pct))

# Print the result
print(filtered_addresses_supply)

##print top cluster by supply with filtered addresses that hold only
# Summarize total supply per component (cluster) using the filtered data
top_clusters_by_supply <- filtered_addresses_supply %>%
  group_by(component) %>%
  summarise(
    n_wallets = n(),
    total_supply_pct = sum(supply_pct, na.rm = TRUE)
  ) %>%
  arrange(desc(total_supply_pct)) %>%
  head(10)  # Change 10 to however many top clusters you want

print(top_clusters_by_supply)

#visualizing
top_clusters_by_supply <- top_clusters_by_supply %>%
  mutate(cluster_label = paste0("Cluster ", row_number()))


# Calculate total supply held by top clusters for subtitle
total_top_supply <- sum(top_clusters_by_supply$total_supply_pct)
subtitle_text <- sprintf("Top 10 clusters control %.2f%% of total supply", total_top_supply)

ggplot(top_clusters_by_supply, aes(x = reorder(cluster_label, -total_supply_pct), y = total_supply_pct)) +
  geom_bar(stat = "identity", fill = "#2C3E50", width = 0.7, alpha = 0.92) +
  geom_text(aes(label = paste0(sprintf("%.2f%%", total_supply_pct), "\n(", n_wallets, " wallets)")),
            vjust = -0.5, size = 5, fontface = "bold", color = "#34495E") +
  labs(
    title = "Major Early Buyer Clusters",
    subtitle = subtitle_text,
    x = NULL,
    y = "Total Supply (%)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(size = 15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

##individual wallet checker for cluster
# The address you want to check
query_address <- "8bEfPKLTs2dLKVA9REjKXcDysyzmZEUVaqyw6QHowub5"

# Check if the address is present and get its cluster/component info
address_info <- address_component_df_no_hubs %>%
  filter(address == query_address)

if (nrow(address_info) == 0) {
  cat("Address not found in any cluster.\n")
} else {
  cat("Address", query_address, "is in cluster (component):", address_info$component, "\n")
  cat("Current supply held:", address_info$supply_pct, "\n")
}
