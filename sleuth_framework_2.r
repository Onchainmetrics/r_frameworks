# --- 
title: "improved_sleuth_fixed"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

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
# --- 2. Load Data ---
setwd("C:/Users/Nitropc/Downloads")


early_buyers_df <- fread("TROLL_insiders.csv")
early_buyers <- trimws(early_buyers_df$address)
early_buyers_final <- early_buyers_df

insiders_df <- fread("TROLL_insiders.csv")
insider_addresses <- trimws(insiders_df$address)
insider_addresses <- insider_addresses[insider_addresses %in% early_buyers]
current_balances <- fread("TROLL_all_balances.csv")
current_balances$address <- trimws(current_balances$address)

# --- 2. Exclude AMMs, contracts, etc. ---
exclude_addresses <- c(
  "BAX9M9a5FVy5cNiewwnuwkVDzhSg9psZnb4fJ9r677tN",
  "5Q544fKrFoe6tsEbD7S8EmxGTJYAKtTVhAW5Q5pge4j1",
  "67C4rdUriP9EFbUo7CeoiFhM52Jgu9LZpe37Jk2k1tHZ",
  "6TYDxGmVxkBPBmEfnmLXx6jVff9LknsjRHqdTjVyZmG8",
  "HLnpSz9h2S4hiLQ43rnSD9XkcUThA7B8hQMKmDaiTLcC",
  "36m3V3vggs2peoZhn9xz2XoJv6YNX1pFKfag9aRZDnqN",
  "6DemDwdCaLuuHdBfbojEwxNScQGEYY6p2Ux4TEkPBfyx",
  "8FM9Cd28PNFaHTJiEQE96HCgfyQZ2E3kk3rEvVtenZE",
  "DDFfKazbGVgXsd4FvAzYiEftvmBMThG7ZPorQofE4bEb",
  "6qs6RBvKxaw1ncWrXyiZt4RCju8w2RWe3vY1WFVK5wuH",
  "BQ72nSv9f3PRyRKCBnHLVrerrv37CYTHm5h3s9VSGQDV",
  "GGztQqQ6pCPaJQnNpXBgELr5cs3WwDakRbh1iEMzjgSJ",
  "4xDsmeTWPNjgSVSS1VTfzFq3iHZhp77ffPkAmkZkdu71",
  "6U91aKa8pmMxkJwBCfPTmUEfZi6dHe7DcFq2ALvB2tbB",
  "BQ72nSv9f3PRyRKCBnHLVrerrv37CYTHm5VSGQDV",
  "2MFoS3MPtvyQ4Wh4M9pdfPjz6UhVoNbFbGJAskCPCj3h",
  "9nnLbotNTcUhvbrsA6Mdkx45Sm82G35zo28AqUvjExn8",
  "EaEnFcLt7vbxNorTsKbChqXjRicsAh8SD8aZZ47tGnKp",
  "6LXutJvKUw8Q5ue2gCgKHQdAN4suWW8awzFVC6XCguFx",
  "CapuXNQoDviLvU1PxFiVRNzAgsou9kETXNJm2SXZyaKuJraVRtf",
  "7HkzG4LYyCJSrD3gopPQv3VVzQQKbHBZcm9fbjj5fuaH",
  "8psNvWTrdNTiVRNzAgsou9kETXNJm2SXZyaKuJraVRtf",
  "ZG98FUCjb8mJ824Gbs6RsgVmr1FhXb2oNiJHa2dwmPd",
  "FE9qe5NodLxD6aHwfAtNiY7cCbCG4znHVFGDnSSRpnS8",
  "HV1KXxWFaSeriyFvXyx48FqG9BoFbfinB8njCJonqP7K",
  "8qivNVDwkEXceEYCf97tr2RGUv9PSgsMGxiYp7yh5jHx",
  "7dGrdJRYtsNR8UYxZ3TnifXGjGc9eRYLq9sELwYpuuUu",
  "AorF9MEjaEhKHnSH4P972L3PBpbC6q39k3C4oJkJAE5v",
  "AJnaak1zJHhsnYq3WNzGJQGphBUB7jPW99XRRJJryDex5",
  "BgQuQf1xNoiHsLYJr3dUWLf7DEe4ANvbcW5fty7oSRtJ",
  "j1oxqtEHFn7rUkdABJLmtVtz5fFmHFs4tCG3fWJnkHX",
  "GpMZbSM2GgvTKHJirzeGfMFoaZ8UR2X7F4v8vHTvxFbL",
  "2QfBNK2WDwSLoUQRb1zAnp3KM12N9hQ8q6ApwUMnWW2T",
  "8ztFxjFPfVUtEf4SLSapcFj8GW2dxyUA9no2bLPq7H7V",
  "BBXyTX5UfbASibLRo3iaptuwF5846njxm7M4xFQTQz3d",
  "4w2cysotX6czaUGmmWg13hDpY4QEMG2CzeKYEQyK9Ama",
  "C7hyRzjt3b5n6qDQW9VzpGQdTH49QW7BJKMaZefZqBFa",
  "5asKWJfxCYEGyC8Yocyi9KGVQyhi8rmTbzP72pgajUnF",
  "9v2bZjjz981ojybxtem7LdMb7aiygeVx4nLjtydyztvG",
  "5YET3YapxD6to6rqPqTWB3R9pSbURy6yduuUtoZkzoPX",
  "CZiW56PZLNetanRu9kfCvxL2qS6KvKENWAYSZTaVb9vV",
  "HsQGWEh3ib6w59rBh5n1jXmi8VXFBqKEjxozL6PGfcgb",
  "8pY1AukbuPgUE3EetyLa59rFLMimJGT94ZzbMEZcQF4w",
  "3s31FNGZCiNsavo5wWEd32mmMUg5EJ2TZJ5vyKhyYgmX",
  "ER9qRhHSysuY1yk9GjogNyomFGebf7Zf2bEp3k9hVqb4",
  "GTvLGt46BqXi1v6cvSivXEybPAKBEoSGnLKDLZC8Q6SZ",
  "A7FMMgue4aZmPLLoutVtbC7gJcyqkHybUieiaDg9aaVE",
  "WLHv2UAZm6z4KyaaELi5pjdbJh6RESMva1Rnn8pJVVh",
  "GF9MHYopv9L4jp9MgFG35Mht8Ucq4vpxDmSFNtw4dgJZ",
  "Fvh8z34VTK9hyvbkbaBQ8hmrzEAvfYjy4ev4oU6WA9Jn",
  "Cgnuirsk5dQ9Ka1Grnru7J8YW1sYncYUjiXvYxT7G4iZ",
  "DeNFmyYsMPyAcZx4NChYaU68zcqBQZuntQHwEyBrFE7f",
  "H5sVUKA5jqTyBCVC3BvVTE7s72MSmdf75hCKHeKpaH5T",
  "932fX4SwBirUHzUzjHkE8AmXibxJiJ9xCBmwRsZYWEdd",
  "6yUh6uJ7UpCGQXSXkrgQyFLDEcs61YXabVar34ia4p1L",
  "888DUdR3P4fYa4Es5AVvJhoqwQGhFV56NotdHt2gz3e1",
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
  "38K48AddR83Bdbm94H2pASyX3vQCh4QX7NoGRP8ZmWp4",
  "GCvHhEUYQwTJ8jyf8Lc4bv8jBXyZU4LMMsZCobwEPzvM",
  "CKBCxNxdsfZwTwKYHQmBs7J8zpPjCjMJAxcxoBUwExw6",
  "Dw95gaRQtSo1yzCy8DjUjmnXoRthHWa6Rj3NX17dUuoV",
  "CufrWmkXfz1kdbmZcWgH8V7doLXHHx88eCjs3djbXzDs",
  "Eo8ttYZZeBqHP7fmBdMgbquPJ4SP4WSY2755QT9Z7pqX",
  "5Dw9fqeipoVpU3wLRVgD23JGkRBstoiz1oaP6WbYWGG8",
  "EHhnWUe656du4Dit8esnkhNRJNdXKnZqZjs2ZgvgP7T1",
  "3LkGTjNsF2zWc2ddBPHYyEJJZKqbdHJDgfjztxnjwL5R",
  "rm4cojNDeUfFQZBMVUSTuxFn15dpAmygaaqyaaWqbUC",
  "9PHm2cYU8DhwBrbRsqqAjhW9uXVrNR1RaLsvo9oGVeaq",
  "FhRiiVdT5Q2NDb6szFyETdyZK9Ui2e84Mi9qPNhV8tTK",
  "J2G9Md8VfF8ZYRiiu5BwRhfa4XU5P6mZotFH1f9DCdu7",
  "CapuXNQoDviLvU1PxFiizLgPNQCxrsag1uMeyk6zLVps",
  "Fztu3B6v23wakKu3FwNfJrYk8KZQkoNZMscExHrN4dpt",
  "ChSYVaJnSYQaBKJw7eKKFNfe3yb93VeRJ38yD6BYKxoF"
  )
current_balances <- current_balances %>%
  filter(!address %in% exclude_addresses)

# --- 3. Load Transfers ---
transfers <- fread("TROLL_all_transfers.csv")
transfers <- transfers[from_owner != "" & to_owner != ""]
transfers <- unique(transfers, by = c("from_owner", "to_owner", "amount", "block_time"))
transfers$from_owner <- trimws(transfers$from_owner)
transfers$to_owner <- trimws(transfers$to_owner)

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
    total_supply = sum(current_balance),
    .groups = "drop"
  ) %>%
  mutate(
    supply_pct = total_supply / sum(total_supply) * 100,
    wallet_pct = wallets / sum(wallets) * 100
  )

summary_table_insider <- current_balances_insider %>%
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



# --- 8. Print Results ---
print(summary_table_early)
print(summary_table_insider)

cat("Total wallets (Early Buyer cohort):", sum(summary_table_early$wallets), "\n")
cat("Total wallets (Insider cohort):", sum(summary_table_insider$wallets), "\n")

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
    title = "TROLL Supply Distribution by Group",
    subtitle = "Insiders who bought at 20-70k in a 2 hour period.\nA Descendant is a wallet linked to an Insider.",
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
  
# Exclude known hubs from both 'from' and 'to' columns
edge_list <- edge_list %>%
  filter(!from %in% exclude_addresses & !to %in% exclude_addresses)

# --- 2. Build igraph Object ---
g <- graph_from_data_frame(edge_list, directed = TRUE)

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

# --- 9. Filter for Significant Holders ---
filtered_addresses_supply <- address_component_df_no_hubs %>%
  filter(!is.na(supply_pct) & supply_pct >= 0.001) %>%
  arrange(component, desc(supply_pct))




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

# Create individual summaries (for addresses not in clusters)
individual_summaries <- all_insider_descendant_holdings %>%
  filter(address %in% individual_addresses) %>%
  mutate(
    entity_id = address,
    entity_name = paste0("Individual: ", substr(address, 1, 8), "..."),
    n_wallets = 1,
    total_supply_pct = supply_pct,
    type = "Individual"
  ) %>%
  select(entity_id, entity_name, n_wallets, total_supply_pct, type)

# Combine clusters and individuals, then filter for significant entities
min_entity_supply_threshold <- 0.001  # Minimum 0.001% supply to show in visualization

# Combine clusters and individuals, then get top 20
combined_entities <- bind_rows(
  cluster_summaries %>% select(entity_id, entity_name, n_wallets, total_supply_pct, type),
  individual_summaries
) %>%
  filter(total_supply_pct >= min_entity_supply_threshold) %>%  # Filter out tiny entities
  arrange(desc(total_supply_pct))

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
query_address <- "9qpucJgTkTVSbMPksG4fequu9STgFxr9AQmLN25MwiBV"
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
component_number <- 5  # Change this to your component of interest

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
insider_wallets_comprehensive <- current_balances_insider %>%
  filter(group %in% c("Insider", "Insider Descendant")) %>%
  select(address, current_balance, group) %>%
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

# --- FILTER FOR SIGNIFICANT HOLDERS BEFORE EXPORT ---
# Remove addresses with very small current holdings
significant_threshold <- 0.001  # 0.001% of supply minimum

insider_wallets_comprehensive <- insider_wallets_comprehensive %>%
  filter(supply_pct >= significant_threshold)

cat("Filtered out wallets below", significant_threshold, "% supply threshold\n")
cat("Remaining significant insider addresses:", nrow(insider_wallets_comprehensive), "\n")

# Export single comprehensive list with all metadata
write_csv(insider_wallets_comprehensive, "TROLL_insider_wallets_comprehensive.csv")

# Print summary of export
cat("\n=== EXPORT SUMMARY ===\n")
export_summary <- insider_wallets_comprehensive %>%
  group_by(classification, wallet_type) %>%
  summarise(
    count = n(),
    total_supply = sum(supply_pct),
    .groups = "drop"
  )
print(export_summary)

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat("Total clustered addresses:", length(clustered_addresses), "\n")
cat("Total individual addresses:", length(individual_addresses), "\n")
cat("Top individual holder supply %:", max(individual_summaries$total_supply_pct, na.rm = TRUE), "\n")
cat("Top cluster supply %:", max(cluster_summaries$total_supply_pct, na.rm = TRUE), "\n")


# ===============================================================================
# LATE BUYER CLUSTERING SECTION
# ===============================================================================

cat("\n=== STARTING LATE BUYER CLUSTER ANALYSIS ===\n")

# --- 1. Extract Late Buyers ---
late_buyers <- current_balances_insider %>%
  filter(group == "Late Buyer") %>%
  arrange(desc(current_balance)) %>%
  mutate(
    supply_pct = round(current_balance / sum(current_balances$current_balance) * 100, 6)
  ) %>%
  filter(supply_pct >= 0.001) %>%  # Only significant holders
  as_tibble()

cat("Total Late Buyers with significant holdings (>0.001%):", nrow(late_buyers), "\n")
cat("Late Buyer supply coverage:", round(sum(late_buyers$supply_pct), 2), "%\n")

# --- 2. Build Late Buyer Transfer Network ---
# Find transfers between late buyers
late_buyer_addresses <- late_buyers$address

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
  late_buyer_supply_map <- setNames(late_buyers$supply_pct, late_buyers$address)
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
  
  late_individual_entities <- late_buyers %>%
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
  
  # --- 10. Export Late Buyer Cluster Data ---
  late_cluster_mapping <- data.frame(
    address = V(g_late)$name,
    late_cluster_id = V(g_late)$cluster
  )
  
  late_buyer_comprehensive <- late_buyers %>%
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
  
  # Still create individual late buyer analysis
  late_individual_entities <- late_buyers %>%
    head(15) %>%
    mutate(
      entity_name = paste0("Late Individual: ", substr(address, 1, 8), "...")
    )
  
  print("=== TOP 15 INDIVIDUAL LATE BUYERS ===")
  print(late_individual_entities %>% select(entity_name, supply_pct, current_balance))
  
  # Export individual late buyers
  late_buyer_comprehensive <- late_buyers %>%
    mutate(
      classification = "Individual",
      late_cluster_id = "N/A",
      wallet_type = "Late_Buyer"
    ) %>%
    select(address, wallet_type, current_balance, supply_pct, classification, late_cluster_id)
  
  write_csv(late_buyer_comprehensive, "late_buyer_clusters_comprehensive.csv")
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
late_cluster_number <- 14  # Change this to your cluster of interest

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

if (!exists("late_buyers")) {
  stop("Required variable 'late_buyers' not found. Please run the main late buyer clustering analysis first.")
}

# Extract addresses and supply_pct for the specified late buyer cluster, sorted by supply descending
late_addresses_supply_in_cluster <- data.frame(
  address = V(g_late)$name,
  cluster = V(g_late)$cluster,
  supply_pct = V(g_late)$supply_pct
) %>%
  filter(cluster == late_cluster_number) %>%
  left_join(late_buyers %>% select(address, current_balance), by = "address") %>%
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
  
  


##----- INSIDER CSV MEGA SECTION
# --- Insider Activity Analysis ---
# Analyzing patterns in clusters vs individuals and their trading behavior

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
    subtitle = "Showing trading behavior, supply percentage held and wallet count",
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
  scale_fill_manual(values = behavior_colors, name = "Dominant Behavior") +
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

# --- 10. Distribution Analysis ---
p5 <- activity_data %>%
  filter(current_holding_usd > 0) %>%
  ggplot(aes(x = current_holding_usd, fill = entity_type)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_fill_manual(values = c("Individual" = "#E74C3C", "Cluster" = "#3498DB")) +
  labs(
    title = "Distribution of Current Holdings",
    subtitle = "Log scale - USD value of current token holdings",
    x = "Current Holding Value (USD, log scale)",
    y = "Count",
    fill = "Entity Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "top"
  )

print(p5)

# --- 11. Export Summary Tables ---
write_csv(behavior_summary, "behavior_analysis_summary.csv")
write_csv(cluster_analysis, "top_clusters_analysis.csv")
write_csv(entity_comparison, "individuals_vs_clusters_comparison.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Summary files exported:\n")
cat("- behavior_analysis_summary.csv\n")
cat("- top_clusters_analysis.csv\n") 
cat("- individuals_vs_clusters_comparison.csv\n")
