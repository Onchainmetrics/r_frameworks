# Address Transfer Connection Checker
# Use this to investigate how a specific address got classified as insider/descendant

# Set the address you want to investigate
query_address <- "YOUR_ADDRESS_HERE"  # Replace with the actual address

cat("=== INVESTIGATING ADDRESS:", query_address, "===\n")

# 1. Check if address received transfers FROM any insiders
transfers_from_insiders <- transfers %>%
  filter(to_owner == query_address & from_owner %in% insider_addresses) %>%
  arrange(desc(block_time))

cat("\n--- TRANSFERS RECEIVED FROM INSIDERS ---\n")
if (nrow(transfers_from_insiders) > 0) {
  print(transfers_from_insiders %>% 
    select(from_owner, to_owner, amount, block_time) %>%
    mutate(from_first8 = substr(from_owner, 1, 8)))
} else {
  cat("No direct transfers from insiders found.\n")
}

# 2. Check if address sent transfers TO any insiders  
transfers_to_insiders <- transfers %>%
  filter(from_owner == query_address & to_owner %in% insider_addresses) %>%
  arrange(desc(block_time))

cat("\n--- TRANSFERS SENT TO INSIDERS ---\n")
if (nrow(transfers_to_insiders) > 0) {
  print(transfers_to_insiders %>% 
    select(from_owner, to_owner, amount, block_time) %>%
    mutate(to_first8 = substr(to_owner, 1, 8)))
} else {
  cat("No transfers to insiders found.\n")
}

# 3. Check ALL incoming transfers (to see full transfer history)
all_incoming <- transfers %>%
  filter(to_owner == query_address) %>%
  arrange(desc(block_time)) %>%
  head(20)  # Show last 20 transfers

cat("\n--- ALL INCOMING TRANSFERS (Last 20) ---\n")
if (nrow(all_incoming) > 0) {
  print(all_incoming %>% 
    select(from_owner, amount, block_time) %>%
    mutate(
      from_first8 = substr(from_owner, 1, 8),
      is_insider = from_owner %in% insider_addresses,
      is_descendant = from_owner %in% descendant_set_insider
    ))
} else {
  cat("No incoming transfers found.\n")
}

# 4. Check ALL outgoing transfers
all_outgoing <- transfers %>%
  filter(from_owner == query_address) %>%
  arrange(desc(block_time)) %>%
  head(20)

cat("\n--- ALL OUTGOING TRANSFERS (Last 20) ---\n")
if (nrow(all_outgoing) > 0) {
  print(all_outgoing %>% 
    select(to_owner, amount, block_time) %>%
    mutate(
      to_first8 = substr(to_owner, 1, 8),
      is_insider = to_owner %in% insider_addresses,
      is_descendant = to_owner %in% descendant_set_insider
    ))
} else {
  cat("No outgoing transfers found.\n")
}

# 5. Trace the descendant path (if it's classified as descendant)
if (query_address %in% descendant_set_insider && !query_address %in% insider_addresses) {
  cat("\n--- TRACING DESCENDANT PATH ---\n")
  cat("This address is classified as Insider Descendant.\n")
  cat("Tracing connection path to original insiders...\n")
  
  # Find direct connections to insiders
  direct_insider_connections <- transfers %>%
    filter(
      (to_owner == query_address & from_owner %in% insider_addresses) |
      (from_owner == query_address & to_owner %in% insider_addresses)
    )
  
  if (nrow(direct_insider_connections) > 0) {
    cat("DIRECT connection to insiders found:\n")
    print(direct_insider_connections %>%
      select(from_owner, to_owner, amount, block_time) %>%
      mutate(
        from_first8 = substr(from_owner, 1, 8),
        to_first8 = substr(to_owner, 1, 8)
      ))
  } else {
    cat("No DIRECT connection to insiders. Checking indirect path...\n")
    
    # Find who this address received from
    immediate_sources <- transfers %>%
      filter(to_owner == query_address) %>%
      pull(from_owner) %>%
      unique()
    
    # Check if any of these sources are descendants
    descendant_sources <- immediate_sources[immediate_sources %in% descendant_set_insider]
    
    if (length(descendant_sources) > 0) {
      cat("Received transfers from other descendants:\n")
      for (source in descendant_sources[1:5]) {  # Show first 5
        cat("  -", substr(source, 1, 8), "...\n")
      }
    }
  }
}

# 6. Classification summary
cat("\n--- CLASSIFICATION SUMMARY ---\n")
cat("Address:", query_address, "\n")
cat("Is Insider:", query_address %in% insider_addresses, "\n")
cat("Is Insider Descendant:", query_address %in% descendant_set_insider, "\n")
cat("Classification in dataset:", 
    ifelse(query_address %in% insider_addresses, "Original Insider",
    ifelse(query_address %in% descendant_set_insider, "Insider Descendant", "Late Buyer")), "\n")

# 7. Check current balance
current_bal <- current_balances %>%
  filter(address == query_address) %>%
  pull(current_balance)

if (length(current_bal) > 0) {
  supply_pct <- round(current_bal / 1e9 * 100, 6)  # Adjust 1e9 if different total supply
  cat("Current balance:", format(current_bal, scientific = FALSE), "\n")
  cat("Supply percentage:", supply_pct, "%\n")
} else {
  cat("Address not found in current balances.\n")
}


# ============================================================================
# === LATE BUYER CONNECTION CHECKER ===
# ============================================================================
# Use this section to investigate connections between late buyer addresses
# Prerequisites: 
# - Variable 'late_buyer_transfers' must exist (from late buyer clustering analysis)
# - Variable 'late_buyers' dataframe must exist
# - Variable 'g_late' (late buyer network graph) must exist

cat("\n\n=== LATE BUYER CONNECTION INVESTIGATION ===\n")

# Configuration: Set the late buyer address you want to investigate
late_buyer_query_address <- "YOUR_LATE_BUYER_ADDRESS_HERE"  # Replace with actual address

# Check if required data exists
if (!exists("late_buyer_transfers")) {
  cat("ERROR: Variable 'late_buyer_transfers' not found.\n")
  cat("Please run the late buyer clustering analysis first.\n")
} else if (!exists("late_buyers")) {
  cat("ERROR: Variable 'late_buyers' not found.\n") 
  cat("Please run the late buyer clustering analysis first.\n")
} else if (!exists("g_late")) {
  cat("ERROR: Variable 'g_late' not found.\n")
  cat("Please run the late buyer clustering analysis first.\n")
} else {
  
  cat("=== INVESTIGATING LATE BUYER ADDRESS:", late_buyer_query_address, "===\n")
  
  # Get all late buyer addresses for reference
  all_late_buyer_addresses <- unique(c(late_buyer_transfers$from, late_buyer_transfers$to))
  
  # Check if the query address is actually a late buyer
  if (!late_buyer_query_address %in% all_late_buyer_addresses) {
    cat("WARNING: Address not found in late buyer network.\n")
    cat("Make sure you're using the correct address and that it was part of the late buyer analysis.\n")
  } else {
    
    # 1. Check cluster assignment
    if (exists("g_late")) {
      if (late_buyer_query_address %in% V(g_late)$name) {
        cluster_id <- V(g_late)$cluster[V(g_late)$name == late_buyer_query_address]
        supply_pct <- V(g_late)$supply_pct[V(g_late)$name == late_buyer_query_address]
        
        cat("\n--- CLUSTER INFORMATION ---\n")
        cat("Cluster ID:", cluster_id, "\n")
        cat("Supply percentage:", round(supply_pct, 6), "%\n")
        
        # Get other members of the same cluster
        cluster_members <- V(g_late)$name[V(g_late)$cluster == cluster_id]
        cat("Cluster size:", length(cluster_members), "addresses\n")
        
        if (length(cluster_members) > 1) {
          cat("Other cluster members:\n")
          other_members <- cluster_members[cluster_members != late_buyer_query_address]
          for (i in seq_along(other_members)) {
            if (i <= 5) {  # Show first 5 other members
              member_supply <- V(g_late)$supply_pct[V(g_late)$name == other_members[i]]
              cat("  -", substr(other_members[i], 1, 8), "... (", round(member_supply, 4), "%)\n")
            }
          }
          if (length(other_members) > 5) {
            cat("  ... and", length(other_members) - 5, "more\n")
          }
        }
      }
    }
    
    # 2. Check direct transfers RECEIVED from other late buyers
    transfers_from_late_buyers <- late_buyer_transfers %>%
      filter(to == late_buyer_query_address) %>%
      arrange(desc(weight))
    
    cat("\n--- TRANSFERS RECEIVED FROM OTHER LATE BUYERS ---\n")
    if (nrow(transfers_from_late_buyers) > 0) {
      cat("Found", nrow(transfers_from_late_buyers), "transfer connections from other late buyers:\n")
      print(transfers_from_late_buyers %>%
        mutate(from_first8 = substr(from, 1, 8)) %>%
        select(from_first8, weight) %>%
        rename(sender = from_first8, transfer_count = weight))
    } else {
      cat("No transfer connections received from other late buyers.\n")
    }
    
    # 3. Check direct transfers SENT to other late buyers
    transfers_to_late_buyers <- late_buyer_transfers %>%
      filter(from == late_buyer_query_address) %>%
      arrange(desc(weight))
    
    cat("\n--- TRANSFERS SENT TO OTHER LATE BUYERS ---\n")
    if (nrow(transfers_to_late_buyers) > 0) {
      cat("Found", nrow(transfers_to_late_buyers), "transfer connections to other late buyers:\n")
      print(transfers_to_late_buyers %>%
        mutate(to_first8 = substr(to, 1, 8)) %>%
        select(to_first8, weight) %>%
        rename(recipient = to_first8, transfer_count = weight))
    } else {
      cat("No transfer connections sent to other late buyers.\n")
    }
    
    # 4. Connection strength analysis
    all_connections <- rbind(
      transfers_from_late_buyers %>% mutate(direction = "received_from", connected_address = from),
      transfers_to_late_buyers %>% mutate(direction = "sent_to", connected_address = to)
    )
    
    if (nrow(all_connections) > 0) {
      cat("\n--- CONNECTION STRENGTH ANALYSIS ---\n")
      
      # Find strongest connections
      strongest_connections <- all_connections %>%
        group_by(connected_address) %>%
        summarise(
          total_transfers = sum(weight),
          directions = paste(unique(direction), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(total_transfers)) %>%
        head(10)
      
      cat("Top connected late buyer addresses:\n")
      print(strongest_connections %>%
        mutate(address_first8 = substr(connected_address, 1, 8)) %>%
        select(address_first8, total_transfers, directions))
      
      # Check if connected addresses are in the same cluster
      if (exists("g_late")) {
        same_cluster_connections <- strongest_connections %>%
          mutate(
            their_cluster = sapply(connected_address, function(addr) {
              if (addr %in% V(g_late)$name) {
                V(g_late)$cluster[V(g_late)$name == addr]
              } else {
                NA
              }
            }),
            same_cluster = their_cluster == cluster_id
          ) %>%
          filter(!is.na(their_cluster))
        
        if (nrow(same_cluster_connections) > 0) {
          cat("\nCluster relationship analysis:\n")
          same_cluster_count <- sum(same_cluster_connections$same_cluster, na.rm = TRUE)
          different_cluster_count <- sum(!same_cluster_connections$same_cluster, na.rm = TRUE)
          
          cat("Connected addresses in SAME cluster:", same_cluster_count, "\n")
          cat("Connected addresses in DIFFERENT clusters:", different_cluster_count, "\n")
          
          if (same_cluster_count > 0) {
            cat("Same cluster connections:\n")
            same_cluster_addrs <- same_cluster_connections %>%
              filter(same_cluster) %>%
              mutate(address_first8 = substr(connected_address, 1, 8))
            print(same_cluster_addrs %>% select(address_first8, total_transfers))
          }
        }
      }
    }
    
    # 5. Network position analysis
    cat("\n--- NETWORK POSITION ANALYSIS ---\n")
    
    # Count total connections
    total_connections <- nrow(all_connections)
    unique_connections <- length(unique(all_connections$connected_address))
    
    cat("Total transfer relationships:", total_connections, "\n")
    cat("Unique connected addresses:", unique_connections, "\n")
    
    if (unique_connections > 0) {
      # Calculate connection diversity
      avg_transfers_per_connection <- round(mean(all_connections$weight), 2)
      max_transfers_to_one_address <- max(all_connections$weight)
      
      cat("Average transfers per connection:", avg_transfers_per_connection, "\n")
      cat("Maximum transfers to one address:", max_transfers_to_one_address, "\n")
      
      # Assess centrality
      if (unique_connections >= 5) {
        cat("Network position: HIGHLY CONNECTED (hub-like behavior)\n")
      } else if (unique_connections >= 2) {
        cat("Network position: MODERATELY CONNECTED\n")
      } else {
        cat("Network position: MINIMALLY CONNECTED\n")
      }
    }
    
    # 6. Current holdings comparison with cluster
    late_buyer_info <- late_buyers %>%
      filter(address == late_buyer_query_address)
    
    cat("\n--- HOLDINGS COMPARISON ---\n")
    if (nrow(late_buyer_info) > 0) {
      cat("Current balance:", format(late_buyer_info$current_balance, scientific = FALSE), "\n")
      cat("Supply percentage:", round(late_buyer_info$supply_pct, 6), "%\n")
      
      # Compare with cluster averages if in a cluster
      if (exists("cluster_id") && !is.na(cluster_id)) {
        cluster_addresses <- V(g_late)$name[V(g_late)$cluster == cluster_id]
        cluster_supplies <- V(g_late)$supply_pct[V(g_late)$cluster == cluster_id]
        
        cluster_avg_supply <- mean(cluster_supplies, na.rm = TRUE)
        cluster_total_supply <- sum(cluster_supplies, na.rm = TRUE)
        
        cat("Cluster average supply per address:", round(cluster_avg_supply, 6), "%\n")
        cat("Total cluster supply:", round(cluster_total_supply, 4), "%\n")
        cat("This address vs cluster average:", 
            round((late_buyer_info$supply_pct / cluster_avg_supply), 2), "x\n")
      }
    } else {
      cat("Address not found in late buyers dataset.\n")
    }
    
    # 7. Investigation summary
    cat("\n--- INVESTIGATION SUMMARY ---\n")
    cat("Address:", late_buyer_query_address, "\n")
    cat("Classification: Late Buyer\n")
    if (exists("cluster_id")) {
      cat("Cluster assignment:", cluster_id, "\n")
    }
    cat("Transfer connections to other late buyers:", unique_connections, "\n")
    
    if (unique_connections > 0) {
      cat("Most connected to:", substr(strongest_connections$connected_address[1], 1, 8), 
          "... (", strongest_connections$total_transfers[1], "transfers)\n")
    }
    
    cat("\nThis analysis helps understand:\n")
    cat("- Which late buyer cluster this address belongs to\n")
    cat("- How connected it is to other late buyers\n") 
    cat("- Whether connections are mainly within-cluster or cross-cluster\n")
    cat("- Its relative importance within the late buyer network\n")
  }
}