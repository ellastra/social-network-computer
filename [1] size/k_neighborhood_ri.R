# ==========================================
# Social Network Size (Local): K-Neighborhood with different rhees

# Required Helper(s)
# - Graph Builder (refer to graph_processing.R)
# - Rhee mapper (included in this script)
# - Rhee counter (included in this script)
# Functions: ri_mapper, ri_counter, one_neighborhood_ri, two_neighborhood_ri

# Output: 
# - one_neighborhood: k1_ri_in, k1_ri_out, k1_ri_all, k1_ri_un
# - two_neighborhood: k2_ri_in, k2_ri_out, k2_ri_all, k2_ri_un
# ==========================================

# ------------------------------------------
# [helper] ri_mapper: maps rhee to each corresponding node
# ------------------------------------------
ri_mapper <- function(nodes_df, nid_col = "nid", ri_col = "n_ri2_") {
  v <- nodes_df[[ri_col]]
  names(v) <- as.character(nodes_df[[nid_col]])
  v
}

ri_map <- ri_mapper(nodes_df, nid_col = "nid", ri_col = "n_ri2_2021")

# ------------------------------------------
# [helper] ri_counter: counts number of different rhees in a group
# ------------------------------------------
ri_counter <- function(node_ids, ri_map, include_na = FALSE) {
  x <- ri_map[node_ids]
  if (!include_na) x <- x[!is.na(x)]
  length(unique(x))
}

# ------------------------------------------
# 1. One neighborhood for rhees
# step 1. access precomputed adjacency lists from g
# step 2. count number of different rhees within friends of each ego
# ------------------------------------------

one_neighborhood_ri <- function(g, ri_map, include_self_ri = FALSE, include_na = FALSE) {
  
  calc <- function(adj_list) {
    sapply(seq_along(g$nodes), function(i) {
      nb <- adj_list[[i]]
      if (include_self_ri) nb <- c(nb, g$nodes[i])
      ri_counter(nb, ri_map, include_na)
    })
  }
  
  data.frame(
    nid       = g$nodes,
    k1_ri_in  = as.integer(calc(g$adj_in)),
    k1_ri_out = as.integer(calc(g$adj_out)),
    k1_ri_all = as.integer(calc(g$adj_all)),
    k1_ri_un  = as.integer(calc(g$adj_un)),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------
# 2. Two neighborhood for rhees
# step 1. access precomputed adjacency lists from g
# step 2. count number of differnt rhees within friends of each ego
# step 3. count number of differnt rhees within friends' friends using 2-step counter
# ------------------------------------------

two_neighborhood_ri <- function(g, ri_map, include_self_ri = FALSE, include_na = FALSE) {
  
  step_length2 <- function(adj_list, i) {
    n1 <- adj_list[[i]] #i's 1-step neighbors
    if (length(n1) == 0L) return(character(0))
    
    n2_list <- adj_list[g$idx[n1]] #i's 2-step neighbors
    acc <- unique(c(n1, unlist(n2_list, use.names = FALSE)))
    
    acc[acc != g$nodes[i]] 
  }
  
  calc <- function(adj_list) {
    sapply(seq_along(g$nodes), function(i) {
      nb <- step_length2(adj_list, i)
      if (include_self_ri) nb <- c(nb, g$nodes[i])
      ri_counter(nb, ri_map, include_na)
    })
  }
  
  data.frame(
    nid       = g$nodes,
    k2_ri_in  = as.integer(calc(g$adj_in)),
    k2_ri_out = as.integer(calc(g$adj_out)),
    k2_ri_all = as.integer(calc(g$adj_all)),
    k2_ri_un  = as.integer(calc(g$adj_un)),
    stringsAsFactors = FALSE
  )
}