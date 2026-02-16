# ==========================================
# Social Network Size (Local): K-Neighborhood

# Required Helper(s)
# - Directed/Undirected Adjacency List Builder (refer to adjacency_list.R)
# Functions: one_neighborhood, two_neighborhood

# Output: 
# - one_neighborhood: k1_in, k1_out, k1_all, k1_un
# - two_neighborhood: k2_in, k2_out, k2_all, k2_un
# ==========================================

# ------------------------------------------
# 1. One neighborhood
# step 1. get directed/undirected adjacency list
# step 2. count number of friends of each ego
# ------------------------------------------

one_neighborhood <- function(edges,
                             from = "from",
                             to   = "to",
                             nodes = NULL,
                             drop_self_loops = TRUE) {
  g <- adj_builder(edges, from, to, nodes, drop_self_loops)
  
  data.frame(
    nid    = g$nodes,
    k1_in  = as.integer(lengths(g$adj_in)),
    k1_out = as.integer(lengths(g$adj_out)),
    k1_all = as.integer(lengths(g$adj_all)),
    k1_un  = as.integer(lengths(g$adj_un)),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------
# 2. Two neighborhood
# step 1. get directed/undirected adjacency list
# step 2. count number of friends of each ego
# step 3. count number of friends' friends using 2-step counter
# ------------------------------------------

two_neighborhood <- function(edges,
                             from = "from",
                             to   = "to",
                             nodes = NULL,
                             drop_self_loops = TRUE) {
  g <- adj_builder(edges, from, to, nodes, drop_self_loops)
  

  step_length2 <- function(adj_list, i) {
    n1 <- adj_list[[i]] #i's 1-step neighbors
    if (length(n1) == 0L) return(character(0))
    
    n2_list <- adj_list[g$idx[n1]] #i's 2-step neighbors
    acc <- unique(c(n1, unlist(n2_list, use.names = FALSE)))
    
    acc[acc != g$nodes[i]] # Ego 제외
  }
  
  data.frame(
    nid    = g$nodes,
    k2_in  = as.integer(sapply(seq_along(g$nodes), function(i) length(step_length2(g$adj_in, i)))),
    k2_out = as.integer(sapply(seq_along(g$nodes), function(i) length(step_length2(g$adj_out, i)))),
    k2_all = as.integer(sapply(seq_along(g$nodes), function(i) length(step_length2(g$adj_all, i)))),
    k2_un  = as.integer(sapply(seq_along(g$nodes), function(i) length(step_length2(g$adj_un, i)))),
    stringsAsFactors = FALSE
  )
}