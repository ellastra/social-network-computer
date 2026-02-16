# ==========================================
# Social Network Size (Local): K-Neighborhood

# Required Helper: Directed/Undirected Adjacency List Builder (refer to adjacency_list.R)
# Functions: one_neighborhood, two_neighborhood

# Variables: 
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
  g <- build_adjacency(edges, from, to, nodes, drop_self_loops)
  
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
# step 3. count number of friends' friends using length 2 breath-first search
# ------------------------------------------

two_neighborhood <- function(edges,
                             from = "from",
                             to   = "to",
                             nodes = NULL,
                             drop_self_loops = TRUE) {
  g <- build_adjacency(edges, from, to, nodes, drop_self_loops)
  
  step_length2 <- function(adj_list) {
    n <- length(g$nodes)
    out <- integer(n)
    
    for (i in seq_len(n)) {
      n1 <- adj_list[[i]] #i's 1-step neighbors
      if (length(n1) == 0L) {
        out[i] <- 0L
        next
      }
      
      n2 <- n1
      for (v in n1) {
        j <- g$idx[[v]] #v's index
        if (!is.null(j) && !is.na(j)) {
          nb <- adj_list[[j]] #v's 1-step neighbors (i's 2-step neighbors)
          if (length(nb) > 0L) n2 <- c(n2, nb)
        }
      }
      
      u <- unique(n2)
      u <- u[u != g$nodes[i]]
      out[i] <- length(u)
    }
    out
  }
  
  data.frame(
    nid    = g$nodes,
    k2_in  = as.integer(step_length2(g$adj_in)),
    k2_out = as.integer(step_length2(g$adj_out)),
    k2_all = as.integer(step_length2(g$adj_all)),
    k2_un  = as.integer(step_length2(g$adj_un)),
    stringsAsFactors = FALSE
  )
}