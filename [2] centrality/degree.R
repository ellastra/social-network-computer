# ==========================================
# Social Network Centrality: Degree centrality

# Required Helper(s)
# - Graph Builder (refer to graph_processing.R)
# Functions: degree

# Output: 
# - degree: deg_in, deg_out, deg_all, deg_un
# ==========================================

# ------------------------------------------
# 1. Degree centrality
# step 1. access precomputed adjacency lists or strengths from g
# step 2. count number of ties of each ego
# step 3. normalize (divide by (n-1))
# ------------------------------------------

degree <- function(g, normalized = TRUE) {
  
  # Choose strength if weights exist
  if (!is.null(g$strength_out)) {
    deg_in  <- g$strength_in
    deg_out <- g$strength_out
    deg_all <- g$strength_all
    deg_un  <- g$strength_all 
  } else {
    deg_in  <- lengths(g$adj_in)
    deg_out <- lengths(g$adj_out)
    deg_all <- lengths(g$adj_all)
    deg_un  <- lengths(g$adj_un)
  }
  
  # Normalization
  if (normalized) {
    n <- length(g$nodes)
    if (n > 1) {
      deg_in  <- deg_in / (n - 1)
      deg_out <- deg_out / (n - 1)
      deg_all <- deg_all / (n - 1)
      deg_un  <- deg_un / (n - 1)
    }
  }
  
  data.frame(
    nid     = g$nodes,
    deg_in  = as.numeric(deg_in),
    deg_out = as.numeric(deg_out),
    deg_all = as.numeric(deg_all),
    deg_un  = as.numeric(deg_un),
    stringsAsFactors = FALSE
  )
}