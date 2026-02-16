# ==========================================
# Graph Builder
# Prepares graphs to be used in social network variable computations

# Required Helper(s)
# - Directed/Undirected Adjacency List Builder (refer to adjacency_list.R)

# Function: graph_builder
# Output: strength_in, strength_out, strength_all
# ==========================================
graph_builder <- function(edges, from = "from", to = "to", 
                       nodes = NULL, drop_self_loops = TRUE, 
                       weight_col = NULL) {
  
  u <- as.character(edges[[from]])
  v <- as.character(edges[[to]])
  ok <- !is.na(u) & !is.na(v)
  edges <- edges[ok, ]
  
  if (drop_self_loops) {
    edges <- edges[edges[[from]] != edges[[to]], ]
  }
  
  # create adjacency list (using adj_builder)
  g <- adj_builder(edges, from, to, nodes, drop_self_loops = FALSE)
  
  # add information about weights
  if (!is.null(weight_col)) {
    w <- as.numeric(edges[[weight_col]])
    
    accum <- function(keys, vals, universe) {
      out <- setNames(numeric(length(universe)), universe)
      if (length(keys) == 0) return(out)
      s <- tapply(vals, keys, sum)
      keep <- intersect(names(s), universe)
      out[keep] <- as.numeric(s[keep])
      out
    }
    
    g$strength_out <- accum(edges[[from]], w, g$nodes)
    g$strength_in  <- accum(edges[[to]], w, g$nodes)
    g$strength_all <- g$strength_out + g$strength_in
  }
  
  return(g)
}