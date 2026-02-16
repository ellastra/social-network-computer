# ==========================================
# Adjacency List Builder

# Creates Directed/Undirected Adjacency List

# Function: adj_builder
# Output: adj_in, adj_out, adj_all, adj_un
# ==========================================

adj_builder <- function(edges,
                            from = "from",
                            to   = "to",
                            nodes = NULL,
                            drop_self_loops = TRUE) {
  stopifnot(is.data.frame(edges))
  if (!all(c(from, to) %in% names(edges))) {
    stop(sprintf("edges must contain columns '%s' and '%s'", from, to))
  }
  
  f <- as.character(edges[[from]])
  t <- as.character(edges[[to]])
  
  if (drop_self_loops) {
    keep <- f != t
    f <- f[keep]
    t <- t[keep]
  }
  
  if (is.null(nodes)) {
    nodes <- sort(unique(c(f, t)))
  } else {
    nodes <- as.character(nodes)
  }
  
  n   <- length(nodes)
  idx <- setNames(seq_len(n), nodes)
  
  ok <- (!is.na(idx[f])) & (!is.na(idx[t]))
  f2 <- f[ok]; t2 <- t[ok]
  fi <- idx[f2]; ti <- idx[t2]
  
  adj_out <- vector("list", n)
  adj_in  <- vector("list", n)
  
  out_splits <- split(t2, fi)
  in_splits  <- split(f2, ti)
  
  if (length(out_splits) > 0) {
    for (k in names(out_splits)) {
      i <- as.integer(k)
      adj_out[[i]] <- unique(out_splits[[k]])
    }
  }
  if (length(in_splits) > 0) {
    for (k in names(in_splits)) {
      i <- as.integer(k)
      adj_in[[i]] <- unique(in_splits[[k]])
    }
  }
  
  adj_all <- vector("list", n)
  for (i in seq_len(n)) {
    adj_all[[i]] <- union(adj_in[[i]], adj_out[[i]])
  }
  
  adj_un <- vector("list", n)
  if (length(f2) > 0) {
    add1 <- split(t2, fi)  # i has neighbor j
    add2 <- split(f2, ti)  # j has neighbor i
    for (k in names(add1)) {
      i <- as.integer(k)
      adj_un[[i]] <- c(adj_un[[i]], add1[[k]])
    }
    for (k in names(add2)) {
      i <- as.integer(k)
      adj_un[[i]] <- c(adj_un[[i]], add2[[k]])
    }
    for (i in seq_len(n)) adj_un[[i]] <- unique(adj_un[[i]])
  }
  
  list(
    nodes = nodes,
    idx   = idx,
    adj_in  = adj_in,
    adj_out = adj_out,
    adj_all = adj_all,
    adj_un  = adj_un
  )
}