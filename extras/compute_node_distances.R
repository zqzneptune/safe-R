compute_node_distances <- function(safe) {
  
  if (safe[["modality"]] == "default") {
    
    print("Loading the pre-calculated node distances for the default network...")
    load("data/layout_Costanzo2010_150831_nodeDistance.RData")
    safe[["nodeDistance"]] <- nodeDistance
    
  } else {
    
    if (safe[["nodeDistanceType"]] == "shortpath_weighted_layout") {
      
      print("Calculating node distances...")
      
      # First, compute the Euclidean distance between all nodes. These will be used as weights on the edges.
      x <- cbind(safe[["nodeX"]], safe[["nodeY"]])
      euclid <- as.matrix(dist(x, method="euclidean"))
      euclid[safe[["edges"]]==0] <- 0
      
      # Then, compute all shortest path lengths
      library(igraph)
      g <- graph_from_adjacency_matrix(euclid, mode = c("undirected"), weighted = TRUE)
      safe[["nodeDistance"]] <- distances(g)
      
    }
  }
  
  # Output
  safe
  
}