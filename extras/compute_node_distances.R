compute_node_distances <- function(safe_data) {
  
  if (safe_data[["modality"]] == "default") {
    
    print("Loading the pre-calculated node distances for the default network...")
    load("safe/data/layout_Costanzo2010_150831_nodeDistance.RData")
    safe_data[["nodeDistance"]] <- nodeDistance
    
  } else {
    
    if (safe_data[["nodeDistanceType"]] == "shortpath_weighted_layout") {
      
      print("Calculating node distances...")
      
      # First, compute the Euclidean distance between all nodes. These will be used as weights on the edges.
      x <- cbind(safe_data[["nodeX"]], safe_data[["nodeY"]])
      euclid <- as.matrix(dist(x, method="euclidean"))
      euclid[safe_data[["edges"]]==0] <- 0
      
      # Then, compute all shortest path lengths
      library(igraph)
      g <- graph_from_adjacency_matrix(euclid, mode = c("undirected"), weighted = TRUE)
      safe_data[["nodeDistance"]] <- distances(g)
      
    }
  }
  
  # Output
  safe_data
  
}