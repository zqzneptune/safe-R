plot_network <- function(safe_data) {
  
  library(igraph)

  g <- graph_from_adjacency_matrix(safe_data[["edges"]], mode=c("undirected"))
  l <- cbind(safe_data[["nodeX"]], -safe_data[["nodeY"]])
  
  par(bg = "black")
  plot(g, 
       vertex.size=3, vertex.color="white", vertex.frame.color=NA, vertex.label=NA, 
       edge.color=rgb(1,1,1,0.2),
       layout=l)
  
}