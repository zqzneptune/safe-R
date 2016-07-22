plot_network <- function(safe) {
  
  library(igraph)

  g <- graph_from_adjacency_matrix(safe[["edges"]], mode=c("undirected"))
  l <- cbind(safe[["nodeX"]], -safe[["nodeY"]])
  
  par(bg = "black")
  plot(g, 
       vertex.size=3, vertex.color="white", vertex.frame.color=NA, vertex.label=NA, 
       edge.color=rgb(1,1,1,0.2),
       layout=l)
  
}