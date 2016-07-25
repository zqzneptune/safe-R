plot_sample_attributes <- function(safe) {
  
  num_nodes = length(safe[["nodeLabels"]])
  num_attributes = length(safe[["attributeNames"]])
  
  # Yellow/blue colormap
  map_colors <- rbind(c(255, 204, 0), c(0, 204, 255))/255
  
  color1 <- matrix(rep(map_colors[1,], each = num_nodes), ncol = 3)
  color2 <- matrix(rep(map_colors[2,], each = num_nodes), ncol = 3)
  
  # Network outline
  ind_xy <- chull(safe[["nodeX"]], safe[["nodeY"]])
  ind_xy <- c(ind_xy, ind_xy[1])   # to close the gap between the first and the last point
  
  for (i in 1:num_attributes) {
    
    opacity1 <- matrix(rep(safe[["opacity"]][,i,1], times = 3), ncol = 3)
    opacity2 <- matrix(rep(safe[["opacity"]][,i,2], times = 3), ncol = 3)
    
    c12 <- array(c(color1 * opacity1, color2 * opacity2), dim = c(num_nodes, 3, 2))
    c12 <- apply(c12, c(1, 2), sum, na.rm = TRUE)
    
    # Sort points so that the brightest points are on top
    opacity <- apply(cbind(safe[["opacity"]][,i,1], safe[["opacity"]][,i,2]), 1, max, na.rm = TRUE)
    opacity_sorted <- sort(opacity, na.last = NA, index.return = TRUE, decreasing = FALSE)
    
    # Plot
    par(bg = "black")
    plot(safe[["nodeX"]][opacity_sorted$ix], -safe[["nodeY"]][opacity_sorted$ix], 
         pch = 19, col = rgb(c12[opacity_sorted$ix,]), asp = 1)
    lines(safe[["nodeX"]][ind_xy], -safe[["nodeY"]][ind_xy], col = "white", lty = 2)
    
  }

  
}