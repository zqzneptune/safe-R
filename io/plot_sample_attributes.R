plot_sample_attributes <- function(safe_data) {
  
  num_nodes = length(safe_data[["nodeLabels"]])
  num_attributes = length(safe_data[["attributeNames"]])
  
  # Yellow/blue colormap
  map_colors <- rbind(c(255, 204, 0), c(0, 204, 255))/255
  
  color1 <- matrix(rep(map_colors[1,], each = num_nodes), ncol = 3)
  color2 <- matrix(rep(map_colors[2,], each = num_nodes), ncol = 3)
  
  # Network outline
  ind_xy <- chull(safe_data[["nodeX"]], safe_data[["nodeY"]])
  ind_xy <- c(ind_xy, ind_xy[1])   # to close the gap between the first and the last point
  
  for (i in 1:num_attributes) {
    
    opacity1 <- matrix(rep(safe_data[["opacity"]][,i,1], times = 3), ncol = 3)
    opacity2 <- matrix(rep(safe_data[["opacity"]][,i,2], times = 3), ncol = 3)
    
    c12 <- array(c(color1 * opacity1, color2 * opacity2), dim = c(num_nodes, 3, 2))
    c12 <- apply(c12, c(1, 2), sum, na.rm = TRUE)
    
    # Sort points so that the brightest points are on top
    opacity <- apply(cbind(safe_data[["opacity"]][,i,1], safe_data[["opacity"]][,i,2]), 1, max, na.rm = TRUE)
    opacity_sorted <- sort(opacity, na.last = NA, index.return = TRUE, decreasing = FALSE)
    
    # Plot
    par(bg = "black", col.main = "white")
    plot(safe_data[["nodeX"]][opacity_sorted$ix], -safe_data[["nodeY"]][opacity_sorted$ix], 
         pch = 19, col = rgb(c12[opacity_sorted$ix,]), asp = 1)
    lines(safe_data[["nodeX"]][ind_xy], -safe_data[["nodeY"]][ind_xy], col = "white", lty = 2)
    
    # Title
    ttl <- paste(strwrap(safe_data[["attributeNames"]][i], width = 60), collapse = "\n")
    title(main = ttl)
    
  }

  
}