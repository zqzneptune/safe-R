load_attributes <- function(safe_data) {
  
  print("Loading the attribute file...")
  
  data <- read.table(safe_data[["annotationfile"]], header=TRUE,
                     row.names = 1,
                     comment.char="#", sep="\t",
                     na.strings=c("NaN","None"),
                     check.names = FALSE)
  
  safe_data[["attributeIds"]] <- 1:ncol(data)
  safe_data[["attributeNames"]] <- colnames(data)
  
  # Re-sort the loaded attribute annotation data by node label
  node2attribute <- data[safe_data[["nodeLabelsSystematic"]],,drop=FALSE]
  node2attribute <- as.matrix(node2attribute)
  
  # Replace NAs with zeros
  node2attribute[is.na(node2attribute)] <- 0
  
  safe_data[["node2attribute"]] <- node2attribute
  
  # Store some general stats about the attributes & prepare for future analyses
  safe_data[["numNodesPerAttribute"]] <- colSums(node2attribute, na.rm = TRUE)
  
  safe_data[["attributeSize"]] <- colSums(data, na.rm = TRUE)
  safe_data[["attributeLabelNumber"]] <- nrow(data)
  safe_data[["attributeIsTop"]] <- !logical(length=ncol(data))
  
  safe_data
}