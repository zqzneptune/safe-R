load_attributes <- function(safe) {
  
  data <- read.table(safe[["annotationfile"]], header=TRUE,
                     row.names=1,
                     comment.char="#", sep="\t",
                     na.strings=c("NaN","None"))
  
  safe[["attributeIds"]] <- 1:ncol(data)
  safe[["attributeNames"]] <- colnames(data)
  
  # Re-sort the loaded attribute annotation data by node label
  node2attribute <- data[safe[["nodeLabelsSystematic"]],,drop=FALSE]
  node2attribute <- as.matrix(node2attribute)
  
  # Replace NAs with zeros
  node2attribute[is.na(node2attribute)] <- 0
  
  safe[["node2attribute"]] <- node2attribute
  
  # Store some general stats about the attributes & prepare for future analyses
  safe[["attributeSize"]] <- colSums(data, na.rm=TRUE)
  safe[["attributeLabelNumber"]] <- nrow(data)
  safe[["attributeIsTop"]] <- !logical(length=ncol(data))
  
  safe
}