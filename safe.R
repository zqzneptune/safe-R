# install.packages("ini")
# install.packages("igraph")
# install.packages('doParallel')

# path_to_settings_file <- "safe.ini"

safe <- function(path_to_settings_file, datasetid) {

  source("io/load_settings.R")
  safe <- load_settings(path_to_settings_file)
  
  if (!is.null(datasetid)) {
    safe[["annotationfile"]] <- paste("http://yeastphenome-dev.princeton.edu/data/datasets/", datasetid, "/YeastPhenome_dataset_", datasetid, "_data.txt",
                                      sep = "")
  }
  source("io/load_network.R")
  safe <- load_network(safe)
  
  # source("io/plot_network.R")
  # plot_network(safe)
  
  source("extras/compute_node_distances.R")
  safe <- compute_node_distances(safe)
  
  source("io/load_attributes.R")
  safe <- load_attributes(safe)
  
  source("extras/compute_enrichments.R")
  safe <- compute_enrichments(safe)

  source("io/plot_sample_attributes.R")
  plot_sample_attributes(safe)
  
}