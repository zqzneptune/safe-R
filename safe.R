# install.packages("ini")
# install.packages("igraph")
# install.packages('doParallel')

# path_to_settings_file <- "safe.ini"

source("io/load_settings.R", chdir = TRUE)
source("io/load_network.R", chdir = TRUE)
source("extras/compute_node_distances.R", chdir = TRUE)
source("io/load_attributes.R", chdir = TRUE)
source("extras/compute_enrichments.R", chdir = TRUE)
source("io/plot_sample_attributes.R", chdir = TRUE)

safe <- function(path_to_settings_file, datasetid = NULL) {
  
  safe_data <- load_settings(path_to_settings_file, datasetid)
  
  safe_data <- load_network(safe_data)
  
  # source("io/plot_network.R")
  # plot_network(safe)
  
  safe_data <- compute_node_distances(safe_data)
  
  safe_data <- load_attributes(safe_data)
  
  safe_data <- compute_enrichments(safe_data)

  plot_sample_attributes(safe_data)
  
  safe_data
  
}