# install.packages("ini")
# install.packages("igraph")

path_to_settings_file <- "~/Laboratory/Utils/R/Networks/safe/safe.ini"

safe <- function(path_to_settings_file) {
  
  source("~/Laboratory/Utils/R/Networks/safe/io/load_settings.R")
  source("~/Laboratory/Utils/R/Networks/safe/io/load_network.R")
  source("~/Laboratory/Utils/R/Networks/safe/io/plot_network.R")
  source("~/Laboratory/Utils/R/Networks/safe/io/load_attributes.R")
  source("~/Laboratory/Utils/R/Networks/safe/extras/compute_node_distances.R")
  source("~/Laboratory/Utils/R/Networks/safe/extras/compute_enrichments.R")
  source("~/Laboratory/Utils/R/Networks/safe/io/plot_sample_attributes.R")
  
  safe <- load_settings(path_to_settings_file)
  safe <- load_network(safe)
  
  # plot_network(safe)
  
  safe <- compute_node_distances(safe)
  
  safe <- load_attributes(safe)
  
  ptm <- proc.time()
  safe <- compute_enrichments(safe)
  proc.time() - ptm

  plot_sample_attributes(safe)
  
}