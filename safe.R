install.packages("ini")
install.packages("igraph")

source("io/load_settings.R")
source("io/load_network.R")
source("io/plot_network.R")
source("io/load_attributes.R")
source("extras/compute_node_distances.R")

safe <- load_settings("")
safe <- load_network(safe)
plot_network(safe)
safe <- load_attributes(safe)
safe <- compute_node_distances(safe)