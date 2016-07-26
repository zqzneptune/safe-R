load_settings <- function(path_to_settings_file, datasetid) {
  
  library(ini)
  settings = read.ini(path_to_settings_file)
  
  # Get rid of section names ----------
  safe_data <- list()
  sections = names(settings)
  for (i in 1:length(sections)) {
    safe_data <- c(safe_data, settings[[i]])
  }
  
  # Backwards compatibility ----------
  params <- names(safe_data)
  
  if (!("layoutAlgorithm" %in% params)) {
    safe_data <- c(safe_data, layoutAlgorithm="")
  }
  
  if (!("neighborhoodRadiusType" %in% params)) {
    safe_data <- c(safe_data, neighborhoodRadiusType="percentile")
  }
  
  if (!("unimodality" %in% params)) {
    safe_data <- c(safe_data, unimodality=1)
    if (safe_data[["unimodalityType"]]=="") {
      safe_data[["unimodality"]] <- 0
    }
  }
  
  if (!("groupDistance" %in% params)) {
    safe_data <- c(safe_data, groupDistance=1)
    if (safe_data[["groupDistanceType"]]=="") {
      safe_data[["groupDistanceType"]] <- 0
    }
  }
  
  if (!("plotNetwork" %in% params)) {
    safe_data <- c(safe_data, plotNetwork=1)
  }
  
  
  # Cross-checks and adjustments ----------
  
  safe_data[["neighborhoodRadius"]] <- as.numeric(safe_data[["neighborhoodRadius"]])/100
  safe_data[["MAX_LOG10_PVAL"]] <- as.numeric(safe_data[["MAX_LOG10_PVAL"]])
  safe_data[["THRESHOLD_ENRICHMENT"]] <- as.numeric(safe_data[["THRESHOLD_ENRICHMENT"]])
  
  if (!(safe_data[["annotationsign"]] %in% c("both", "highest", "lowest"))) {
    safe_data[["annotationsign"]] <- "highest"
    warning("Unknown value for parameter \"annotationsign\". Setting the parameter to default value: \"highest\". See safe.ini for available options.")
  }
  
  if (!(safe_data[["neighborhoodRadiusType"]] %in% c("percentile","absolute","diameter"))) {
    safe_data[["neighborhoodRadiusType"]] <- "percentile"
    warning("Unknown value for parameter \"neighborhoodRadiusType\". Setting the parameter to default value: \"percentile\". See safe.ini for available options.")
  }
  
  if (!is.null(datasetid)) {
    safe_data[["annotationfile"]] <- paste("http://www.yeastphenome.org/data/datasets/", datasetid, "/YeastPhenome_dataset_", datasetid, "_data.txt",
                                      sep = "")
  }
  
  safe_data
  
}
  