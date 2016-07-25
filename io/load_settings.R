load_settings <- function(path_to_settings_file) {
  
  library(ini)
  
  settings = read.ini(path_to_settings_file)
  
  # Get rid of section names ----------
  safe <- list()
  sections = names(settings)
  for (i in 1:length(sections)) {
    safe <- c(safe, settings[[i]])
  }
  
  # Backwards compatibility ----------
  params <- names(safe)
  
  if (!("layoutAlgorithm" %in% params)) {
    safe <- c(safe, layoutAlgorithm="")
  }
  
  if (!("neighborhoodRadiusType" %in% params)) {
    safe <- c(safe, neighborhoodRadiusType="percentile")
  }
  
  if (!("unimodality" %in% params)) {
    safe <- c(safe, unimodality=1)
    if (safe[["unimodalityType"]]=="") {
      safe[["unimodality"]] <- 0
    }
  }
  
  if (!("groupDistance" %in% params)) {
    safe <- c(safe, groupDistance=1)
    if (safe[["groupDistanceType"]]=="") {
      safe[["groupDistanceType"]] <- 0
    }
  }
  
  if (!("plotNetwork" %in% params)) {
    safe <- c(safe, plotNetwork=1)
  }
  
  
  # Cross-checks and adjustments ----------
  
  safe[["neighborhoodRadius"]] <- as.numeric(safe[["neighborhoodRadius"]])/100
  safe[["MAX_LOG10_PVAL"]] <- as.numeric(safe[["MAX_LOG10_PVAL"]])
  safe[["THRESHOLD_ENRICHMENT"]] <- as.numeric(safe[["THRESHOLD_ENRICHMENT"]])
  
  if (!(safe[["annotationsign"]] %in% c("both", "highest", "lowest"))) {
    safe[["annotationsign"]] <- "highest"
    warning("Unknown value for parameter \"annotationsign\". Setting the parameter to default value: \"highest\". See safe.ini for available options.")
  }
  
  if (!(safe[["neighborhoodRadiusType"]] %in% c("percentile","absolute","diameter"))) {
    safe[["neighborhoodRadiusType"]] <- "percentile"
    warning("Unknown value for parameter \"neighborhoodRadiusType\". Setting the parameter to default value: \"percentile\". See safe.ini for available options.")
  }
  
  safe
  
}