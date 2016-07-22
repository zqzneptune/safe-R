compute_enrichments <- function(safe) {
  
  # Define neighborhood radius ---------------------------
  
  if (safe[["neighborhoodRadiusType"]] == "percentile") {
    r <- quantile(safe[["nodeDistance"]], c(safe[["neighborhoodRadius"]]),
                  na.rm = TRUE, type = 1)
    # Note: type=1 was chosen because it gives the same answer as MATLAB
  }
  
  
  # Define neighborhoods ---------------------------
  
  safe[["neighborhoods"]] <- safe[["nodeDistance"]] <= r
  
  
  # Compute starting values ---------------------------
  
  NLBL <- length(safe[["nodeLabels"]])
  NGRP <- length(safe[["attributeNames"]])
  
  if (safe[["background"]] == "map") {
    
    # Total number of nodes on the map (in matrix format)
    N <- matrix(data = NLBL, nrow = NLBL, ncol = NGRP)
    
    # Number of nodes (on the map) that are annotated to a given attribute
    Ng <- matrix(rep(colSums(safe[["node2attribute"]], na.rm = TRUE), each = NLBL), nrow = NLBL)
  }
  
  # Number of nodes in each node's neighborhood
  Ni <- rowSums(safe[["neighborhoods"]], na.rm = TRUE)
  
  # Number of nodes in each node's neighborhood that are also annotated to each attribute
  Nig <- safe[["neighborhoods"]] %*% safe[["node2attribute"]]
  
  
  # Determine if we are dealing with binary or quantitative annotations ---------------------------
  
  
  # Run the quantative schema ---------------------------
  
  nPermutations <- 1000
  Sr <- array(data = NA, dim = c(NLBL, NGRP, nPermutations))
  
  for (r in 1:nPermutations) {
    ixPerm <- sample(NLBL, size = NLBL, replace = FALSE)
    Wr <- safe[["node2attribute"]][ixPerm,]
    Sr[,,r] <- safe[["neighborhoods"]] %*% Wr
  }
  
  if (safe[["annotationsign"]] == "both") {
    safe[["pval"]] <- array(data = NA, dim = c(NLBL, NGRP, 2))
  } else {
    safe[["pval"]] <- matrix(data = NA, nrow = NLBL, ncol = NGRP)
  }
  
  for (grp in 1:NGRP) {
    Sm <- rowMeans(Sr[,grp,], na.rm = TRUE)
    Ss <- apply(Sr[,grp,], 1, sd, na.rm = TRUE)
    Z <- (Nig[,grp] - Sm) / Ss
    
    if (safe[["annotationsign"]] == "highest") {
      safe[["pval"]][,grp] <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = FALSE, log.p = FALSE)
    } else if (safe[["annotationsign"]] == "lowest") {
      safe[["pval"]][,grp] <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = TRUE, log.p = FALSE)
    } else {
      t1 <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = FALSE, log.p = FALSE)
      t2 <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = TRUE, log.p = FALSE)
      safe[["pval"]][,grp,1] <- t1
      safe[["pval"]][,grp,1] <- t2
    }
    
  }
  
  # Final adjustments ---------------------------
  
  safe[["opacity"]] <- -log10(safe[["pval"]])
  
  m <- safe[["MAX_LOG10_PVAL"]]
  safe[["opacity"]][safe[["opacity"]] > m] <- m
  safe[["opacity"]] <- safe[["opacity"]] / m
  
  # Calculate the minimum opacity corresponding to significant enrichment (after Bonferroni multiple testing correction)
  
  safe[["thresholdOpacity"]] <- -log10(safe[["THRESHOLD_ENRICHMENT"]]/length(safe[["attributeIds"]]))/safe[["MAX_LOG10_PVAL"]]
  
  safe[["opacity_01"]] <- safe[["opacity"]] > safe[["thresholdOpacity"]]
  
}