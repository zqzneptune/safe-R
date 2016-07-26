compute_enrichments <- function(safe_data) {
  
  print("Calculating enrichments...")
  
  # Define neighborhood radius ---------------------------
  
  if (safe_data[["neighborhoodRadiusType"]] == "percentile") {
    r <- quantile(safe_data[["nodeDistance"]], c(safe_data[["neighborhoodRadius"]]),
                  na.rm = TRUE, type = 1)
    # Note: type=1 was chosen because it gives the same answer as MATLAB
  }
  
  
  # Define neighborhoods ---------------------------
  
  safe_data[["neighborhoods"]] <- safe_data[["nodeDistance"]] <= r
  
  
  # Compute starting values ---------------------------
  
  NLBL <- length(safe_data[["nodeLabels"]])
  NGRP <- length(safe_data[["attributeNames"]])
  
  if (safe_data[["background"]] == "map") {
    
    # Total number of nodes on the map (in matrix format)
    N <- matrix(data = NLBL, nrow = NLBL, ncol = NGRP)
    
    # Number of nodes (on the map) that are annotated to a given attribute
    Ng <- matrix(rep(colSums(safe_data[["node2attribute"]], na.rm = TRUE), each = NLBL), nrow = NLBL)
  }
  
  # Number of nodes in each node's neighborhood
  Ni <- rowSums(safe_data[["neighborhoods"]], na.rm = TRUE)
  
  # Number of nodes in each node's neighborhood that are also annotated to each attribute
  Nig <- safe_data[["neighborhoods"]] %*% safe_data[["node2attribute"]]
  
  
  # Determine if we are dealing with binary or quantitative annotations ---------------------------
  
  
  # Run the quantative schema ---------------------------
  
  nPermutations <- 1000
  
  if (safe_data[["annotationsign"]] == "both") {
    safe_data[["pval"]] <- array(data = NA, dim = c(NLBL, NGRP, 2))
  } else {
    safe_data[["pval"]] <- matrix(data = NA, nrow = NLBL, ncol = NGRP)
  }
  
  if (NGRP == 1) {
    
    # Faster solution for the case with only 1 attribute
    Attr <- array(data = rep(safe_data[["node2attribute"]], times = nPermutations), dim = c(NLBL, nPermutations))
    Attr <- apply(Attr, 2, sample)
    Sr <- safe_data[["neighborhoods"]] %*% Attr
    Sr <- array(data = Sr, dim = c(NLBL, 1, nPermutations))
    
  } else {
    
    Sr <- array(data = NA, dim = c(NLBL, NGRP, nPermutations))
    
    for (r in 1:nPermutations) {
      ixPerm <- sample(NLBL, size = NLBL, replace = FALSE)
      Wr <- safe_data[["node2attribute"]][ixPerm,]
      Sr[,,r] <- safe_data[["neighborhoods"]] %*% Wr
    }
    
  }
  
  for (grp in 1:NGRP) {
    Sm <- rowMeans(Sr[,grp,], na.rm = TRUE)
    Ss <- apply(Sr[,grp,], 1, sd, na.rm = TRUE)
    # Z <- (Nig[,grp] - Sm) / Ss
    
    if (safe_data[["annotationsign"]] == "highest") {
      safe_data[["pval"]][,grp] <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = FALSE, log.p = FALSE)
    } else if (safe_data[["annotationsign"]] == "lowest") {
      safe_data[["pval"]][,grp] <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = TRUE, log.p = FALSE)
    } else {
      t1 <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = FALSE, log.p = FALSE)
      t2 <- pnorm(Nig[,grp], mean = Sm, sd = Ss, lower.tail = TRUE, log.p = FALSE)
      safe_data[["pval"]][,grp,1] <- t1
      safe_data[["pval"]][,grp,2] <- t2
    }
    
  }
  
  # Final steps ---------------------------
  
  safe_data[["opacity"]] <- -log10(safe_data[["pval"]])
  
  m <- safe_data[["MAX_LOG10_PVAL"]]
  safe_data[["opacity"]][safe_data[["opacity"]] > m] <- m
  safe_data[["opacity"]] <- safe_data[["opacity"]] / m
  
  # Calculate the minimum opacity corresponding to significant enrichment (after Bonferroni multiple testing correction)
  safe_data[["thresholdOpacity"]] <- -log10(safe_data[["THRESHOLD_ENRICHMENT"]]/length(safe_data[["attributeIds"]]))/safe_data[["MAX_LOG10_PVAL"]]
  
  # Binarize the opacity
  safe_data[["opacity_01"]] <- safe_data[["opacity"]] > safe_data[["thresholdOpacity"]]
  
  # Calculate the size of each attribute's enrichment landscape
  safe_data[["numNodesEnrichedPerAttribute"]] <- apply(safe_data[["opacity_01"]], c(2, 3), sum, na.rm = TRUE)
  
  # Calculate the number of attributes each node is enriched for
  safe_data[["numAttributesEnrichedPerNode"]] <- apply(safe_data[["opacity_01"]], c(1, 3), sum, na.rm = TRUE)
  
  # Output
  safe_data
  
}