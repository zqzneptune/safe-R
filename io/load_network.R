load_network <- function(safe) {
  
  # If the input file is not specified, use the default Costanzo et al., 2010 network
  if (is.null(safe[["networkfile"]])) {
    print("Loading the genetic interaction similarity network (Costanzo~Boone, 2010)...")
    
    load("~/Laboratory/Utils/R/Networks/safe/data/layout_Costanzo2010_150831.RData")
    
    fields <- names(costanzo2010)
    for (i in 1:length(fields)) {
      safe[[fields[[i]]]] <- costanzo2010[[fields[[i]]]]
    }
    
    # Since this is the default network, pre-load the randomization data
    
    
  }
  
  # Output
  safe
  
}