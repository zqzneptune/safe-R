load_network <- function(safe_data) {
  
  # If the input file is not specified, use the default Costanzo et al., 2010 network
  if (is.null(safe_data[["networkfile"]])) {
    
    print("Loading the genetic interaction similarity network (Costanzo~Boone, 2010)...")
    
    load("safe/data/layout_Costanzo2010_150831.RData")
    
    fields <- names(costanzo2010)
    for (i in 1:length(fields)) {
      safe_data[[fields[[i]]]] <- costanzo2010[[fields[[i]]]]
    }
    
  }
 
  safe_data
  
}

