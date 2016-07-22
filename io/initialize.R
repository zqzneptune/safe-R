load_settings <- function(path_to_results_folder) {
  
  library(ini)
  
  inifilepath <- paste(path_to_results_folder, "safe.ini", sep="")
  settings = read.ini(inifilepath)
  settings
}