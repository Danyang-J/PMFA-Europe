for (c in Systems) {
  load(paste0("Input/InputFormatted_", c, ".Rdata"))  
  
  ##### HYGIENE PRODUCTS ############################################################################
  
  source("Code/TC_Hygiene.R")
  
  ##### save data as Rdata ##########################################################################
  save_path <- paste0("Input/InputReady.Mod1_", c, ".Rdata")
  
  save(TC.Norm, TC.Release, Input, file = save_path)
  message(paste(format(Sys.time(), "%H:%M:%S"), "Data saved at", save_path))
}
