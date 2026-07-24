##### INTRO #######################################################################################

# source needed functions
source("Code/functions.needed.R")
library("xlsx")
library("trapezoid")

for (c in Systems) {
  # import data
  load(paste0("Input/InputReady.Mod2_",c,".Rdata"))
  
  # loop over systems and polymers
  for(mat in Materials){
    
    timer <- proc.time()
    
    # the solve.MC function will solve the equation system N times after having
    # reconstructed the TC matrix for every iteration
    Mass <- solve.MC(TC.Distr  = TC.Norm[[mat]],
                     inp.Distr = Input[[mat]],
                     Names     = Names,
                     N         = SIM)
    
    save(Mass = Mass,
         file = paste0("Results/Emissions/OutputMass_",c,"_",mat,".Rdata"))
    
    # notify how long was needed for the whole calculation
    message("Time needed for the simulation:")
    print(proc.time() - timer) # second timer
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"), "Results saved for", c))
  
}
