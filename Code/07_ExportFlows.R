##### INTRODUCTION ################################################################################
source("Code/functions.needed.analysis.R")
source("Code/functions.needed.sci.not.R")
library(xlsx)

NiceNames <- as.matrix(read.xlsx(paste0("Input/",excel.file), sheetName = "Label"))

for (c in Systems) {
  load(paste0("Input/InputReady.Mod2_",c,".Rdata"))
  Masses <- array(NA, c(length(Materials), length(Names), SIM), dimnames = list(Materials, Names, NULL))
  for(mat in Materials){
    # import output data
    load(paste0("Results/Emissions/OutputMass_",c,"_",mat,".Rdata"))
    for(comp in Names){
      Masses[mat,comp,] <- Mass[comp,]
    }
  }
  rm(Mass)
  
  rownames(NiceNames) <- NiceNames[,"Name"]
  
  # Flows <- matrix(NA, length(theflows),SIM, dimnames = list(theflows, NULL))
  # to.export <- matrix(NA, length(theflows), 2, dimnames = list(theflows, c("Label", "ArrowWidth")))
  
  ##### DEFINITION OF FLOWS #########################################################################
  
  # create empty vectors for flows
  Flow <- sapply(Materials, function(x) NULL)
  for(mat in Materials){
    
    ### CALCULATE FLOWS BETWEEN ALL COMPARTMENTS
    
    Flow[[mat]] <- sapply(Names, function(x) NULL)
    
    for(comp in Names){
      for(dest in names(TC.Norm[[mat]][[comp]])){
        Flow[[mat]][[comp]][[dest]] <- TC.Norm[[mat]][[comp]][[dest]]*Masses[mat,comp,]
      }      
    }
    # add the input
    Flow[[mat]][["Input"]] <- Input[[mat]]
    
    # save in a matrix
    FlowsSummary <- matrix(NA,1,9, 
                           dimnames = list(NULL, c("From", "To", "Mean", "SD", "Q5", "Q25", "Q50", "Q75", "Q95")))
    for(comp in c("Input",Names)){
      for(dest in names(Flow[[mat]][[comp]])){
        
        # skip if flow is NULL
        if(is.null(Flow[[mat]][[comp]][[dest]])){
          next
        }
        
        data <- Flow[[mat]][[comp]][[dest]]
        
        # append new flow to previous flows
        FlowsSummary <- rbind(FlowsSummary, c(NiceNames[comp, "MediumLabel"],
                                              NiceNames[dest, "MediumLabel"],
                                              signif(mean(data),4),
                                              signif(sd(data),4),
                                              signif(quantile(data, 0.05),4),
                                              signif(quantile(data, 0.25),4),
                                              signif(quantile(data, 0.50),4),
                                              signif(quantile(data, 0.75),4),
                                              signif(quantile(data, 0.95),4)))
      }
    }
    
    # remove first line that contains only NA values
    FlowsSummary <- FlowsSummary[-1,]
    
    # save data
    write.xlsx(FlowsSummary, file = paste0("Results/Tables/Flows/Flows_",c,".xlsx"),
               sheetName = mat, append = TRUE, col.names = T, row.names = F)
    
  }
  message(paste0(format(Sys.time(), "%H:%M:%S"),"\n",
                 "All the flows exported for Country: ", c))
}

rm(Flow,FlowsSummary,data)