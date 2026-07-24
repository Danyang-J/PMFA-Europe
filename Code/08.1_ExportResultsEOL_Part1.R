source("Code/functions.needed.analysis.R")
source("Code/functions.needed.sci.not.R")
library(xlsx)
library(dplyr)

NiceNames <- as.matrix(read.xlsx(paste0("Input/", excel.file), sheetName = "Label"))
rownames(NiceNames) <- NiceNames[, "Name"]

##### Calculate Emissions ##########################################################################
# Final compartment groups
env_levels <- list(
  "Incineration"          = c("Elimination"),
  "Landfill"         = c("Landfill"),
  "Sludge amended soil" = c("SludgeAmendedSoilMacro"),
  "Sludge amended soil (MP)" = c("SludgeAmendedSoilMicro"),
  "Release"         =c("UrbanSoilMicro", "UrbanSoilMacro",
                       "AgriculturalSoilMacro", "AgriculturalSoilMicro",
                       "DeepUrbanSoilMicro",
                       "RuralSoilMacro", "RuralSoilMicro",
                       "RoadSideMacro",
                       "BeachSoilMicro", "BeachSoilMacro",
                       "SurfaceWaterMacro", "SurfaceWaterMicro",
                       "CoastalWaterMicro","CoastalWaterMacro",
                       "OceanMicro","OceanMacro")
  )

# Final compartments of interest
end.comp <- c(
  "UrbanSoilMicro", "UrbanSoilMacro",
  "AgriculturalSoilMacro", "AgriculturalSoilMicro",
  "DeepUrbanSoilMicro",
  "RuralSoilMacro", "RuralSoilMicro",
  "RoadSideMacro",
  "BeachSoilMicro", "BeachSoilMacro",
  "SurfaceWaterMacro", "SurfaceWaterMicro",
  "CoastalWaterMicro","CoastalWaterMacro",
  "OceanMicro","OceanMacro",
  "Elimination","Landfill",
  "SludgeAmendedSoilMacro", "SludgeAmendedSoilMicro"
)

for (c in Systems) {
  # import input data
  load(paste0("Input/InputReady.Mod2_",c,".Rdata"))
  
  # product categories
  PC <- unique(c(names(TC.Norm[["PP"]][["Packaging"]]),
                 names(TC.Norm[["PP"]][["BuildingConstruction"]]),
                 names(TC.Norm[["PP"]][["Automotive"]]),
                 names(TC.Norm[["PP"]][["EEE"]]),
                 names(TC.Norm[["PP"]][["Agriculture"]]),
                 names(TC.Norm[["PP"]][["Other"]]),
                 names(TC.Norm[["PP"]][["Fishery"]]),
                 names(TC.Norm[["PP"]][["Aquaculture"]]),               
                 names(TC.Norm[["PP"]][["FisheryTextiles"]]),
                 names(TC.Norm[["PP"]][["AquacultureTextiles"]]),
                 names(TC.Norm[["PP"]][["Apparel"]]),
                 names(TC.Norm[["PP"]][["HouseholdTextiles"]]),
                 names(TC.Norm[["PP"]][["TechnicalTextiles"]])))
  PC <- PC[!PC == "Export"]
  
  # Import masses in PC
  Masses <- array(NA, c(length(Materials), length(Names), SIM),
                  dimnames = list(Materials, Names, NULL))
  for(mat in Materials){
    load(paste0("Results/Emissions/OutputMass_",c,"_",mat,".Rdata"))
    for(comp in Names){
      Masses[mat,comp,] <- Mass[comp,]
    }
  }
  
  # Stop compartments
  PC.stop <- c("PCPlasticCollection",
                "PCFibreCollection",
                "PackagingCollection",
                "MixedCollection",
                "CDCollection",
                "CDIncinerableCollection",
                "ELVCollection",
                "ELVTextilesCollection",
                "WEEECollection",
                "AgricultureCollection",
                "TextileCollection",
                "FishingGearCollection",
                "PackagingRecycling",
                "CDRecycling",
                "LargePartRecycling",
                "ASRRecycling",
                "WEEPRecycling",               
                "AgricultureRecycling",
                "Incineration")
  
  # matrices for storing final fraction or mass
  FC.Frac <- FC.Mass <- array(0, c(length(env_levels), length(Materials), length(PC)+2,SIM),
                              dimnames = list(names(env_levels),
                                              Materials,
                                              c("Pre-consumer processes", PC, "Post-consumer processes"),
                                              NULL))
  
  # Determine final compartment and store appropriately
  for(comp in PC){
    for(mat in Materials){
      FC.Frac.List <- find.fc(comp = comp,
                              mass = NULL,
                              TC.Distr = TC.Norm[[mat]],
                              verbose = F,
                              stop.at = PC.stop)
      
      FC.Mass.List <- mult.tc(FC.Frac.List, Masses[mat,comp,])
      
      # Aggregate by environment
      for(env_name in names(env_levels)){
        for(dest in env_levels[[env_name]]){
          if(!is.null(FC.Frac.List[[dest]])){
            FC.Frac[env_name, mat, comp,] <- FC.Frac[env_name, mat, comp,] + FC.Frac.List[[dest]]
            FC.Mass[env_name, mat, comp,] <- FC.Mass[env_name, mat, comp,] + FC.Mass.List[[dest]]
          }
        }
      }
    }
  }
  
  ### --- Pre-consumer processes
  comp.pre <- c("PrimaryProduction",
                "RecyclateRepelletizing",
                "Transport",
                "FibreProduction",
                "NonTextileManufacturing",
                "TextileManufacturing")
  
  comp.pre.mass <- c("Packaging",
                     "BuildingConstruction",
                     "Automotive",
                     "EEE",
                     "Agriculture",
                     "Other",
                     "Fishery",
                     "Aquaculture",
                     "Apparel",
                     "HouseholdTextiles",
                     "TechnicalTextiles",
                     "FisheryTextiles",
                     "AquacultureTextiles",
                     "PCPlasticCollection",
                     "PCFibreCollection")
  
  for(mat in Materials){
    EM.mat <- array(0, c(length(comp.pre), length(end.comp), SIM), dimnames = list(comp.pre,end.comp, NULL))
    EF.mat <- array(NA, c(length(comp.pre), length(end.comp), SIM), dimnames = list(comp.pre,end.comp, NULL))
    
    for(comp in comp.pre){
      comp.stop <- c(comp.pre,PC,"PCPlasticCollection","PCFibreCollection")
      comp.stop <- comp.stop[-which(comp.stop == comp)]
      
      FC.Frac.List <- find.fc(comp = comp,
                              mass = NULL,
                              TC.Distr = TC.Norm[[mat]],
                              verbose = F,
                              stop.at = comp.stop)
      
      for(dest in end.comp){
        if(!is.null(FC.Frac.List[[dest]])){
          EM.mat[comp,dest,] <- FC.Frac.List[[dest]]*Masses[mat,comp,]
          EF.mat[comp,dest,] <- FC.Frac.List[[dest]]
        }
      }
    }
    
    # Aggregate into FC.Mass and FC.Frac
    for(env_name in names(env_levels)){
      for(dest in env_levels[[env_name]]){
        FC.Mass[env_name,mat,"Pre-consumer processes",] <- FC.Mass[env_name,mat,"Pre-consumer processes",] + apply(EM.mat[,dest,],2,sum)
        temp <- matrix(NA, length(comp.pre), SIM, dimnames = list(comp.pre,NULL))
        for(comp in comp.pre){
          temp[comp,] <- EF.mat[comp,dest,]*Masses[mat,comp,]/apply(Masses[mat,comp.pre.mass,],2,sum)
        }
        FC.Frac[env_name,mat,"Pre-consumer processes",] <- apply(temp,2,function(x) sum(x,na.rm = TRUE))
      }
    }
  }
  
  ### --- Post-consumer processes
  comp.post <- c("PCPlasticCollection",
                 "PCFibreCollection",
                 "PackagingCollection",
                 "MixedCollection",
                 "CDCollection",
                 "CDIncinerableCollection",
                 "ELVCollection",
                 "ELVTextilesCollection",
                 "WEEECollection",
                 "AgricultureCollection",
                 "TextileCollection",
                 "FishingGearCollection",
                 "PackagingRecycling",
                 "CDRecycling",
                 "LargePartRecycling",
                 "ASRRecycling",
                 "WEEPRecycling",
                 "AgricultureRecycling",
                 "Incineration")
  
  comp.post.mass <- c("PCPlasticCollection",
                      "PCFibreCollection",
                      "PackagingCollection",
                      "MixedCollection",
                      "CDCollection",
                      "CDIncinerableCollection",
                      "ELVCollection",
                      "ELVTextilesCollection",
                      "WEEECollection",
                      "AgricultureCollection",
                      "TextileCollection",
                      "FishingGearCollection")
  
  for(mat in Materials){
    EM.mat <- array(0, c(length(comp.post), length(end.comp), SIM), dimnames = list(comp.post,end.comp, NULL))
    EF.mat <- array(NA, c(length(comp.post), length(end.comp), SIM), dimnames = list(comp.post,end.comp, NULL))
    
    for(comp in comp.post){
      comp.stop <- c(comp.post,PC)
      comp.stop <- comp.stop[-which(comp.stop == comp)]
      
      FC.Frac.List <- find.fc(comp = comp,
                              mass = NULL,
                              TC.Distr = TC.Norm[[mat]],
                              verbose = F,
                              stop.at = comp.stop)
      
      for(dest in end.comp){
        if(!is.null(FC.Frac.List[[dest]])){
          EM.mat[comp,dest,] <- FC.Frac.List[[dest]]*Masses[mat,comp,]
          EF.mat[comp,dest,] <- FC.Frac.List[[dest]]
        }
      }
    }
    
    for(env_name in names(env_levels)){
      for(dest in env_levels[[env_name]]){
        FC.Mass[env_name,mat,"Post-consumer processes",] <- FC.Mass[env_name,mat,"Post-consumer processes",] + apply(EM.mat[,dest,],2,sum)
        temp <- matrix(NA, length(comp.post), SIM, dimnames = list(comp.post,NULL))
        for(comp in comp.post){
          temp[comp,] <- EF.mat[comp,dest,]*Masses[mat,comp,]/apply(Masses[mat,comp.post.mass,],2,sum)
        }
        FC.Frac[env_name,mat,"Post-consumer processes",] <- apply(temp,2,function(x) sum(x,na.rm = TRUE))
      }
    }
  }
  
  ### --- Save results
  save_path <- paste0("Results/Emissions/ProcessedMassEOL_", c, ".Rdata")
  save(FC.Frac,FC.Mass,Masses, file = save_path)
  
  message(paste0(format(Sys.time(), "%H:%M:%S"), " Results (EOL) saved for ", c))
}
