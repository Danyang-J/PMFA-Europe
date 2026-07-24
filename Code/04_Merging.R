##### Merge the two TC modules into one ###########################################################
for (c in Systems) {
  load(paste0("Input/InputReady.Mod1_", c, ".Rdata")) 
  
  for(mat in Materials){
    
    # loop over all compartments which are not sinks
    for(comp in unique(c(names(TC.Norm[[mat]]), names(TC.Release[[mat]])))){
      
      
      # if there is no TC.Release flow, skip as there are no flows to add to TC.Norm
      if(!comp %in% names(TC.Release[[mat]])){
        next
        
        
        # if the compartment does not exist in module 1, include TCs and skip normalization
      } else if(!comp %in% names(TC.Norm[[mat]])) {
        
        # include TC.Release TC to TC.Norm
        TC.Norm[[mat]][[comp]] <- TC.Release[[mat]][[comp]]
        
        
        # else if there are emission flows for pre-existing compartments
      } else {
        
        # calculate sum of TC.Release factors
        sum.TC.Release <- apply(sapply(TC.Release[[mat]][[comp]], cbind), 1, sum)
        
        # loop over module 1 to renormalize each flow from module 1
        for(dest.m1 in names(TC.Norm[[mat]][[comp]])){
          
          # renormalize TC from module 1
          TC.Norm[[mat]][[comp]][[dest.m1]] <- TC.Norm[[mat]][[comp]][[dest.m1]] * (1 - sum.TC.Release)
          if(any(TC.Norm[[mat]][[comp]][[dest.m1]] < 0)){
            TC.Norm[[mat]][[comp]][[dest.m1]][ TC.Norm[[mat]][[comp]][[dest.m1]] < 0 ]  <- 0
          }
          
          # check if need to add some flows between the two modules (there may be two different kinds
          # of flows with identical source and destination)
          if(dest.m1 %in% names(TC.Release[[mat]][[comp]])){
            
            # add the two
            TC.Norm[[mat]][[comp]][[dest.m1]] <- ( TC.Norm[[mat]][[comp]][[dest.m1]] + 
                                                     TC.Release[[mat]][[comp]][[dest.m1]] )
            
            # remove entry from TC.Release database, otherwise it will replace the data calculated in the line above
            TC.Release[[mat]][[comp]][[dest.m1]] <- NULL
          }
        }
        
        # add the module 2 flows to TC.Norm
        for(dest.m2 in names(TC.Release[[mat]][[comp]])){
          TC.Norm[[mat]][[comp]][[dest.m2]] <- TC.Release[[mat]][[comp]][[dest.m2]]
        }
        
      }
    }
  }
  
  # calculate rest flows
  TC.Norm <- calc.rest.TC(TC.Norm)
  
  ##### Textile emissions ##########################################################################
  
  # introduce TCs
  for(mat in Materials){
    
    TC.Norm[[mat]][["ApparelPC"]][["WasteWaterMicro"]] <- Em.Clothing.WasteWater
    TC.Norm[[mat]][["ApparelPC"]][["IndoorAirMicro"]]  <- Em.Clothing.IndoorAir
    TC.Norm[[mat]][["ApparelPC"]][["OutdoorAirMicro"]] <- Em.Clothing.OutdoorAir
    
    TC.Norm[[mat]][["TechnicalClothing"]][["WasteWaterMicro"]] <- Em.Clothing.WasteWater
    TC.Norm[[mat]][["TechnicalClothing"]][["IndoorAirMicro"]]  <- Em.Clothing.IndoorAir
    TC.Norm[[mat]][["TechnicalClothing"]][["OutdoorAirMicro"]] <- Em.Clothing.OutdoorAir
    
    TC.Norm[[mat]][["HouseholdTextilesPC"]][["WasteWaterMicro"]] <- Em.HHText.WasteWater
    TC.Norm[[mat]][["HouseholdTextilesPC"]][["IndoorAirMicro"]]  <- Em.HHText.IndoorAir
    TC.Norm[[mat]][["HouseholdTextilesPC"]][["OutdoorAirMicro"]] <- Em.HHText.OutdoorAir
    
    TC.Norm[[mat]][["TechnicalHouseholdTextiles"]][["WasteWaterMicro"]] <- Em.HHText.WasteWater
    TC.Norm[[mat]][["TechnicalHouseholdTextiles"]][["IndoorAirMicro"]]  <- Em.HHText.IndoorAir
    TC.Norm[[mat]][["TechnicalHouseholdTextiles"]][["OutdoorAirMicro"]] <- Em.HHText.OutdoorAir
    
  }
  
  # normalize
  for(mat in Materials){
    TC.Norm[[mat]] <- normalize(TC.Norm[[mat]])    
  }
  
  ##### save data as Rdata ##########################################################################
  
  save_path <- paste0("Input/InputReady.Mod2_", c, ".Rdata")
  
  save(TC.Norm, Input, file = save_path)
  message(paste(format(Sys.time(), "%H:%M:%S"), "Data saved at", save_path))
  
}

rm(data, N.washing.cl, N.washing.hh, F.onecycle.washing, F.onecycle.TD,
   F.wear.cl, F.wear.other, f.TD, f.ICLD, f.OCLD, t.outdoor,
   F.onecycle.CLD.vs.washing, f.to.WW, f.to.indoor.air,
   Em.Clothing.IndoorAir, Em.Clothing.OutdoorAir, Em.Clothing.WasteWater,
   Em.HHText.IndoorAir, Em.HHText.OutdoorAir, Em.HHText.WasteWater)
rm(c,comp,comp.type,dest.m1,dest.m2,df_name,f.surface.worn,FemUsers,
   firstcomp,firstrem,Fraction.Flushable.WetWipes,
   ind,k,mat,rem2rem,secondcomp,secondrem,sum.TC.Release,
   theprods,thirdrem,tot2rem,total)
