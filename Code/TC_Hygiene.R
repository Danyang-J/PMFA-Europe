##### INTRO #######################################################################################

message(paste0(format(Sys.time(), "%H:%M:%S")," Calculating flushing flows..."))

Flushing <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "Flushing")

# Organize by countries

any_rows <- subset(Flushing, Country == "any")
results_list <- list()

# import the names of all parameters in the table
param <- sapply(unique(as.matrix(Flushing)[,"ParameterName"]), function(x) NULL)

# product names
Products <- c("DisposableCleaningCloths", "WetWipes", "Tampons",
              "PantyLiners", "SanitaryNapkins", "TamponApplicators")

data <- rbind(subset(Flushing, Country == c), any_rows)
data <- as.data.frame(data, stringsAsFactors = FALSE)
Cons <- sapply(Materials, function(x) NULL)

# total population
Pop <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Population"), "Value"]),
             perc = as.numeric(data[which(data[,"ParameterName"] == "Population"), "Spread"]),
             N = SIM,
             linf = 0)

# number households
NHH <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Households"), "Value"]),
             perc = as.numeric(data[which(data[,"ParameterName"] == "Households"), "Spread"]),
             N = SIM,
             linf = 0)

# calculation of the number of feminine hygiene products users
FemUsers <- Pop * rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Fraction_Female12_54"), "Value"]),
                        perc = as.numeric(data[which(data[,"ParameterName"] == "Fraction_Female12_54"), "Spread"]),
                        N = SIM,
                        linf = 0)

# calculation of the fraction of actual users
FractionUsing.Tampons <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Fraction_UsingTampons"), "Value"]),
                               perc = as.numeric(data[which(data[,"ParameterName"] == "Fraction_UsingTampons"), "Spread"]),
                               N = SIM,
                               linf = 0,
                               lsup = 1)

FractionUsing.SanPads <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Fraction_UsingSanitaryNapkins"), "Value"]),
                               perc = as.numeric(data[which(data[,"ParameterName"] == "Fraction_UsingSanitaryNapkins"), "Spread"]),
                               N = SIM,
                               linf = 0,
                               lsup = 1)

FractionUsing.PantyLin <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Fraction_UsingPantyLiners"), "Value"]),
                                perc = as.numeric(data[which(data[,"ParameterName"] == "Fraction_UsingPantyLiners"), "Spread"]),
                                N = SIM,
                                linf = 0,
                                lsup = 1)

Fraction.Tampons.wApp <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Fraction_Tampons_w_App"), "Value"]),
                               perc = as.numeric(data[which(data[,"ParameterName"] == "Fraction_Tampons_w_App"), "Spread"]),
                               N = SIM,
                               linf = 0,
                               lsup = 1)

Fraction.Flushable.WetWipes <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Share_Flushable_WetWipes"), "Value"]),
                                     perc = as.numeric(data[which(data[,"ParameterName"] == "Share_Flushable_WetWipes"), "Spread"]),
                                     N = SIM,
                                     linf = 0,
                                     lsup = 1)

# calculation of the number of uses per year
Use.Tampons <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Use_Tampons_Year"), "Value"]),
                     perc = as.numeric(data[which(data[,"ParameterName"] == "Use_Tampons_Year"), "Spread"]),
                     N = SIM,
                     linf = 0)

Use.SanPads <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Use_SanitaryNapkins_Year"), "Value"]),
                     perc = as.numeric(data[which(data[,"ParameterName"] == "Use_SanitaryNapkins_Year"), "Spread"]),
                     N = SIM,
                     linf = 0)

Use.PantyLin <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Use_PantyLiners_Year"), "Value"]),
                      perc = as.numeric(data[which(data[,"ParameterName"] == "Use_PantyLiners_Year"), "Spread"]),
                      N = SIM,
                      linf = 0)

Use.CCloths <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Use_DisposableCleaningCloths_Year"), "Value"]),
                     perc = as.numeric(data[which(data[,"ParameterName"] == "Use_DisposableCleaningCloths_Year"), "Spread"]),
                     N = SIM,
                     linf = 0)

Use.WetWipes <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Use_WetWipes_Year"), "Value"]),
                      perc = as.numeric(data[which(data[,"ParameterName"] == "Use_WetWipes_Year"), "Spread"]),
                      N = SIM,
                      linf = 0)

for(mat in Materials){
  
  Cons[[mat]] <- sapply(Products, function(x) NULL)
  
  
  # TAMPONS
  Mass <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Mass_Tampon" & data[,"Material"] %in% c("any", mat)), "Value"]),
                perc = as.numeric(data[which(data[,"ParameterName"] == "Mass_Tampon" & data[,"Material"] %in% c("any", mat)), "Spread"]),
                N = SIM,
                linf = 0)
  Cons[[mat]][["Tampons"]] <- FemUsers * FractionUsing.Tampons * Use.Tampons * Mass / 10^9
  
  
  # CLEANING CLOTHS
  Mass <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Mass_DisposableCleaningCloths" & data[,"Material"] %in% c("any", mat)), "Value"]),
                perc = as.numeric(data[which(data[,"ParameterName"] == "Mass_DisposableCleaningCloths" & data[,"Material"] %in% c("any", mat)), "Spread"]),
                N = SIM,
                linf = 0)
  Cons[[mat]][["DisposableCleaningCloths"]] <- NHH * Use.CCloths * Mass / 10^9
  
  # UNFLUSHABLE WET WIPES (number of wet wipes given for all geographical boundary)
  Mass <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Mass_Wipes" & data[,"Material"] %in% c("any", mat)), "Value"]),
                perc = as.numeric(data[which(data[,"ParameterName"] == "Mass_Wipes" & data[,"Material"] %in% c("any", mat)), "Spread"]),
                N = SIM,
                linf = 0)
  Cons[[mat]][["WetWipes"]] <- Use.WetWipes * (1 - Fraction.Flushable.WetWipes) * Mass / 10^9
  
  # PANTY LINERS
  Mass <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Mass_PantyLiner" & data[,"Material"] %in% c("any", mat)), "Value"]),
                perc = as.numeric(data[which(data[,"ParameterName"] == "Mass_PantyLiner" & data[,"Material"] %in% c("any", mat)), "Spread"]),
                N = SIM,
                linf = 0)
  Cons[[mat]][["PantyLiners"]] <- FemUsers * FractionUsing.PantyLin * Use.PantyLin * Mass / 10^9
  
  # SANITARY PADS
  Mass <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Mass_SanitaryNapkin" & data[,"Material"] %in% c("any", mat)), "Value"]),
                perc = as.numeric(data[which(data[,"ParameterName"] == "Mass_SanitaryNapkin" & data[,"Material"] %in% c("any", mat)), "Spread"]),
                N = SIM,
                linf = 0)
  Cons[[mat]][["SanitaryNapkins"]] <- FemUsers * FractionUsing.SanPads * Use.SanPads * Mass / 10^9
  
  # TAMPON APPLICATORS
  Mass <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "Mass_TamponApplicator" & data[,"Material"] %in% c("any", mat)), "Value"]),
                perc = as.numeric(data[which(data[,"ParameterName"] == "Mass_TamponApplicator" & data[,"Material"] %in% c("any", mat)), "Spread"]),
                N = SIM,
                linf = 0)
  Cons[[mat]][["TamponApplicators"]] <- FemUsers * FractionUsing.Tampons * Fraction.Tampons.wApp * Use.Tampons * Mass / 10^9
  
}

##### CALCULATE TCs ###############################################################################

# two types of products
Household.Products <- c("TamponApplicators")
Textile.Products <- c("DisposableCleaningCloths", "WetWipes", "Tampons",
                      "PantyLiners", "SanitaryNapkins")

##### Textile products ###############################################################################

for(mat in Materials){
  
  # calculate mean flows
  flo <- lapply(TC.Norm[[mat]], function(x) lapply(x,mean))
  inp <- lapply(Input[[mat]], function(x){ if(is.null(x)){ 0 } else { mean(x) } })
  
  # temporarily replace NA values of undefined flows by zeroes (for all special flows which weren't defined yet)
  for(cc in names(flo)){
    for(dd in names(flo[[cc]])){
      if(is.na(flo[[cc]][[dd]])){
        flo[[cc]][[dd]] <- 0
      }
    }
  }
  
  # calculate mean masses
  Mass <- solve.MC(TC.Distr  = flo,
                   inp.Distr = inp,
                   Names     = Names,
                   N         = 1)
  
  comp.type <- "TechnicalTextiles"
  theprods <-  Textile.Products
  
  # check that there is enough mass
  if(any(Mass[comp.type,] - apply(do.call(rbind,Cons[[mat]])[theprods,],2,sum) < -0.000001)){
    
    # find for which occurrences there is not enough mass
    ind <- which(Mass[comp.type,] < apply(do.call(rbind,Cons[[mat]])[theprods,],2,sum))
    
    message(format(Sys.time(), "%H:%M:%S"), " WARNING !\n-------- ", mat,
            ": ", "Not enough mass in ", comp.type,
            " for calculation of hygiene product categories.\n-------- ",
            length(ind)/SIM*100, "% of the data is corrected to account for the difference.")
    
    # replace
    total <- apply(do.call(rbind,Cons[[mat]])[theprods,],2,sum)
    for(comp in names(Cons[[mat]])){
      for(k in ind){
        Cons[[mat]][[comp]][k] <- ( Cons[[mat]][[comp]][k] * Mass[comp.type,] /
                                      total[k]  )
      }
    }
    
  } else if(Mass[comp.type,] == 0 & all(apply(do.call(rbind,Cons[[mat]])[theprods,],2,sum) == 0)){
    next
  }
  
  # calculate TC
  for(comp in theprods){
    TC.Norm[[mat]][[comp.type]][[comp]] <- Cons[[mat]][[comp]]/Mass[comp.type,]
  }
  
  # calculate sum of new TCs
  tot2rem <- apply(do.call(rbind,TC.Norm[[mat]][[comp.type]])[theprods,],2,sum)
  
  # renormalize other TC (first with the closert PC, then the second closest, then the rest)
  if(any(tot2rem != 0)){
    firstcomp <- ifelse(comp.type == "Other", "Household", "HygieneMedicalTextiles")
    secondcomp <- ifelse(comp.type == "Other", "OtherOther", "OtherTechnicalTextiles")
    
    # first removal
    firstrem <- tot2rem
    for(k in 1:length(firstrem)){
      if(firstrem[k] > TC.Norm[[mat]][[comp.type]][[firstcomp]][k]){
        firstrem[k] <- TC.Norm[[mat]][[comp.type]][[firstcomp]][k]
      }
    }
    # remove from first flow
    TC.Norm[[mat]][[comp.type]][[firstcomp]] <- TC.Norm[[mat]][[comp.type]][[firstcomp]] - firstrem
    # determine remaining share to remove
    rem2rem <- tot2rem - firstrem
    
    # second removal
    if(any(rem2rem != 0)){
      secondrem <- rem2rem
      for(k in 1:length(secondrem)){
        if(secondrem[k] > TC.Norm[[mat]][[comp.type]][[secondcomp]][k]){
          secondrem[k] <- TC.Norm[[mat]][[comp.type]][[secondcomp]][k]
        }
      }
      # remove from first flow
      TC.Norm[[mat]][[comp.type]][[secondcomp]] <- TC.Norm[[mat]][[comp.type]][[secondcomp]] - secondrem
      # determine remaining share to remove
      rem2rem <- rem2rem - secondrem
      
      # third and last removal
      if(any(rem2rem != 0)){
        thirdrem <- rem2rem
        total <- do.call(rbind, TC.Norm[[mat]][[comp.type]])
        total <- apply(total[!rownames(total) %in% c(theprods, firstcomp, secondcomp),],2,sum)
        for(comp in names(TC.Norm[[mat]][[comp.type]])){
          if(comp %in% c(theprods, firstcomp, secondcomp)){
            next
          } else {
            TC.Norm[[mat]][[comp.type]][[comp]] <- ( TC.Norm[[mat]][[comp.type]][[comp]]/total * 
                                                       (total-thirdrem) )
          }
        }
      }
    }
  }
  
  # check if normalized
  stopifnot(apply(do.call(rbind,TC.Norm[[mat]][[comp.type]]),2,sum) - 1 < 0.000000001)
}

##### Household products ###############################################################################

for(mat in Materials){
  
  # calculate mean flows
  flo <- lapply(TC.Norm[[mat]], function(x) lapply(x,mean))
  inp <- lapply(Input[[mat]], function(x){ if(is.null(x)){ 0 } else { mean(x) } })
  
  # temporarily replace NA values of undefined flows by zeroes (for all special flows which weren't defined yet)
  for(cc in names(flo)){
    for(dd in names(flo[[cc]])){
      if(is.na(flo[[cc]][[dd]])){
        flo[[cc]][[dd]] <- 0
      }
    }
  }
  
  # calculate mean masses
  Mass <- solve.MC(TC.Distr  = flo,
                   inp.Distr = inp,
                   Names     = Names,
                   N         = 1)
  
  for(comp.type in c("Other")){
    theprods <-  Household.Products
    
    # calculate TC
    for(comp in theprods){
      TC.Norm[[mat]][[comp.type]][[comp]] <- Cons[[mat]][[comp]]/Mass[comp.type,]
    }
    
    # calculate sum of new TCs
    tot2rem <- TC.Norm[[mat]][[comp.type]][[comp]]
    
    # renormalize other TC (first with the closert PC, then the second closest, then the rest)
    if(any(tot2rem != 0)){
      firstcomp <- ifelse(comp.type == "Other", "Household", "HygieneMedicalTextiles")
      secondcomp <- ifelse(comp.type == "Other", "OtherOther", "OtherTechnicalTextiles")
      
      # first removal
      firstrem <- tot2rem
      for(k in 1:length(firstrem)){
        if(firstrem[k] > TC.Norm[[mat]][[comp.type]][[firstcomp]][k]){
          firstrem[k] <- TC.Norm[[mat]][[comp.type]][[firstcomp]][k]
        }
      }
      # remove from first flow
      TC.Norm[[mat]][[comp.type]][[firstcomp]] <- TC.Norm[[mat]][[comp.type]][[firstcomp]] - firstrem
      # determine remaining share to remove
      rem2rem <- tot2rem - firstrem
      
      # second removal
      if(any(rem2rem != 0)){
        secondrem <- rem2rem
        for(k in 1:length(secondrem)){
          if(secondrem[k] > TC.Norm[[mat]][[comp.type]][[secondcomp]][k]){
            secondrem[k] <- TC.Norm[[mat]][[comp.type]][[secondcomp]][k]
          }
        }
        # remove from first flow
        TC.Norm[[mat]][[comp.type]][[secondcomp]] <- TC.Norm[[mat]][[comp.type]][[secondcomp]] - secondrem
        # determine remaining share to remove
        rem2rem <- rem2rem - secondrem
        
        # third and last removal
        if(any(rem2rem != 0)){
          thirdrem <- rem2rem
          total <- do.call(rbind, TC.Norm[[mat]][[comp.type]])
          total <- apply(total[!rownames(total) %in% c(theprods, firstcomp, secondcomp),],2,sum)
          for(comp in names(TC.Norm[[mat]][[comp.type]])){
            if(comp %in% c(theprods, firstcomp, secondcomp)){
              next
            } else {
              TC.Norm[[mat]][[comp.type]][[comp]] <- ( TC.Norm[[mat]][[comp.type]][[comp]]/total * 
                                                         (total-thirdrem) )
            }
          }
        }
      }
    }
    
    # renormalize other TC (same proportions applied to all)
    # for(comp in names(TC.Norm[[mat]][[comp.type]])){
    #   if(comp %in% theprods){ next }
    #   
    #   TC.Norm[[mat]][[comp.type]][[comp]] <- TC.Norm[[mat]][[comp.type]][[comp]]*(1-tot2rem)
    # }
    
    # check if normalized
    stopifnot(apply(do.call(rbind,TC.Norm[[mat]][[comp.type]]),2,sum) - 1 < 0.000000001)
  }
}

# Replace all NA in TC.norm with 0
TC.Norm <- rapply(TC.Norm, function(x) ifelse(is.na(x), 0, x), how = "replace")

rm(data, Mass, cc, dd, flo, Household.Products, Textile.Products,Cons, inp, 
   param, Products, NHH, Pop, Use.CCloths, Use.PantyLin, Use.SanPads, 
   Use.Tampons, Use.WetWipes,Fraction.Tampons.wApp, FractionUsing.PantyLin, 
   FractionUsing.SanPads, FractionUsing.Tampons)
rm(Flushing,any_rows,results_list)