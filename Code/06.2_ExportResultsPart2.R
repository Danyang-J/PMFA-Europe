Pop <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "GeoCode")
colnames(Pop) <- c("Geo","ShortGeo","Pop")

############ Export overall EF #######################################################

env <- c("Soil", "Soil (MP)", "Fresh water", "Fresh water (MP)", 
         "Subsurface soil (MP)", "Coastal and ocean water", "Coastal and ocean water (MP)")

#### Store overall EF from consumption ####
to.save.agg.cons <- data.frame(
  country = rep(Systems,times = length(env)),
  dest = rep(env, times = length(Systems)),
  mean = 0,
  sd   = 0,
  stringsAsFactors = FALSE
)

mass.agg.cons <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(mass.agg.cons) <- list(Systems,env, NULL)

emission.agg.cons <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(emission.agg.cons) <- list(Systems,env, NULL)

EF.agg.cons <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(EF.agg.cons) <- list(Systems,env, NULL)

#### Store overall EF from whole lifecycle ####
to.save.agg.lifecycle <- data.frame(
  country = rep(Systems,times = length(env)),
  dest = rep(env, times = length(Systems)),
  mean = 0,
  sd   = 0,
  stringsAsFactors = FALSE
)

mass.agg.lifecycle <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(mass.agg.lifecycle) <- list(Systems,env, NULL)

emission.agg.lifecycle <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(emission.agg.lifecycle) <- list(Systems,env, NULL)

EF.agg.lifecycle <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(EF.agg.lifecycle) <- list(Systems,env, NULL)

#### Store emissions from whole lifecycle ####
to.save.em.lifecycle <- data.frame(
  country = rep(Systems,times = length(env)),
  dest = rep(env, times = length(Systems)),
  mean = 0,
  sd   = 0,
  stringsAsFactors = FALSE
)

emission.lifecycle <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(emission.lifecycle) <- list(Systems,env, NULL)

##

to.save.em.percap.lifecycle <- data.frame(
  country = rep(Systems,times = length(env)),
  dest = rep(env, times = length(Systems)),
  mean = 0,
  sd   = 0,
  stringsAsFactors = FALSE
)

emission.pp.lifecycle <- array(0, dim = c(length(Systems),length(env),SIM))
dimnames(emission.pp.lifecycle) <- list(Systems,env, NULL)

#### Calculation and export ####
for (c in Systems) {
  load(paste0("Results/Emissions/ProcessedMass_", c, ".Rdata"))
  
  ###### Overall emissions, from consumption ###################################
  
  # Total consumption: mass of all polymers; independent of environment
  for (dest in env) {
    mass.agg.cons[c,dest,] <- apply(Masses[,PC,],3,sum)
  }
  
  # Total emissions: emissions of all polymers to environment Z, from consumption
  for (dest in env) {
    emission.agg.cons[c,dest,] <- apply(FC.Mass[dest,,c(-1,-47),],3,sum)
  }
  
  # Calculate EF: total emissions / total mass
  for (dest in env) {
    EF.agg.cons[c, dest, ] <- emission.agg.cons[c, dest, ] / mass.agg.cons[c, dest, ]
  }
  
  EF.mean <- apply(EF.agg.cons, c(1,2),mean)
  EF.sd <- apply(EF.agg.cons, c(1,2),sd)
  
  for (dest in env) {
    # Find the corresponding row in to.save.agg
    row_index <- which(to.save.agg.cons$country == c &
                         to.save.agg.cons$dest == dest)
    
    # Assign values from EF.mean and EF.sd
    to.save.agg.cons$mean[row_index] <- EF.mean[c, dest]
    to.save.agg.cons$sd[row_index]   <- EF.sd[c, dest]
  }
  
  ###### Overall emissions, from lifecycle #####################################
  
  # Total input: input of all polymers; independent of environment
  load(paste0("Input/InputReady.Mod2_",c,".Rdata"))
  
  input.pol <- lapply(Materials, function(x) rep(0, SIM))
  names(input.pol) <- Materials
  for (mat in Materials) {
    input.pol[[mat]] <- Reduce(function(x, y) {
      if (is.null(x)) x <- rep(0, SIM)
      if (is.null(y)) y <- rep(0, SIM)
      x + y
    }, Input[[mat]], init = rep(0, SIM))
  }
  
  for (dest in env) {
    mass.agg.lifecycle[c,dest,] <- Reduce(`+`, input.pol)
  }  
  
  # Total emissions: emissions of all polymers to environment Z, whole life cycle
  
  for (dest in env) {
    emission.agg.lifecycle[c,dest,] <- apply(FC.Mass[dest,,,],3,sum)
  }
  
  # Calculate EF: total emissions / total input
  
  for (dest in env) {
    EF.agg.lifecycle[c,dest, ] <- emission.agg.lifecycle[c,dest,] / mass.agg.lifecycle[c,dest,]
  }
  
  EF.mean <- apply(EF.agg.lifecycle, c(1,2),mean)
  EF.sd <- apply(EF.agg.lifecycle, c(1,2),sd)
  
  for (dest in env) {
    # Find the corresponding row in to.save.agg
    row_index <- which(to.save.agg.lifecycle$country == c &
                         to.save.agg.lifecycle$dest == dest)
    
    # Assign values from EF.mean and EF.sd
    to.save.agg.lifecycle$mean[row_index] <- EF.mean[c, dest]
    to.save.agg.lifecycle$sd[row_index]   <- EF.sd[c, dest]
  }
  
  # Calculate emissions per capita
  
  for (dest in env) {
    emission.pp.lifecycle[c,dest,] <- 
      emission.agg.lifecycle[c,dest,] / Pop$Pop[Pop$ShortGeo==c] * 10^9 # unit: g/cap
  }
  
  EM.percap.mean <- apply(emission.pp.lifecycle, c(1,2),mean)
  EM.percap.sd <- apply(emission.pp.lifecycle, c(1,2),sd)
  
  for (dest in env) {
    # Find the corresponding row in to.save.agg
    row_index <- which(to.save.em.percap.lifecycle$country == c &
                         to.save.em.percap.lifecycle$dest == dest)
    
    # Assign values from EF.mean and EF.sd
    to.save.em.percap.lifecycle$mean[row_index] <- EM.percap.mean[c, dest]
    to.save.em.percap.lifecycle$sd[row_index]   <- EM.percap.sd[c, dest]
  }
  
  ## Calculate aggregated total emissions
  
  for (dest in env) {
    emission.lifecycle[c,dest,] <- emission.agg.lifecycle[c,dest,] # unit: kt
  }
  
  EM.mean <- apply(emission.lifecycle, c(1,2), mean)
  EM.sd <- apply(emission.lifecycle, c(1,2), sd)
  
  for (dest in env) {
    # Find the corresponding row in to.save.agg
    row_index <- which(to.save.em.lifecycle$country == c &
                         to.save.em.lifecycle$dest == dest)
    
    # Assign values from EF.mean and EF.sd
    to.save.em.lifecycle$mean[row_index] <- EM.mean[c, dest]
    to.save.em.lifecycle$sd[row_index]   <- EM.sd[c, dest]
  }
  
  message(paste0(format(Sys.time(), "%H:%M:%S"), " Aggregated EF and EM calculated for ",c))
}

to.save.agg.cons <- to.save.agg.cons[order(to.save.agg.cons$country, to.save.agg.cons$dest), ]
write.csv(to.save.agg.cons, file = paste0("Results/Tables/EF/EF_agg_cons.csv"), row.names = FALSE)

to.save.agg.lifecycle <- to.save.agg.lifecycle[order(to.save.agg.lifecycle$country, to.save.agg.lifecycle$dest), ]
write.csv(to.save.agg.lifecycle, file = paste0("Results/Tables/EF/EF_agg_lifecycle.csv"), row.names = FALSE)

to.save.em.lifecycle <- to.save.em.lifecycle[order(to.save.em.lifecycle$country, to.save.em.lifecycle$dest), ]
write.csv(to.save.em.lifecycle, file = paste0("Results/Tables/EM/EM_agg_lifecycle.csv"), row.names = FALSE)

to.save.em.percap.lifecycle <- to.save.em.percap.lifecycle[order(to.save.em.percap.lifecycle$country, to.save.em.percap.lifecycle$dest), ]
write.csv(to.save.em.percap.lifecycle, file = paste0("Results/Tables/EM/EM_agg_percap_lifecycle.csv"), row.names = FALSE)
