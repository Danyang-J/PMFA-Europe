############ Export detailed EF ##################
for (c in Systems) {
  
  load(paste0("Results/Emissions/ProcessedMass_", c, ".Rdata"))
  
  ###### By product categories and polymers ################################
  
  # Total mass: mass of polymer X in product Y; independent of environment
  mass.prod.pol <- array(0,dim = c((length(PC)+2), # prod
                                   length(Materials), # mat
                                   length(env), # dest
                                   SIM))
  dimnames(mass.prod.pol) <- list(c("Pre-consumer processes",PC,"Post-consumer processes"),
                                  Materials, env, NULL)
  
  for (mat in Materials) {
    for (dest in env) {
      mass.prod.pol["Pre-consumer processes",mat,dest,] <- apply(Masses[mat,comp.pre.mass,],2,sum)
      for (prod in PC) {
        mass.prod.pol[prod,mat,dest,] <- Masses[mat, prod, ]
      }
      mass.prod.pol["Post-consumer processes",mat,dest,] <- apply(Masses[mat,comp.post.mass,],2,sum)
    }
  }
  
  # Total emissions: emissions of polymer X from product Y to environment Z
  emission.prod.pol <- array(0,dim = c((length(PC)+2), # prod
                                       length(Materials), # mat
                                       length(env), # dest
                                       SIM))
  dimnames(emission.prod.pol) <- list(c("Pre-consumer processes",PC,"Post-consumer processes"),
                                      Materials, env, NULL)
  
  for (mat in Materials) {
    for (dest in env) {
      for (prod in c("Pre-consumer processes",PC,"Post-consumer processes")) {
        emission.prod.pol[prod,mat,dest,] <- FC.Mass[dest,mat,prod,]
      }
    }
  }
  
  # Calculate EF: total emissions / total mass
  EF.prod.pol <- array(0, dim = dim(emission.prod.pol),
                       dimnames = dimnames(emission.prod.pol))
  
  for (prod in c("Pre-consumer processes",PC,"Post-consumer processes")) {
    for (mat in Materials) {
      for (dest in env) {
        EF.prod.pol[prod, mat, dest, ] <- emission.prod.pol[prod, mat, dest, ] / mass.prod.pol[prod, mat, dest, ]
      }
    }
  }
  
  EF.mean <- apply(EF.prod.pol, c(1,2,3),mean)
  EF.sd <- apply(EF.prod.pol, c(1,2,3),sd)
  
  to.save.prod.pol <- data.frame(
    prod = rep(c("Pre-consumer processes", PC, "Post-consumer processes"),
               each = length(Materials) * length(env)),
    mat  = rep(rep(Materials, each = length(env)), times = length(c("Pre-consumer processes", PC, "Post-consumer processes"))),
    dest = rep(env, times = length(Materials) * length(c("Pre-consumer processes", PC, "Post-consumer processes"))),
    mean = 0,
    sd   = 0,
    stringsAsFactors = FALSE
  )
  
  for (prod in c("Pre-consumer processes", PC, "Post-consumer processes")) {
    for (mat in Materials) {
      for (dest in env) {
        # Find the corresponding row in to.save.prod.pol
        row_index <- which(to.save.prod.pol$prod == prod &
                             to.save.prod.pol$mat  == mat &
                             to.save.prod.pol$dest == dest)
        
        # Assign values from EF.mean and EF.sd
        to.save.prod.pol$mean[row_index] <- EF.mean[prod, mat, dest]
        to.save.prod.pol$sd[row_index]   <- EF.sd[prod, mat, dest]
      }
    }
  }
  
  #to.save.prod.pol$prod[to.save.prod.pol$prod %in% PC] <- 
  #  NiceNames[to.save.prod.pol$prod[to.save.prod.pol$prod %in% PC],"MediumLabel"]
  
  write.csv(to.save.prod.pol, file = paste0("Results/Tables/EF/EF_prodpol_",c,".csv"), row.names = FALSE)
  
  ###### EM by prod and pol ##############################
  
  EM.mean <- apply(emission.prod.pol,c(1,2,3),mean)
  EM.sd <- apply(emission.prod.pol,c(1,2,3),sd)
  
  M.mean <- apply(mass.prod.pol,c(1,2,3),mean)
  M.sd <- apply(mass.prod.pol,c(1,2,3),sd)
  
  EM.percap.mean <- apply(
    emission.prod.pol / Pop$Pop[Pop$ShortGeo==c] * 10^9, # unit: g/cap
    c(1,2,3),mean
  )
  
  EM.percap.sd <- apply(
    emission.prod.pol / Pop$Pop[Pop$ShortGeo==c] * 10^9, # unit: g/cap
    c(1,2,3),sd
  )
  
  to.save.extra <- data.frame(
    prod = rep(c("Pre-consumer processes", PC, "Post-consumer processes"),
               each = length(Materials) * length(env)),
    mat  = rep(rep(Materials, each = length(env)), times = length(c("Pre-consumer processes", PC, "Post-consumer processes"))),
    dest = rep(env, times = length(Materials) * length(c("Pre-consumer processes", PC, "Post-consumer processes"))),
    EM.mean = 0,
    EM.sd   = 0,
    M.mean = 0,
    M.sd = 0,
    EM.percap.mean = 0,
    EM.percap.sd = 0,
    stringsAsFactors = FALSE
  )
  
  for (prod in c("Pre-consumer processes", PC, "Post-consumer processes")) {
    for (mat in Materials) {
      for (dest in env) {
        # Find the corresponding row in to.save.extra
        row_index <- which(to.save.extra$prod == prod &
                             to.save.extra$mat  == mat &
                             to.save.extra$dest == dest)
        
        # Assign values 
        to.save.extra$EM.mean[row_index] <- EM.mean[prod, mat, dest]
        to.save.extra$EM.sd[row_index]   <- EM.sd[prod, mat, dest]
        to.save.extra$M.mean[row_index] <- M.mean[prod, mat, dest]
        to.save.extra$M.sd[row_index]   <- M.sd[prod, mat, dest]
        to.save.extra$EM.percap.mean[row_index] <- EM.percap.mean[prod, mat, dest]
        to.save.extra$EM.percap.sd[row_index] <- EM.percap.sd[prod, mat, dest]
      }
    }
  }
  
  write.csv(to.save.extra, file = paste0("Results/Tables/EM/EM_prodpol_",c,".csv"), row.names = FALSE)
  
  
  ###### By product categories ##############################
  
  # Total mass: mass of product Y; independent of environment
  mass.prod <- array(0,dim = c((length(PC)+2), # prod
                               length(env), # dest
                               SIM))
  dimnames(mass.prod) <- list(c("Pre-consumer processes",PC,"Post-consumer processes"),
                              env, NULL)
  
  for (dest in env) {
    mass.prod["Pre-consumer processes",dest,] <- apply(Masses[,comp.pre.mass,],3,sum)
    for (prod in PC) {
      mass.prod[prod,dest,] <- apply(Masses[,prod,],2,sum)
    }
    mass.prod["Post-consumer processes",dest,] <- apply(Masses[,comp.post.mass,],3,sum)
  }
  
  # Total emissions: emissions from product Y to environment Z
  emission.prod <- array(0,dim = c((length(PC)+2), # prod
                                   length(env), # dest
                                   SIM))
  dimnames(emission.prod) <- list(c("Pre-consumer processes",PC,"Post-consumer processes"),
                                  env, NULL)
  
  for (dest in env) {
    for (prod in c("Pre-consumer processes",PC,"Post-consumer processes")) {
      emission.prod[prod,dest,] <- apply(FC.Mass[dest,,prod,],2,sum)
    }
  }
  
  # Calculate EF: total emissions / total mass
  EF.prod <- array(0, dim = dim(emission.prod),
                   dimnames = dimnames(emission.prod))
  
  for (prod in c("Pre-consumer processes",PC,"Post-consumer processes")) {
    for (dest in env) {
      EF.prod[prod, dest, ] <- emission.prod[prod, dest, ] / mass.prod[prod, dest, ]
    }
  }
  
  EF.mean <- apply(EF.prod, c(1,2),mean)
  EF.sd <- apply(EF.prod, c(1,2),sd)
  
  to.save.prod <- data.frame(
    prod = rep(c("Pre-consumer processes", PC, "Post-consumer processes"),
               each = length(env)),
    dest = rep(env, times = length(c("Pre-consumer processes", PC, "Post-consumer processes"))),
    mean = 0,
    sd   = 0,
    stringsAsFactors = FALSE
  )
  
  for (prod in c("Pre-consumer processes", PC, "Post-consumer processes")) {
    for (dest in env) {
      # Find the corresponding row in to.save.prod
      row_index <- which(to.save.prod$prod == prod &
                           to.save.prod$dest == dest)
      
      # Assign values from EF.mean and EF.sd
      to.save.prod$mean[row_index] <- EF.mean[prod, dest]
      to.save.prod$sd[row_index]   <- EF.sd[prod, dest]
    }
    
  }
  
  write.csv(to.save.prod, file = paste0("Results/Tables/EF/EF_prod_",c,".csv"), row.names = FALSE)
  
  ###### By polymers, from consumption ##############################
  
  # Total consumption: mass of polymer X; independent of environment
  mass.pol <- array(0,dim = c(length(Materials), # mat
                              length(env), # dest
                              SIM))
  dimnames(mass.pol) <- list(Materials,
                             env, NULL)
  
  for (dest in env) {
    for (mat in Materials) {
      mass.pol[mat,dest,] <- apply(Masses[mat,PC,],2,sum)
    }
  }
  
  # Total emissions: emissions of polymer X to environment Z
  emission.pol <- array(0,dim = c(length(Materials), # mat
                                  length(env), # dest
                                  SIM))
  dimnames(emission.pol) <- list(Materials,env, NULL)
  
  for (dest in env) {
    for (mat in Materials) {
      emission.pol[mat,dest,] <- apply(FC.Mass[dest,mat,c(-1,-47),],2,sum)
    }
  }
  
  # Calculate EF: total emissions / total mass
  EF.pol <- array(0, dim = dim(emission.pol),
                  dimnames = dimnames(emission.pol))
  
  for (mat in Materials) {
    for (dest in env) {
      EF.pol[mat, dest, ] <- emission.pol[mat, dest, ] / mass.pol[mat, dest, ]
    }
  }
  
  EF.mean <- apply(EF.pol, c(1,2),mean)
  EF.sd <- apply(EF.pol, c(1,2),sd)
  
  to.save.pol <- data.frame(
    mat = rep(Materials,
              each = length(env)),
    dest = rep(env, times = length(Materials)),
    mean = 0,
    sd   = 0,
    stringsAsFactors = FALSE
  )
  
  for (mat in Materials) {
    for (dest in env) {
      # Find the corresponding row in to.save.pol
      row_index <- which(to.save.pol$mat == mat &
                           to.save.pol$dest == dest)
      
      # Assign values from EF.mean and EF.sd
      to.save.pol$mean[row_index] <- EF.mean[mat, dest]
      to.save.pol$sd[row_index]   <- EF.sd[mat, dest]
    }
    
  }
  
  write.csv(to.save.pol, file = paste0("Results/Tables/EF/EF_pol_cons_",c,".csv"), row.names = FALSE)
  
  ###### By polymers, whole life cycle ##############################
  
  # Total mass: mass of polymer X; independent of environment
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
  
  # Total emissions: emissions of polymer X to environment Z, whole life cycle
  emission.pol <- array(0,dim = c(length(Materials), # mat
                                  length(env), # dest
                                  SIM))
  dimnames(emission.pol) <- list(Materials,env, NULL)
  
  for (dest in env) {
    for (mat in Materials) {
      emission.pol[mat,dest,] <- apply(FC.Mass[dest,mat,,],2,sum)
    }
  }
  
  # Calculate EF: total emissions / total input
  
  EF.pol.lifecycle <- array(0, dim = dim(emission.pol),
                            dimnames = dimnames(emission.pol))
  
  for (mat in Materials) {
    for (dest in env) {
      EF.pol.lifecycle[mat, dest, ] <- emission.pol[mat, dest, ] / input.pol[[mat]]
    }
  }
  
  EF.mean <- apply(EF.pol.lifecycle, c(1,2),mean)
  EF.sd <- apply(EF.pol.lifecycle, c(1,2),sd)
  
  to.save.pol.lifecycle <- data.frame(
    mat = rep(Materials,
              each = length(env)),
    dest = rep(env, times = length(Materials)),
    mean = 0,
    sd   = 0,
    stringsAsFactors = FALSE
  )
  
  for (mat in Materials) {
    for (dest in env) {
      # Find the corresponding row in to.save.pol
      row_index <- which(to.save.pol.lifecycle$mat == mat &
                           to.save.pol.lifecycle$dest == dest)
      
      # Assign values from EF.mean and EF.sd
      to.save.pol.lifecycle$mean[row_index] <- EF.mean[mat, dest]
      to.save.pol.lifecycle$sd[row_index]   <- EF.sd[mat, dest]
    }
  }
  
  write.csv(to.save.pol.lifecycle, file = paste0("Results/Tables/EF/EF_pol_lifecycle_",c,".csv"), row.names = FALSE)
  
  # Calculate EM per capita
  
  EM.percap.mean <- apply(
    emission.pol / Pop$Pop[Pop$ShortGeo==c] * 10^9, # unit: g/cap
    c(1,2),mean
  )
  
  EM.percap.sd <- apply(
    emission.pol / Pop$Pop[Pop$ShortGeo==c] * 10^9, # unit: g/cap
    c(1,2),sd
  ) 
  
  to.save.em.pol.lifecycle <- data.frame(
    mat = rep(Materials,
              each = length(env)),
    dest = rep(env, times = length(Materials)),
    mean = 0,
    sd   = 0,
    stringsAsFactors = FALSE
  )
  
  for (mat in Materials) {
    for (dest in env) {
      # Find the corresponding row in to.save.pol
      row_index <- which(to.save.em.pol.lifecycle$mat == mat &
                           to.save.em.pol.lifecycle$dest == dest)
      
      # Assign values from EF.mean and EF.sd
      to.save.em.pol.lifecycle$mean[row_index] <- EM.percap.mean[mat, dest]
      to.save.em.pol.lifecycle$sd[row_index]   <- EM.percap.sd[mat, dest]
    }
  }
  
  write.csv(to.save.em.pol.lifecycle, file = paste0("Results/Tables/EM/EM_pol_lifecycle_",c,".csv"), row.names = FALSE)
  
  message(paste0(format(Sys.time(), "%H:%M:%S"), " EF and EM exported for ", c))
}

rm(EM.mat,EF.mat,comp.post.mass,env,to.save.agg.cons,to.save.agg.lifecycle,
   mass.agg.cons,mass.agg.lifecycle,emission.agg.cons,emission.agg.lifecycle,
   EF.agg.cons,EF.agg.lifecycle,input.pol,mass.prod.pol,emission.prod.pol,EF.prod.pol,to.save.prod.pol,
   EM.mean,EF.mean,EM.sd,EF.sd,to.save.extra,row_index,
   mass.prod,emission.prod,EF.prod,to.save.prod,
   mass.pol,emission.pol,EF.pol,to.save.pol,EF.pol.lifecycle,to.save.pol.lifecycle,
   to.save.em.pol.lifecycle,EM.percap.mean,EM.percap.sd
)