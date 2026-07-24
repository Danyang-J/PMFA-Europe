##### Calculate overall EF and EM from lifecycle ##################################
library(readr)

env <- c("Incineration", "Landfill", "Sludge amended soil", "Sludge amended soil (MP)", "Release")

# Population table (must exist: excel.file)
Pop <- openxlsx::read.xlsx(paste0("Input/", excel.file), sheet = "GeoCode")
colnames(Pop) <- c("Geo", "ShortGeo", "Pop")

# EF aggregated
to.save.agg <- data.frame(
  country = rep(Systems, each = length(env)),
  dest    = rep(env, times = length(Systems)),
  mean    = 0,
  sd      = 0,
  stringsAsFactors = FALSE
)

# EM aggregated
to.save.em.agg <- data.frame(
  country = rep(Systems, each = length(env)),
  dest    = rep(env, times = length(Systems)),
  mean    = 0,
  sd      = 0,
  stringsAsFactors = FALSE
)

mass.agg <- array(0, dim = c(length(Systems), length(env), SIM))
dimnames(mass.agg) <- list(Systems, env, NULL)

emission.agg <- array(0, dim = c(length(Systems), length(env), SIM))
dimnames(emission.agg) <- list(Systems, env, NULL)

EF.agg <- array(0, dim = c(length(Systems), length(env), SIM))
dimnames(EF.agg) <- list(Systems, env, NULL)

for (c in Systems) {
  load(paste0("Results/Emissions/ProcessedMassEOL_", c, ".Rdata")) # FC.Frac, FC.Mass, Masses
  
  # Total input: input of all polymers; independent of final compartment
  load(paste0("Input/InputReady.Mod2_", c, ".Rdata"))
  
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
    mass.agg[c, dest, ] <- Reduce(`+`, input.pol)
  }
  
  # Total emissions aggregated over pol + prod: [dest, SIM]
  for (dest in env) {
    emission.agg[c, dest, ] <- apply(FC.Mass[dest, , , , drop = FALSE], 4, sum)
  }
  
  # Calculate EF: total emissions / total mass
  for (dest in env) {
    EF.agg[c, dest, ] <- emission.agg[c, dest, ] / mass.agg[c, dest, ]
  }
  
  # EF mean/sd (existing behavior)
  EF.mean <- apply(EF.agg, c(1, 2), mean)
  EF.sd   <- apply(EF.agg, c(1, 2), sd)
  
  for (dest in env) {
    row_index <- which(to.save.agg$country == c &
                         to.save.agg$dest == dest)
    to.save.agg$mean[row_index] <- EF.mean[c, dest]
    to.save.agg$sd[row_index]   <- EF.sd[c, dest]
  }
  
  # EM mean/sd aggregated over pol + prod (env only)
  EM.mean <- apply(emission.agg, c(1, 2), mean)
  EM.sd   <- apply(emission.agg, c(1, 2), sd)
  
  for (dest in env) {
    row_index <- which(to.save.em.agg$country == c &
                         to.save.em.agg$dest == dest)
    to.save.em.agg$mean[row_index] <- EM.mean[c, dest]
    to.save.em.agg$sd[row_index]   <- EM.sd[c, dest]
  }
  
  # --------------------------------------------------------------------------
  # Export EM EOL by env x pol x prod for this country, with mean/sd over SIM
  # + per-capita mean/sd (g/cap)
  # --------------------------------------------------------------------------
  df <- expand.grid(
    dest = env,
    pol  = dimnames(FC.Mass)[[2]],
    prod = dimnames(FC.Mass)[[3]],
    stringsAsFactors = FALSE
  )
  
  tmp <- FC.Mass[env, , , , drop = FALSE]  # [env, pol, prod, SIM]
  
  df$mean <- as.vector(apply(tmp, c(1, 2, 3), mean))
  df$sd   <- as.vector(apply(tmp, c(1, 2, 3), sd))
  
  pop_c <- Pop$Pop[Pop$ShortGeo == c]
  
  df$mean_percap <- df$mean / pop_c * 1e9  # unit: g/cap
  df$sd_percap   <- df$sd   / pop_c * 1e9  # unit: g/cap
  
  write_csv(df, file = paste0("Results/Tables/EM/EM_EOL_prodpol_", c, ".csv"))
  
  message(paste0(format(Sys.time(), "%H:%M:%S"), " Aggregated EF and EM (EOL) calculated for ", c))
}

suffix <- if (identical(Systems, "EU")) "_EU" else ""

# Existing EF export
to.save.agg <- to.save.agg[order(to.save.agg$country, to.save.agg$dest), ]
write.csv(to.save.agg,
          file = paste0("Results/Tables/EF/EF_agg_EOL", suffix, ".csv"),
          row.names = FALSE)

# EM aggregated export (merged over prod + pol)
to.save.em.agg <- to.save.em.agg[order(to.save.em.agg$country, to.save.em.agg$dest), ]
write.csv(to.save.em.agg,
          file = paste0("Results/Tables/EM/EM_agg_EOL", suffix, ".csv"),
          row.names = FALSE)

# EM per capita aggregated export
to.save.em.agg.percap <- to.save.em.agg
to.save.em.agg.percap$mean <- to.save.em.agg.percap$mean /
  Pop$Pop[match(to.save.em.agg.percap$country, Pop$ShortGeo)] * 1e9
to.save.em.agg.percap$sd   <- to.save.em.agg.percap$sd /
  Pop$Pop[match(to.save.em.agg.percap$country, Pop$ShortGeo)] * 1e9

to.save.em.agg.percap <- to.save.em.agg.percap[order(to.save.em.agg.percap$country, to.save.em.agg.percap$dest), ]
write.csv(to.save.em.agg.percap,
          file = paste0("Results/Tables/EM/EM_agg_EOL_percap", suffix, ".csv"),
          row.names = FALSE)

rm(env_levels, end.comp, PC.stop, comp.pre, comp.pre.mass, comp.post,
   EM.mat, EF.mat, env, to.save.agg, to.save.em.agg, mass.agg, emission.agg, EF.agg, input.pol,
   EF.mean, EF.sd, EM.mean, EM.sd, row_index, to.save.em.agg.percap
)
rm(c, comp, comp.post.mass, comp.stop, FC.Frac.List, FC.Mass.List,
   dest, env_name, mat, save_path, suffix)