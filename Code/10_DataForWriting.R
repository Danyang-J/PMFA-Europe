# Calculate aggregated results for writing  #############################################################
load("Input/InputReady.Mod2_EU.Rdata")
load("Results/Emissions/ProcessedMass_EU.Rdata")

env <- c("Soil", "Soil (MP)", "Water", "Water (MP)", 
         "Subsurface (MP)", "Ocean", "Ocean (MP)")

# EF, whole life cycle #############################################################
## 1 ##################################################################################
# Total mass: mass of polymer X; independent of environment

input.pol <- lapply(Materials, function(x) rep(0, SIM))
names(input.pol) <- Materials
for (mat in Materials) {
  input.pol[[mat]] <- Reduce(function(x, y) {
    if (is.null(x)) x <- rep(0, SIM)
    if (is.null(y)) y <- rep(0, SIM)
    x + y
  }, Input[[mat]], init = rep(0, SIM))
}
total_input <- Reduce(`+`, input.pol)

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

total_emission <- apply(emission.pol, c(2,3), sum)

# EF for all polymers by environment
EF.all <- sweep(total_emission, 2, total_input, "/")
EF.all.mean <- apply(EF.all, 1, mean)  # mean for each environment
EF.all.sd   <- apply(EF.all, 1, sd)    # sd for each environment

# Print all environments in "xx ± xx%" format
cat("Aggregate EF across all polymers (by environment):\n")
for(env_name in env) {
  mean_val <- EF.all.mean[env_name] * 100
  sd_val   <- EF.all.sd[env_name] * 100
  cat(sprintf("%s: %.3f ± %.3f%%\n", env_name, mean_val, sd_val))
}

## 2 #############################################################################

# Total emissions by MP vs MaP
emission.MP  <- apply(emission.pol[ , grepl("MP", env), ], 3, sum)    # sum over polymers and MP environments
emission.MaP <- apply(emission.pol[ , !grepl("MP", env), ], 3, sum)   # sum over polymers and non-MP environments

EF.MP  <- emission.MP / total_input
EF.MaP <- emission.MaP / total_input

mean_MP <- mean(EF.MP) * 100
sd_MP   <- sd(EF.MP) * 100
mean_MaP <- mean(EF.MaP) * 100
sd_MaP   <- sd(EF.MaP) * 100
mean_frac_MaP <- mean(emission.MaP/(emission.MP+emission.MaP)*100)
sd_frac_MaP <- sd(emission.MaP/(emission.MP+emission.MaP)*100)

cat(sprintf("Aggregate EF across all polymers, MP environments: %.3f ± %.3f%%\n", mean_MP, sd_MP))
cat(sprintf("Aggregate EF across all polymers, MaP environments: %.3f ± %.3f%%\n", mean_MaP, sd_MaP))
cat(sprintf("Fraction of MaP in total emissions: %.3f ± %.3f%%\n",mean_frac_MaP,sd_frac_MaP))

## 3 ##############################################################################

# total emission across all polymers and environments, for each simulation
total_emission_per_sim <- apply(emission.pol, 3, sum)  # sum over polymer and env

# emission per environment across polymers
emission_per_env <- apply(emission.pol, c(2,3), sum)  # env x SIM

# percentage contribution of each env in each simulation
perc_env <- sweep(emission_per_env, 2, total_emission_per_sim, "/") * 100  # convert to %

# mean and sd for each environment
perc_env.mean <- apply(perc_env, 1, mean)
perc_env.sd   <- apply(perc_env, 1, sd)

# print in "xx ± xx%" format
cat("Percentage of total emissions received by each environment:\n")
for(env_name in env) {
  cat(sprintf("%s: %.3f ± %.3f%%\n", env_name, perc_env.mean[env_name], perc_env.sd[env_name]))
}

## 4 #################################################################################

env_groups <- list(
  Soil  = c("Soil", "Soil (MP)", "Subsurface (MP)"),
  Water = c("Water", "Water (MP)"),
  Ocean = c("Ocean", "Ocean (MP)")
)

perc_group <- matrix(0, nrow = length(env_groups), ncol = SIM,
                     dimnames = list(names(env_groups), NULL))

for(group_name in names(env_groups)) {
  envs <- env_groups[[group_name]]
  # sum over polymers and selected envs
  em_group <- apply(emission.pol[ , envs, , drop=FALSE], 3, sum)
  perc_group[group_name, ] <- em_group / total_emission_per_sim * 100
}

perc_group.mean <- apply(perc_group, 1, mean)
perc_group.sd   <- apply(perc_group, 1, sd)

cat("Percentage of total emissions by aggregated environment:\n")
for(group_name in names(env_groups)) {
  cat(sprintf("%s: %.3f ± %.3f%%\n", group_name, perc_group.mean[group_name], perc_group.sd[group_name]))
}
