# Unit: kt
# For Europe only
# NOTE: This version does NOT use TC.Norm / find.fc().
# It builds EF.mat directly from FC.Mass in ProcessedMass_EU.Rdata.

##### INTRODUCTION ################################################################################
source("Code/functions.needed.analysis.R")
source("Code/functions.needed.sci.not.R")
library(xlsx)

soil.comp <- c("UrbanSoilMicro",
               "UrbanSoilMacro",
               "AgriculturalSoilMacro",
               "AgriculturalSoilMicro",
               "RuralSoilMacro",
               "RuralSoilMicro",
               "RoadSideMacro",
               "DeepUrbanSoilMicro",
               "BeachSoilMacro",
               "BeachSoilMicro")

water.comp <- c("SurfaceWaterMicro",
                "SurfaceWaterMacro",
                "CoastalWaterMicro",
                "CoastalWaterMacro",
                "OceanMacro",
                "OceanMicro")

NiceNames <- as.matrix(read.xlsx(paste0("Input/", excel.file), sheetName = "Label"))
rownames(NiceNames) <- NiceNames[, "Name"]

c <- "EU"

# import EU data
load(paste0("Input/InputReady.Mod2_EU.Rdata"))               # Input (not used here, but keep for compatibility)
load(paste0("Results/Emissions/ProcessedMass_EU.Rdata"))     # FC.Mass, Masses

### SET UP VARIABLES ##############################################################################

# Product/process axis already contains:
# "Pre-consumer processes", all PC, "Post-consumer processes"
prod_levels <- dimnames(FC.Mass)[[3]]
PC <- setdiff(prod_levels, c("Pre-consumer processes", "Post-consumer processes"))

# Build EF.mat directly from FC.Mass
# Target dims: 4 x Materials x prod_levels x SIM
EF.mat <- array(
  0,
  dim = c(4, length(Materials), length(prod_levels), SIM),
  dimnames = list(
    c("Soil", "Soil (MP)", "Water", "Water (MP)"),
    Materials,
    prod_levels,
    NULL
  )
)

# Soil and Soil (MP)
EF.mat["Soil",      , , ] <- FC.Mass["Soil",      , , ]
EF.mat["Soil (MP)", , , ] <- FC.Mass["Soil (MP)", , , ] + FC.Mass["Subsurface soil (MP)", , , ]

# Water (Fresh + Coastal/Ocean)
EF.mat["Water",      , , ] <- FC.Mass["Fresh water",      , , ] +
  FC.Mass["Coastal and ocean water",      , , ]
EF.mat["Water (MP)", , , ] <- FC.Mass["Fresh water (MP)", , , ] +
  FC.Mass["Coastal and ocean water (MP)", , , ]

### BAR PLOT ########################################################################
library(ggplot2)
library(patchwork)
library(dplyr)

plot_family <- "sans"

## Soil MP
cp.order <- names(sort(apply(apply(EF.mat["Soil (MP)",,,], c(2,3), sum), 1, mean)))

lab <- sci.not(apply(EF.mat["Soil (MP)",,,], 3, sum) * 1000 / 1000)  # convert to kt

labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]

mean.cal <- apply(EF.mat["Soil (MP)",,cp.order,], c(1,2), mean) * 1000 / 1000  # convert to kt
sd.cal   <- apply(EF.mat["Soil (MP)",,cp.order,], c(1,2), sd)   * 1000 / 1000  # convert to kt

colnames(mean.cal) <- labs
colnames(sd.cal) <- labs

data <- data.frame(
  polymer = rep(rownames(mean.cal), ncol(mean.cal)),
  prod    = rep(colnames(mean.cal), each = nrow(mean.cal)),
  mean    = as.vector(mean.cal),
  sd      = as.vector(sd.cal)
)

mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
sd_sum   <- aggregate(sd ~ prod, data = data, FUN = sum)
data_sd  <- merge(mean_sum, sd_sum)

data$prod <- with(data, reorder(prod, mean, FUN = sum))
data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))

data.to.print.1 <- data
data.to.print.1$comp <- rep("Soil MP", length(data$polymer))

p1 <- ggplot(data, aes(x = prod, y = mean)) +
  geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  labs(x = "", y = "Mass (kt)", fill = "Polymer") +
  ggtitle("Microplastic emissions to soil") +
  geom_pointrange(data = data_sd, aes(ymin = mean - sd, ymax = mean + sd),
                  colour = "black", alpha = 0.9, size = 0.2) +
  ylim(0, NA) +
  coord_flip() +
  annotate("text", x = -Inf, y = Inf, label = paste0("Σ = ", lab, " kt"), hjust = 1.05, vjust = -0.7) +
  theme(text = element_text(family = plot_family),
        axis.text = element_text(color = "black"))

p1

####### Soil MaP
cp.order <- names(sort(apply(apply(EF.mat["Soil",,,], c(2,3), sum), 1, mean)))

lab <- sci.not(apply(EF.mat["Soil",,,], 3, sum) * 1000 / 1000)

labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]

mean.cal <- apply(EF.mat["Soil",,cp.order,], c(1,2), mean) * 1000 / 1000
sd.cal   <- apply(EF.mat["Soil",,cp.order,], c(1,2), sd)   * 1000 / 1000

colnames(mean.cal) <- labs
colnames(sd.cal) <- labs

data <- data.frame(
  polymer = rep(rownames(mean.cal), ncol(mean.cal)),
  prod    = rep(colnames(mean.cal), each = nrow(mean.cal)),
  mean    = as.vector(mean.cal),
  sd      = as.vector(sd.cal)
)

mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
sd_sum   <- aggregate(sd ~ prod, data = data, FUN = sum)
data_sd  <- merge(mean_sum, sd_sum)

data$prod <- with(data, reorder(prod, mean, FUN = sum))
data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))

data.to.print.2 <- data
data.to.print.2$comp <- rep("Soil MaP", length(data$polymer))

p2 <- ggplot(data, aes(x = prod, y = mean)) +
  geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  labs(x = "", y = "Mass (kt)", fill = "Polymer") +
  ggtitle("Macroplastic emissions to soil") +
  geom_pointrange(data = data_sd, aes(ymin = mean - sd, ymax = mean + sd),
                  colour = "black", alpha = 0.9, size = 0.2) +
  ylim(0, NA) +
  coord_flip() +
  annotate("text", x = -Inf, y = Inf, label = paste0("Σ = ", lab, " kt"), hjust = 1.05, vjust = -0.7) +
  theme(text = element_text(family = plot_family),
        axis.text = element_text(color = "black"))

p2

####### Water MP
cp.order <- names(sort(apply(apply(EF.mat["Water (MP)",,,], c(2,3), sum), 1, mean)))

lab <- sci.not(apply(EF.mat["Water (MP)",,,], 3, sum) * 1000 / 1000)

labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]

mean.cal <- apply(EF.mat["Water (MP)",,cp.order,], c(1,2), mean) * 1000 / 1000
sd.cal   <- apply(EF.mat["Water (MP)",,cp.order,], c(1,2), sd)   * 1000 / 1000

colnames(mean.cal) <- labs
colnames(sd.cal) <- labs

data <- data.frame(
  polymer = rep(rownames(mean.cal), ncol(mean.cal)),
  prod    = rep(colnames(mean.cal), each = nrow(mean.cal)),
  mean    = as.vector(mean.cal),
  sd      = as.vector(sd.cal)
)

mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
sd_sum   <- aggregate(sd ~ prod, data = data, FUN = sum)
data_sd  <- merge(mean_sum, sd_sum)

data_sd_setlimit <- data_sd %>%
  mutate(sd = if_else(data_sd$mean > data_sd$sd, sd, mean))

data$prod <- with(data, reorder(prod, mean, FUN = sum))
data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))

data.to.print.3 <- data
data.to.print.3$comp <- rep("Water MP", length(data$polymer))

p3 <- ggplot(data, aes(x = prod, y = mean)) +
  geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  labs(x = "", y = "Mass (kt)", fill = "Polymer") +
  ggtitle("Microplastic emissions to water") +
  geom_pointrange(data = data_sd_setlimit, aes(ymin = mean - sd, ymax = mean + sd),
                  colour = "black", alpha = 0.9, size = 0.2) +
  ylim(0, NA) +
  coord_flip() +
  annotate("text", x = -Inf, y = Inf, label = paste0("Σ = ", lab, " kt"), hjust = 1.05, vjust = -0.7) +
  theme(text = element_text(family = plot_family),
        axis.text = element_text(color = "black"))

p3

####### Water MaP
cp.order <- names(sort(apply(apply(EF.mat["Water",,,], c(2,3), sum), 1, mean)))

lab <- sci.not(apply(EF.mat["Water",,,], 3, sum) * 1000 / 1000)

labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]

mean.cal <- apply(EF.mat["Water",,cp.order,], c(1,2), mean) * 1000 / 1000
sd.cal   <- apply(EF.mat["Water",,cp.order,], c(1,2), sd)   * 1000 / 1000

colnames(mean.cal) <- labs
colnames(sd.cal) <- labs

data <- data.frame(
  polymer = rep(rownames(mean.cal), ncol(mean.cal)),
  prod    = rep(colnames(mean.cal), each = nrow(mean.cal)),
  mean    = as.vector(mean.cal),
  sd      = as.vector(sd.cal)
)

mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
sd_sum   <- aggregate(sd ~ prod, data = data, FUN = sum)
data_sd  <- merge(mean_sum, sd_sum)

data_sd_setlimit <- data_sd %>%
  mutate(sd = if_else(data_sd$mean > data_sd$sd, sd, mean))

data$prod <- with(data, reorder(prod, mean, FUN = sum))
data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))

data.to.print.4 <- data
data.to.print.4$comp <- rep("Water MaP", length(data$polymer))

p4 <- ggplot(data, aes(x = prod, y = mean)) +
  geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  labs(x = "", y = "Mass (kt)", fill = "Polymer") +
  ggtitle("Macroplastic emissions to water") +
  geom_pointrange(data = data_sd_setlimit, aes(ymin = mean - sd, ymax = mean + sd),
                  colour = "black", alpha = 0.9, size = 0.2) +
  coord_flip() +
  ylim(0, NA) +
  annotate("text", x = -Inf, y = Inf, label = paste0("Σ = ", lab, " kt"), hjust = 1.05, vjust = -0.7) +
  theme(text = element_text(family = plot_family),
        axis.text = element_text(color = "black"))

p4

save <- p1 + p2 + p3 + p4 +
  plot_layout(guides = "collect")

ggsave(paste0("EmissionsByProd_", c, ".png"),
       plot = save,
       path = "Results/Graphs/EmissionsByProd/",
       width = 9.5, height = 6,
       dpi = 500)

data.to.print <- rbind(data.to.print.1,
                       data.to.print.2,
                       data.to.print.3,
                       data.to.print.4)
data.to.print <- data.to.print[, c("prod", "polymer", "comp", "mean", "sd")]
data.to.print <- data.to.print[order(data.to.print$prod), ]
write.xlsx(data.to.print, paste0("Results/Tables/EmissionsByProd_", c, ".xlsx"))

message(paste0(format(Sys.time(), "%H:%M:%S"),
               " Graph and results saved for: ", c))

rm(data, data_sd, data_sd_setlimit, data.to.print, data.to.print.1, data.to.print.2,
   data.to.print.3, data.to.print.4,
   mean_sum, mean.cal, p1, p2, p3, p4, save, sd_sum, sd.cal,
   c, comp, cp.order, EF.mat, lab, labs, mat, PC, prod_levels,
   soil.comp, water.comp)
