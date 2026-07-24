# Initialize EU totals  ########################################################
Systems <- Systems.all[[1]]

load(paste0("Input/InputReady.Mod2_", Systems, ".Rdata")) # Input, TC.Norm
load(paste0("Results/Emissions/ProcessedMass_", Systems, ".Rdata")) # FC.Frac, FC.Mass, Masses

Input.EU   <- Input
FC.Mass.EU <- FC.Mass
Masses.EU  <- Masses

# Background  ##################################################################
env_levels <- list(
  "Soil"                          = c("UrbanSoilMacro","RuralSoilMacro","RoadSideMacro",
                                      "AgriculturalSoilMacro","BeachSoilMacro"),
  "Soil (MP)"                     = c("UrbanSoilMicro","RuralSoilMicro",
                                      "AgriculturalSoilMicro","BeachSoilMicro"),
  "Fresh water"                   = c("SurfaceWaterMacro"), 
  "Fresh water (MP)"              = c("SurfaceWaterMicro"),  
  "Coastal and ocean water"       = c("OceanMacro","CoastalWaterMacro"), 
  "Coastal and ocean water (MP)"  = c("OceanMicro","CoastalWaterMicro"), 
  "Subsurface soil (MP)"          = c("DeepUrbanSoilMicro")
)

env <- c("Soil", "Soil (MP)", "Fresh water", "Fresh water (MP)", 
         "Subsurface soil (MP)", 
         "Coastal and ocean water", "Coastal and ocean water (MP)")

end.comp <- c("UrbanSoilMicro", "UrbanSoilMacro",
              "AgriculturalSoilMacro", "AgriculturalSoilMicro",
              "DeepUrbanSoilMicro",
              "RuralSoilMacro", "RuralSoilMicro",
              "RoadSideMacro", "BeachSoilMicro", "BeachSoilMacro",
              "SurfaceWaterMacro", "SurfaceWaterMicro",
              "CoastalWaterMicro","CoastalWaterMacro",
              "OceanMicro","OceanMacro")

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

# Sum up masses with the recursive add function  ###############################
for (c in Systems.all[-1]) {
  load(paste0("Input/InputReady.Mod2_", c, ".Rdata"))
  load(paste0("Results/Emissions/ProcessedMass_", c, ".Rdata"))
  
  Input.EU   <- recursive_add(Input.EU, Input)
  FC.Mass.EU <- recursive_add(FC.Mass.EU, FC.Mass)
  Masses.EU  <- recursive_add(Masses.EU, Masses)
}

# Save EU totals  ##############################################################
Input <- Input.EU
FC.Mass <- FC.Mass.EU
Masses <- Masses.EU

save(Input, file = "Input/InputReady.Mod2_EU.Rdata")
save(FC.Mass, Masses, file = "Results/Emissions/ProcessedMass_EU.Rdata")

# EU EOL ##########################################################################
# -------------------------------------------------------------------------
# Build and save EU totals for EOL (same logic as your lifecycle EU totals)
# Output: Results/Emissions/ProcessedMassEOL_EU.Rdata
# -------------------------------------------------------------------------

# Initialize EU totals (EOL) ---------------------------------------------------
Systems <- Systems.all[[1]]

load(paste0("Results/Emissions/ProcessedMassEOL_", Systems, ".Rdata")) # FC.Frac, FC.Mass, Masses

FC.Mass.EU <- FC.Mass
Masses.EU  <- Masses

for (c in Systems.all[-1]) {
  load(paste0("Results/Emissions/ProcessedMassEOL_", c, ".Rdata"))
  
  FC.Mass.EU <- recursive_add(FC.Mass.EU, FC.Mass)
  Masses.EU  <- recursive_add(Masses.EU, Masses)
}

# Save EU totals (EOL) ---------------------------------------------------------
FC.Mass <- FC.Mass.EU
Masses  <- Masses.EU

save(FC.Mass, Masses, file = "Results/Emissions/ProcessedMassEOL_EU.Rdata")

# Export EFs  #############################################################
Systems <- c("EU")
source("Code/06.3_ExportResultsPart3.R")
source("Code/08.2_ExportResultsEOL_Part2.R")
Systems <- Systems.all

rm(c,dest,FC.Mass.EU,Masses.EU,Input.EU,TC.Norm,prod,mat)
