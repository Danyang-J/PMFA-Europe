# import input data
load(paste0("Input/InputReady.Mod2_",c,".Rdata"))
source("Code/functions.needed.analysis.R")
source("Code/functions.needed.sci.not.R")
library(xlsx)

# names of module 2
Rank <- as.matrix(read.xlsx(paste0("Input/",excel.file), sheetName = "Label"))

stopifnot(all(Names %in% Rank[,"Name"]))

# create empty lists for aggregated flows
Flow.List <- Flow.agg <- sapply(Materials, function(x) vector("list"))

# 
PC <- unique(c(names(TC.Norm[["PP"]][["Packaging"]]),
               names(TC.Norm[["PP"]][["BuildingConstruction"]]),
               names(TC.Norm[["PP"]][["Automotive"]]),
               names(TC.Norm[["PP"]][["EEE"]]),
               names(TC.Norm[["PP"]][["Agriculture"]]),
               names(TC.Norm[["PP"]][["Other"]]),
               names(TC.Norm[["PP"]][["Apparel"]]),
               names(TC.Norm[["PP"]][["HouseholdTextiles"]]),
               names(TC.Norm[["PP"]][["TechnicalTextiles"]]),
               names(TC.Norm[["PP"]][["FisheryTextiles"]]),
               names(TC.Norm[["PP"]][["AquacultureTextiles"]]),
               names(TC.Norm[["PP"]][["Fishery"]]),
               names(TC.Norm[["PP"]][["Aquaculture"]])))
PC <- PC[!PC == "Export"]

for(mat in Materials){
  
  # import output data
  load(paste0("Results/Emissions/OutputMass_",c,"_",mat,".Rdata"))
  
  # create empty list for import
  Flow.agg[[mat]][["Import"]] <- list()
  Flow.agg[[mat]][["Production"]] <- list()
  
  # abbreviations for shorter code in the loop
  TC <- TC.Norm[[mat]]
  Inp <- Input[[mat]]
  
  for(comp in names(Inp)){
    if(is.null(Inp[[comp]])){
      Inp[[comp]] <- rep(0,SIM)
    }
  }
  
  ### pre-consumer stuff
  comp.stop <- c(PC,
                 "PCPlasticCollection", "PCFibreCollection",
                 "IndustryWaterMicro", "WasteWaterMicro",
                 "SurfaceWaterMicro", "UrbanSoilMicro", "RoadSideMicro",
                 "ExcludedFlow")
  
  Flow.agg[[mat]][["Import"]][["Pre-consumer processes"]] <- ( Inp[["NonTextileManufacturing"]] +
                                                                 Inp[["TextileManufacturing"]] )  # No transport is considered in "Inp"
  
  Flow.agg[[mat]][["Production"]][["Pre-consumer processes"]] <- ( Inp[["PrimaryProduction"]] +
                                                                     Inp[["RecyclateRepelletizing"]] )
  
  Flow.agg[[mat]][["Pre-consumer processes"]] <- 
    agg.fc(list(find.fc(comp = "PrimaryProduction", mass = Inp[["PrimaryProduction"]],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "RecyclateRepelletizing", mass = Inp[["RecyclateRepelletizing"]],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "NonTextileManufacturing", mass = Inp[["NonTextileManufacturing"]],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "TextileManufacturing", mass = Inp[["TextileManufacturing"]],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop)))
  
  ### packaging
  # consumer packaging
  Flow.agg[[mat]][["Consumer packaging"]] <- 
    agg.fc(list(mult.tc(TC[["ConsumerFilms"]], Mass["ConsumerFilms",]),
                mult.tc(TC[["ConsumerBags"]], Mass["ConsumerBags",]),
                mult.tc(TC[["ConsumerBottles"]], Mass["ConsumerBottles",]),
                mult.tc(TC[["ConsumerOther"]], Mass["ConsumerOther",])))
  
  # non-consumer packaging
  Flow.agg[[mat]][["Non-consumer packaging"]] <- 
    agg.fc(list(mult.tc(TC[["OtherNonConsumerFilms"]], Mass["OtherNonConsumerFilms",]),
                mult.tc(TC[["NonConsumerBags"]], Mass["NonConsumerBags",]),
                mult.tc(TC[["NonConsumerOther"]], Mass["NonConsumerOther",])))
  
  ### construction
  Flow.agg[[mat]][["Construction"]] <- 
    agg.fc(list(mult.tc(TC[["PipesDucts"]], Mass["PipesDucts",]),
                mult.tc(TC[["Insulation"]], Mass["Insulation",]),
                mult.tc(TC[["WallFloorCoverings"]], Mass["WallFloorCoverings",]),
                mult.tc(TC[["WindowsProfilesFittedFurniture"]], Mass["WindowsProfilesFittedFurniture",]),
                mult.tc(TC[["Lining"]], Mass["Lining",]),
                mult.tc(TC[["BuildingPackagingFilms"]], Mass["BuildingPackagingFilms",]),
                mult.tc(TC[["BuildingTextiles"]], Mass["BuildingTextiles",]),
                mult.tc(TC[["Geotextiles"]], Mass["Geotextiles",])))
  
  ### automotive
  Flow.agg[[mat]][["Automotive"]] <- 
    agg.fc(list(mult.tc(TC[["AutomotivePC"]], Mass["AutomotivePC",]),
                mult.tc(TC[["MobilityTextiles"]], Mass["MobilityTextiles",])))

  ### agriculture
  Flow.agg[[mat]][["Agriculture"]] <- 
    agg.fc(list(mult.tc(TC[["AgriculturalFilms"]], Mass["AgriculturalFilms",]),
                mult.tc(TC[["AgriculturalPipes"]], Mass["AgriculturalPipes",]),
                mult.tc(TC[["AgriculturalOther"]], Mass["AgriculturalOther",]),
                mult.tc(TC[["Agrotextiles"]], Mass["Agrotextiles",]),
                mult.tc(TC[["AgriculturalPackagingFilms"]], Mass["AgriculturalPackagingFilms",]),
                mult.tc(TC[["AgriculturalPackagingBottles"]], Mass["AgriculturalPackagingBottles",])))
  
  ### hygiene products
  Flow.agg[[mat]][["Hygiene products"]] <- 
    agg.fc(list(mult.tc(TC[["DisposableCleaningCloths"]], Mass["DisposableCleaningCloths",]),
                mult.tc(TC[["WetWipes"]], Mass["WetWipes",]),
                mult.tc(TC[["Tampons"]], Mass["Tampons",]),
                mult.tc(TC[["PantyLiners"]], Mass["PantyLiners",]),
                mult.tc(TC[["SanitaryNapkins"]], Mass["SanitaryNapkins",]),
                mult.tc(TC[["TamponApplicators"]], Mass["TamponApplicators",])))
  
  ### cosmetics
  Flow.agg[[mat]][["PCCP"]] <- mult.tc(TC[["Cosmetics"]], Mass["Cosmetics",])
  
  ### textiles
  
  # clothing
  Flow.agg[[mat]][["Clothing"]] <- 
    agg.fc(list(mult.tc(TC[["ApparelPC"]], Mass["ApparelPC",]),
                mult.tc(TC[["TechnicalClothing"]], Mass["TechnicalClothing",])))

  # household textiles
  Flow.agg[[mat]][["Household textiles"]] <- 
    agg.fc(list(mult.tc(TC[["HouseholdTextilesPC"]], Mass["HouseholdTextilesPC",]),
                mult.tc(TC[["TechnicalHouseholdTextiles"]], Mass["TechnicalHouseholdTextiles",])))

  ### other products
  Flow.agg[[mat]][["Other products"]] <- 
    agg.fc(list(mult.tc(TC[["Household"]], Mass["Household",]),
                mult.tc(TC[["Furniture"]], Mass["Furniture",]),
                mult.tc(TC[["FabricCoatings"]], Mass["FabricCoatings",]),
                mult.tc(TC[["OtherOther"]], Mass["OtherOther",]),
                mult.tc(TC[["HygieneMedicalTextiles"]], Mass["HygieneMedicalTextiles",]),
                mult.tc(TC[["OtherTechnicalTextiles"]], Mass["OtherTechnicalTextiles",]),
                mult.tc(TC[["EEEPC"]], Mass["EEEPC",]),
                mult.tc(TC[["ShotgunCartridges"]], Mass["ShotgunCartridges",])))

  ### Fishing gear
  Flow.agg[[mat]][["Fishing gear"]] <- 
    agg.fc(list(find.fc(comp = "FishingGearOcean", mass = Mass["FishingGearOcean",],
                         TC.Distr = TC, verbose = F, stop.at = "FishingGearCollection"),
                find.fc(comp = "FishingGearInland", mass = Mass["FishingGearInland",],
                                      TC.Distr = TC, verbose = F, stop.at = "FishingGearCollection"),
                find.fc(comp = "FishingGearAqua", mass = Mass["FishingGearAqua",],
                         TC.Distr = TC, verbose = F, stop.at = "FishingGearCollection")))
  
  ### waste collection, recycling and waste management
  comp.stop <- c("IndoorAirMicro", "OutdoorAirMicro",
                 "IndustryWaterMicro", "WasteWaterMicro", "StormWaterMicro",
                 "UrbanSoilMicro", "UrbanSoilMacro",
                 "UrbanLitter", "RuralLitter", "RoadSideLitter","Landfill","Elimination")
  
  Flow.agg[[mat]][["Waste collection and recycling"]] <- 
    agg.fc(list(find.fc(comp = "PCPlasticCollection", mass = Mass["PCPlasticCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "PCFibreCollection", mass = Mass["PCFibreCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "PackagingCollection", mass = Mass["PackagingCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "MixedCollection", mass = Mass["MixedCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "CDCollection", mass = Mass["CDCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "CDIncinerableCollection", mass = Mass["CDIncinerableCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "ELVCollection", mass = Mass["ELVCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "ELVTextilesCollection", mass = Mass["ELVTextilesCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "WEEECollection", mass = Mass["WEEECollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "AgricultureCollection", mass = Mass["AgricultureCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "TextileCollection", mass = Mass["TextileCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop),
                find.fc(comp = "FishingGearCollection", mass = Mass["FishingGearCollection",],
                        TC.Distr = TC, verbose = F, stop.at = comp.stop)))
  
  ### emission pathways
  comp.stop <- c("UrbanSoilMicro", "DeepUrbanSoilMicro",
                 "MixedCollection", "SurfaceWaterMacro","SurfaceWaterMicro",
                 "CoastalWaterMacro","CoastalWaterMicro",
                 "SludgeAmendedSoilMacro","SludgeAmendedSoilMicro",
                 "CompostMacro","CompostMicro",
                 "Landfill","Elimination")
  
  Flow.agg[[mat]][["Storm water and wastewater"]] <- 
    agg.fc(list( find.fc(comp = "WasteWaterMacro", mass = Mass["WasteWaterMacro",],
                         TC.Distr = TC, verbose = F, stop.at = comp.stop),
                 find.fc(comp = "StormWaterMacro", mass = Mass["StormWaterMacro",],
                         TC.Distr = TC, verbose = F, stop.at = comp.stop)))
  
  Flow.agg[[mat]][["Storm water and wastewater (MP)"]] <- 
    agg.fc(list( find.fc(comp = "IndustryWaterMicro", mass = Mass["IndustryWaterMicro",],
                         TC.Distr = TC, verbose = F, stop.at = comp.stop),
                 find.fc(comp = "WasteWaterMicro", mass = Mass["WasteWaterMicro",],
                         TC.Distr = TC, verbose = F, stop.at = comp.stop),
                 find.fc(comp = "StormWaterMicro", mass = Mass["StormWaterMicro",],
                         TC.Distr = TC, verbose = F, stop.at = comp.stop)))
                 
  Flow.agg[[mat]][["On-the-go consumption"]] <-
    find.fc(comp = "OnTheGo", mass = Mass["OnTheGo",],
            TC.Distr = TC, verbose = F,
            stop.at = c("UrbanLitter", "RoadSideLitter", "RuralLitter", "BeachLitter", 
                        "MixedCollection"))

  Flow.agg[[mat]][["Litter"]] <-
    agg.fc(list( find.fc(comp = "UrbanLitter", mass = Mass["UrbanLitter",],
                         TC.Distr = TC, verbose = F, stop.at = c("MixedCollection","StormWaterMacro")),
                 find.fc(comp = "RoadSideLitter", mass = Mass["RoadSideLitter",],
                         TC.Distr = TC, verbose = F, stop.at = c("MixedCollection","StormWaterMacro")),
                 find.fc(comp = "RuralLitter", mass = Mass["RuralLitter",],
                         TC.Distr = TC, verbose = F, stop.at = c("MixedCollection","StormWaterMacro")),
                 find.fc(comp = "BeachLitter", mass = Mass["BeachLitter",],
                         TC.Distr = TC, verbose = F, stop.at = c("MixedCollection","StormWaterMacro"))))
                 
  # Flow.agg[[mat]][["Dumping"]] <- 
  #   find.fc(comp = "Dumping", mass = Mass["Dumping",],
  #           TC.Distr = TC, verbose = F,
  #           stop.at = c("UrbanLitter", "RoadSideLitter", "RuralLitter","BeachLitter"))
  # 
  Flow.agg[[mat]][["Dumping"]] <- 
    mult.tc(TC[["Dumping"]], Mass["Dumping",])
  
  Flow.agg[[mat]][["Unintended C&D waste"]] <- 
    mult.tc(TC[["UnintendedCDWaste"]], Mass["UnintendedCDWaste",])
  
  Flow.agg[[mat]][["Indoor air"]] <-
    find.fc(comp = "IndoorAirMicro", mass = Mass["IndoorAirMicro",],
            TC.Distr = TC, verbose = F,
            stop.at = c("OutdoorAirMicro", "WasteWaterMicro", "MixedCollection"))
  
  Flow.agg[[mat]][["Air"]] <-
    find.fc(comp = "OutdoorAirMicro", mass = Mass["OutdoorAirMicro",],
            TC.Distr = TC, verbose = F)
  
  Flow.agg[[mat]][["Organic waste collection"]] <-
    agg.fc(list(find.fc(comp = "CompostCollectionLarge", mass = Mass["CompostCollectionLarge",],
                        stop.at = c("CompostMicro", "CompostMacro"),
                        TC.Distr = TC, verbose = F),
                find.fc(comp = "CompostCollectionSmall", mass = Mass["CompostCollectionSmall",],
                        stop.at = c("CompostMicro", "CompostMacro"),
                        TC.Distr = TC, verbose = F)))
  
  Flow.agg[[mat]][["Compost"]] <-
    find.fc(comp = "CompostMacro", mass = Mass["CompostMacro",],
            TC.Distr = TC, verbose = F)
  
  Flow.agg[[mat]][["Compost (MP)"]] <-
    find.fc(comp = "CompostMicro", mass = Mass["CompostMicro",],
            TC.Distr = TC, verbose = F)
  
  ##### AGGREGATE COMPARTMENTS
  
  Flow.List[[mat]] <- agg.flows(Flows = Flow.agg[[mat]],
                                comps = list("Pre-consumer processes" = c("PrimaryProduction", "RecyclateRepelletizing",
                                                                          "Transport", "FibreProduction", 
                                                                          "NonTextileManufacturing", "TextileManufacturing"),
                                             "Consumer packaging" = c("ConsumerFilms", "ConsumerBags",
                                                                      "ConsumerBottles", "ConsumerOther"),
                                             "Non-consumer packaging" = c("OtherNonConsumerFilms", "NonConsumerBags",
                                                                          "NonConsumerOther"),
                                             "Construction" = c("PipesDucts", "Insulation", "WallFloorCoverings",
                                                                "WindowsProfilesFittedFurniture", "Lining",
                                                                "BuildingPackagingFilms", "BuildingTextiles", "Geotextiles"),
                                             "Automotive" = c("AutomotivePC", "MobilityTextiles"),
                                             "Agriculture" = c("AgriculturalFilms", "AgriculturalPipes", "AgriculturalOther",
                                                               "Agrotextiles", "AgriculturalPackagingFilms",
                                                               "AgriculturalPackagingBottles"),
                                             "Hygiene products" = c("DisposableCleaningCloths", "WetWipes",
                                                                    "Tampons", "PantyLiners", "SanitaryNapkins",
                                                                    "TamponApplicators"),
                                             "PCCP" = "Cosmetics",
                                             "Clothing" = c("ApparelPC", "TechnicalClothing"),
                                             "Household textiles" = c("HouseholdTextilesPC", "TechnicalHouseholdTextiles"),
                                             "Other products" = c("Household", "Furniture",
                                                                  "FabricCoatings", "OtherOther", "HygieneMedicalTextiles",
                                                                  "OtherTechnicalTextiles", "EEEPC", "ShotgunCartridges"),
                                             "Fishing gear" = c("FishingGearAqua", "FishingGearInland", "FishingGearOcean"),
                                             "Waste collection and recycling" = c("PCPlasticCollection", "PCFibreCollection",
                                                                                  "PackagingCollection", "MixedCollection",
                                                                                  "CDCollection", "CDIncinerableCollection",
                                                                                  "ELVCollection", "ELVTextilesCollection",
                                                                                  "WEEECollection", "AgricultureCollection",
                                                                                  "TextileCollection", "FishingGearCollection"),
                                             "Reuse" = c("MaterialReuse", "PartReuse", "TextileReuse"),
                                             "End-of-life" = "Elimination",
                                             "Storm water and wastewater" = c("WasteWaterMacro",
                                                                              "StormWaterMacro"),
                                             "Storm water and wastewater (MP)" = c("WasteWaterMicro", "IndustryWaterMicro",
                                                                                   "StormWaterMicro"),
                                             "Soil" = c("AgriculturalSoilMacro",
                                                        "UrbanSoilMacro", 
                                                        "RoadSideMacro",
                                                        "RuralSoilMacro",
                                                        "BeachSoilMacro"),
                                             "Soil (MP)" = c("AgriculturalSoilMicro",
                                                             "UrbanSoilMicro", 
                                                             "DeepUrbanSoilMicro",
                                                             "RoadSideMicro",
                                                             "RuralSoilMicro",
                                                             "BeachSoilMicro"),
                                             "Sludge amended soil" = "SludgeAmendedSoilMacro",
                                             "Sludge amended soil (MP)" = "SludgeAmendedSoilMicro",
                                             "Fresh water" = "SurfaceWaterMacro",
                                             "Fresh water (MP)" = "SurfaceWaterMicro",
                                             "Coastal and ocean water" = c("CoastalWaterMacro",
                                                                           "OceanMacro"),
                                             "Coastal and ocean water (MP)" = c("CoastalWaterMicro",
                                                                                "OceanMicro"),
                                             "Indoor air (MP)" = "IndoorAirMicro",
                                             "Air (MP)" = "OutdoorAirMicro",
                                             "Organic waste collection" = c("CompostCollectionLarge", "CompostCollectionSmall"),
                                             "Compost" = c("CompostMacro"),
                                             "Compost (MP)" = c("CompostMicro"),
                                             "On-the-go consumption" = c("OnTheGo"),
                                             "Litter" = c("UrbanLitter", "RoadSideLitter", "RuralLitter", "BeachLitter")))
  
  stopifnot(
    ## Litter
    mean(Mass["UrbanLitter",] + Mass["RoadSideLitter",] + Mass["RuralLitter",] + Mass["BeachLitter",]) -
      sum(sapply(Flow.agg[[mat]][["Litter"]], mean)) < 0.000000001,
    ## Consumer packaging
    mean(Mass["ConsumerFilms",] + Mass["ConsumerBags",] + Mass["ConsumerBottles",] + Mass["ConsumerOther",]) -
      sum(sapply(Flow.agg[[mat]][["Consumer packaging"]], mean)) < 0.000000001
  )
  
  # add total flows to the environment
  Flow.List[[mat]][["Total"]][["Soil"]] <- apply(Mass[c("AgriculturalSoilMacro",
                                                        "UrbanSoilMacro",
                                                        "RoadSideMacro",
                                                        "RuralSoilMacro",
                                                        "BeachSoilMacro"),],2,sum)
  Flow.List[[mat]][["Total"]][["Soil (MP)"]] <- apply(Mass[c("AgriculturalSoilMicro",
                                                             "UrbanSoilMicro", 
                                                             "DeepUrbanSoilMicro",
                                                             "RuralSoilMicro"),],2,sum)
  Flow.List[[mat]][["Total"]][["Fresh water"]] <- Mass["SurfaceWaterMacro",]
  Flow.List[[mat]][["Total"]][["Fresh water (MP)"]] <- Mass["SurfaceWaterMicro",]
  Flow.List[[mat]][["Total"]][["Coastal and ocean water"]] <- apply(Mass[c("CoastalWaterMacro",
                                                                           "OceanMacro"),],2,sum)
  Flow.List[[mat]][["Total"]][["Coastal and ocean water (MP)"]] <- apply(Mass[c("CoastalWaterMicro",
                                                                                "OceanMicro"),],2,sum)
  Flow.List[[mat]][["Total"]][["Sludge amended soil"]] <- Mass["SludgeAmendedSoilMacro",]
  Flow.List[[mat]][["Total"]][["Sludge amended soil (MP)"]] <- Mass["SludgeAmendedSoilMicro",]
  Flow.List[[mat]][["Total"]][["Landfill"]] <- Mass["Landfill",]
}
