output_dirs <- c(
  "Results/Emissions",
  "Results/Tables/EF",
  "Results/Tables/EM",
  "Results/Tables/Flows",
  "Results/Graphs",
  "Results/Graphs/Maps",
  "Results/Graphs/Maps/Detailed",
  "Charts"
)

invisible(lapply(
  output_dirs,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

excel.file <- "20260618_FeedData.xlsx"

# source needed functions
source("Code/functions.needed.R")
source("Code/functions.needed.analysis.R")

library(openxlsx)
library(trapezoid)

Materials <- c("LDPE","HDPE","PP","PS","EPS","PVC","PET")
Systems.all <- c("AT","BE","BG","HR","CY",
             "CZ","DK","EE","FI","FR",
             "DE","EL","HU","IE","IT",
             "LV","LT","LU","MT","NL",
             "NO","PL","PT","RO","SK",
             "SI","ES","SE","CH","UK")

Systems <- c("AT","BE","BG","HR","CY",
             "CZ","DK","EE","FI","FR",
             "DE","EL","HU","IE","IT",
             "LV","LT","LU","MT","NL",
             "NO","PL","PT","RO","SK",
             "SI","ES","SE","CH","UK")

SIM <- 10^4

color_vector <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
                  "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", 
                  "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 1 definition"))
source("Code/01_InputFormatting.R")
source("Code/02_SpecialFlowsMod1.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 1 done!"))

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 2 definition"))
source("Code/03_SpecialFlowsMod2.R")
source("Code/04_Merging.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Module 2 done!"))

source("Code/05_CalculationScript.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Calculation done!"))

source("Code/06.1_ExportResultsPart1.R")
source("Code/06.2_ExportResultsPart2.R")
source("Code/06.3_ExportResultsPart3.R")

source("Code/07_ExportFlows.R")

source("Code/08.1_ExportResultsEOL_Part1.R")
source("Code/08.2_ExportResultsEOL_Part2.R")
source("Code/08.3_CombineTables.R")
source("Code/08.4_SumEurope.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Results exported as tables."))

# Key visualization
source("Code/09.1_MapVisualization_EM.R")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Results exported as maps."))

source("Code/09.2_HeatMap_Polymer.R")
source("Code/09.2_HeatMap_Product.R")
source("Code/09.3_Country_MP_MaP_Scatter.R")
source("Code/09.3_GDP.R")
source("Code/09.4_CountryHeterogeneity_ByPolymer.R")
source("Code/09.4_CountryHeterogeneity_ByProduct.R")

message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Results exported as graphs"))

source("Code/10_DataForWriting.R")

# TikZ (flow chart)
source("Code/11.1_TikZ_Main.R") # Prepare data
source("Code/11.4_TikZOutput.R") # Draw flow charts
