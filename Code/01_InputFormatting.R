##### INTRO #######################################################################################
library(dplyr)

# Import the initial data
Inp <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "Input")

MFA1 <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "MFA1")
MFA2 <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "MFA2")

EM1  <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "EM1")
EM2  <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "EM2")
EM3  <- openxlsx::read.xlsx(paste0("Input/",excel.file), sheet = "EM3")

Coeff1 <- rbind(MFA1, MFA2)
Coeff2 <- rbind(EM1, EM2, EM3)

# Organize data by country
dfs <- c("Inp", "Coeff1", "Coeff2")

for (df_name in dfs) {
  any_rows <- subset(get(df_name), Country == "any")
  
  for (c in Systems) {
    assign(
      paste0(df_name, "_", c),
      rbind(
        subset(get(df_name), Country == c),
        any_rows
      )
    )
  }
}

# Find all the compartment names
Names <- unique(c(Coeff1[-1, 1], Coeff1[-1, 2],
                  Coeff2[-1, 1], Coeff2[-1, 2]))

# Check if there is missing data (Spread)
bad_rows <- rbind(
  subset(Coeff1, !is.na(Data) & Data != 0 & Data != 1 & Data != "rest" & is.na(Spread),
         select = c("Data", "Spread")),
  subset(Coeff2, !is.na(Data) & Data != 0 & Data != 1 & Data != "rest" & is.na(Spread),
         select = c("Data", "Spread"))
)

bad_rows

### Format input data ##############################################################################

for(c in Systems){
  message(paste(format(Sys.time(), "%H:%M:%S"), "Processing country", c))
  
  Inp_data <- get(paste0("Inp_", c))
  Coeff1_data <- get(paste0("Coeff1_", c))
  Coeff2_data <- get(paste0("Coeff2_", c))
  
  ##### INPUT #######################################################################################
  Input <- import.input(Inp_data, Materials)
  message(paste(format(Sys.time(), "%H:%M:%S"), "Input formatting complete for", c))
  
  ##### TC MODULE 1 #################################################################################
  TC <- import.TC(Coeff1_data, Materials)
  TC <- calc.rest.TC(TC)
  
  # normalization step
  TC.Norm <- sapply(Materials, function(x) NULL)
  for(mat in Materials){
    TC.Norm[[mat]] <- normalize(TC[[mat]])
  }
  message(paste(format(Sys.time(), "%H:%M:%S"), "TC module 1 formatting complete for", c)) 
  
  ##### TC MODULE 2 #################################################################################
  TC.Release <- import.TC(Coeff2_data, Materials)
  message(paste(format(Sys.time(), "%H:%M:%S"), "TC module 2 formatting complete for", c)) 
  
  ##### SAVE DATA ###################################################################################
  save_path <- paste0("Input/InputFormatted_", c, ".Rdata")
  
  save(TC.Norm, TC.Release, Input, file = save_path)
  message(paste(format(Sys.time(), "%H:%M:%S"), "Data saved at", save_path))
}

rm(list = ls(pattern = "^(Coeff1|Coeff2|Inp)"))
rm(bad_rows, any_rows,dfs, MFA1, MFA2, EM1, EM2, EM3)
