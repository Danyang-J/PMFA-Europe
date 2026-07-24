##### Combine tables ##################################
library(readr)
library(writexl)
library(stringr)

data_list <- list()
path <- c("Results/Tables/EF/EF_pol_cons_",
          "Results/Tables/EF/EF_pol_lifecycle_",
          "Results/Tables/EF/EF_prod_",
          "Results/Tables/EF/EF_prodpol_",
          "Results/Tables/EM/EM_pol_lifecycle_",
          "Results/Tables/EM/EM_prodpol_",
          "Results/Tables/EM/EM_EOL_prodpol_")

for (p in path) {
  for (c in Systems) {
    df <- read_csv(paste0(p, c, ".csv"))
    data_list[[c]] <- df
  }
  write_xlsx(data_list, path = paste0(p, "all.xlsx"))
}

rm(data_list, df, c, p, path)