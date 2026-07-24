# Exact per-capita heatmap summaries from saved Monte Carlo results.
# Each standard deviation is calculated after aggregating the relevant flows
# within every simulation, thereby retaining covariance among components.

load_heatmap_exact_emissions <- function(Systems, prod_groups = NULL,
                                         excel.file = "20260618_FeedData.xlsx") {
  geo <- openxlsx::read.xlsx(file.path("Input", excel.file), sheet = "GeoCode")
  pop <- setNames(geo[["Population.2020"]], geo[["Abbreviation"]])
  missing_pop <- setdiff(Systems, names(pop))
  if (length(missing_pop) > 0) {
    stop("Population is missing for: ", paste(missing_pop, collapse = ", "))
  }

  phase_destinations <- list(
    MaP = c("Soil", "Fresh water", "Coastal and ocean water"),
    MP  = c("Soil (MP)", "Fresh water (MP)", "Coastal and ocean water (MP)",
            "Subsurface soil (MP)")
  )

  polymer_rows <- list()
  sector_rows <- list()
  total_rows <- list()
  row_id <- 0L
  sector_id <- 0L
  total_id <- 0L

  for (country in Systems) {
    result_env <- new.env(parent = emptyenv())
    load(file.path("Results", "Emissions", paste0("ProcessedMass_", country, ".Rdata")),
         envir = result_env)
    fc_mass <- result_env$FC.Mass
    polymers <- dimnames(fc_mass)[[2]]
    products <- dimnames(fc_mass)[[3]]
    scale_percap <- 1e9 / pop[[country]]

    for (phase in names(phase_destinations)) {
      destinations <- phase_destinations[[phase]]
      total_sim <- apply(fc_mass[destinations, , , , drop = FALSE], 4, sum) * scale_percap
      total_id <- total_id + 1L
      total_rows[[total_id]] <- data.frame(
        country = country, phase = phase,
        tot_mean = mean(total_sim), tot_sd = stats::sd(total_sim)
      )

      for (mat in polymers) {
        emission_sim <- apply(fc_mass[destinations, mat, , , drop = FALSE], 4, sum) * scale_percap
        row_id <- row_id + 1L
        polymer_rows[[row_id]] <- data.frame(
          country = country, phase = phase, mat = mat,
          EM.mean = mean(emission_sim), EM.sd = stats::sd(emission_sim)
        )
      }

      if (!is.null(prod_groups)) {
        for (sector in names(prod_groups)) {
          included_products <- intersect(prod_groups[[sector]], products)
          emission_sim <- if (length(included_products) == 0L) {
            rep(0, dim(fc_mass)[4])
          } else {
            apply(fc_mass[destinations, , included_products, , drop = FALSE], 4, sum)
          }
          sector_id <- sector_id + 1L
          sector_rows[[sector_id]] <- data.frame(
            country = country, phase = phase, sector = sector,
            EM.mean = mean(emission_sim * scale_percap),
            EM.sd = stats::sd(emission_sim * scale_percap)
          )
        }
      }
    }
    rm(result_env, fc_mass)
    gc(verbose = FALSE)
  }

  list(
    polymer = do.call(rbind, polymer_rows),
    sector = if (length(sector_rows) > 0L) do.call(rbind, sector_rows) else NULL,
    total = do.call(rbind, total_rows)
  )
}
