library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)
source("Code/heatmap_exact_uncertainty.R")

# ------------------------------------------------------------------------------
# Countries to include in the heatmaps
# ------------------------------------------------------------------------------
Systems <- c("AT","BE","BG","HR","CY",
             "CZ","DK","EE","FI","FR",
             "DE","EL","HU","IE","IT",
             "LV","LT","LU","MT","NL",
             "NO","PL","PT","RO","SK",
             "SI","ES","SE","CH","UK",
             "EU")

Systems.fullname <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                      "Czech Republic","Denmark","Estonia","Finland","France",
                      "Germany","Greece","Hungary","Ireland","Italy",
                      "Latvia","Lithuania","Luxembourg","Malta","Netherlands",
                      "Norway","Poland","Portugal","Romania","Slovakia",
                      "Slovenia","Spain","Sweden","Switzerland","United Kingdom",
                      "Europe")

# ------------------------------------------------------------------------------
# Settings (Systems must be defined)
# ------------------------------------------------------------------------------
prod_groups <- list(
  "Pre-consumer processes"  = "Pre-consumer processes",
  "Post-consumer processes" = "Post-consumer processes",
  "Consumer packaging"      = c("ConsumerFilms", "ConsumerBags",
                                "ConsumerBottles", "ConsumerOther"),
  "Non-consumer packaging"  = c("OtherNonConsumerFilms", "NonConsumerBags",
                                "NonConsumerOther"),
  "Construction"            = c("PipesDucts", "Insulation", "WallFloorCoverings",
                                "WindowsProfilesFittedFurniture", "Lining",
                                "BuildingPackagingFilms", "BuildingTextiles", "Geotextiles"),
  "Automotive"              = c("AutomotivePC", "MobilityTextiles"),
  "Agriculture"             = c("AgriculturalFilms", "AgriculturalPipes", "AgriculturalOther",
                                "Agrotextiles", "AgriculturalPackagingFilms",
                                "AgriculturalPackagingBottles"),
  "Hygiene products"        = c("DisposableCleaningCloths", "WetWipes",
                                "Tampons", "PantyLiners", "SanitaryNapkins",
                                "TamponApplicators"),
  "PCCP"                    = "Cosmetics",
  "Clothing"                = c("ApparelPC", "TechnicalClothing"),
  "Household textiles"      = c("HouseholdTextilesPC", "TechnicalHouseholdTextiles"),
  "Other products"          = c("Household", "Furniture",
                                "FabricCoatings", "OtherOther", "HygieneMedicalTextiles",
                                "OtherTechnicalTextiles", "EEEPC", "ShotgunCartridges"),
  "Fishing gear"            = c("FishingGearAqua", "FishingGearInland", "FishingGearOcean")
)

sector_levels <- names(prod_groups)

sector_abbrev <- c(
  "Pre-consumer processes"  = "Pre-c",
  "Post-consumer processes" = "Post-c",
  "Consumer packaging"      = "C-pack",
  "Non-consumer packaging"  = "N-pack",
  "Construction"            = "Constr",
  "Automotive"              = "Auto",
  "Agriculture"             = "Agri",
  "Hygiene products"        = "Hygiene",
  "PCCP"                    = "PCCP",
  "Clothing"                = "Cloth",
  "Household textiles"      = "HH Text",
  "Other products"          = "Other",
  "Fishing gear"            = "Fish"
)

# prod -> sector map
prod_to_sector <- imap_dfr(prod_groups, ~ tibble(prod = .x, sector = .y))

# ------------------------------------------------------------------------------
# Exact aggregation: sum flows within each Monte Carlo iteration, then calculate
# the mean and standard deviation. This retains covariance across pathways.
# ------------------------------------------------------------------------------
heatmap_exact <- load_heatmap_exact_emissions(Systems, prod_groups = prod_groups)
df_sector <- heatmap_exact$sector %>%
  mutate(sector = factor(sector, levels = sector_levels))
df_phase_total <- heatmap_exact$total

# ------------------------------------------------------------------------------
# Heatmap builder (one phase): heatmap + top/right bar plots
# ------------------------------------------------------------------------------
make_heatmap <- function(df_sector, df_phase_total, phase_target,
                         Systems, Systems.fullname,
                         sector_abbrev) {
  
  fmt_mean_sd <- function(m, s){
    paste0(
      sub("\\.$", "", formatC(signif(m, 3), format = "fg", digits = 3, flag = "#")),
      " \u00B1 ",
      sub("\\.$", "", formatC(signif(s, 2), format = "fg", digits = 2, flag = "#"))
    )
  }
  
  df0 <- df_sector %>%
    filter(phase == phase_target) %>%
    mutate(country_full = dplyr::recode(country, !!!setNames(Systems.fullname, Systems)))
  
  # Drop sectors with total==0 (within this phase)
  keep_sector <- df0 %>%
    group_by(sector) %>%
    summarise(tot = sum(EM.mean, na.rm = TRUE), .groups = "drop") %>%
    filter(tot != 0) %>%
    pull(sector)
  df0 <- df0 %>% filter(sector %in% keep_sector)
  
  # Order countries: desc total -> largest at BOTTOM
  ord_country <- df0 %>%
    group_by(country_full) %>%
    summarise(tot = sum(EM.mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(tot)) %>%
    pull(country_full)
  y_levels_display <- rev(ord_country)  # largest at TOP
  
  # Order sectors: desc total -> largest at LEFT
  ord_sector <- df0 %>%
    group_by(sector) %>%
    summarise(tot = sum(EM.mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(tot)) %>%
    pull(sector) %>%
    as.character()
  
  # bar colors: MaP #C4A3E6, MP #9ED8F0
  bar_col <- if (phase_target == "MP") "#9ED8F0" else "#C4A3E6"
  eu_red  <- "#FB8072"
  
  # Heatmap data
  df_plot <- df0 %>%
    mutate(
      country_full = factor(country_full, levels = y_levels_display),
      sector       = factor(as.character(sector), levels = ord_sector),
      lab_mean = ifelse(
        EM.mean == 0,
        "0",
        sub("\\.$", "", formatC(signif(EM.mean, 3), format = "fg", digits = 3, flag = "#"))
      )
    )
  
  # White text on very dark tiles
  cutoff <- stats::quantile(df_plot$EM.mean, probs = 0.95, na.rm = TRUE)
  df_plot <- df_plot %>%
    mutate(text_col = ifelse(EM.mean >= cutoff, "white", "grey20"))
  
  # Exact country totals for the right bar
  df_country_tot <- df_phase_total %>%
    filter(phase == phase_target) %>%
    mutate(
      country_full = dplyr::recode(country, !!!setNames(Systems.fullname, Systems)),
      country_full = factor(country_full, levels = y_levels_display)
    ) %>%
    mutate(
      lab = fmt_mean_sd(tot_mean, tot_sd),
      bar_fill = ifelse(as.character(country_full) == "Europe", eu_red, bar_col)
    )
  
  # palettes
  pal_map <- c("#F8F5FB", "#C4A3E6", "#9A6FD6", "#6F3CC3", "#4B1688", "#2E0B57")
  pal_mp  <- c("#F4FCFF", "#D6EFFA", "#9ED8F0", "#5BB7E3", "#0094C8", "#00558C", "#00388C")
  pal <- if (phase_target == "MP") pal_mp else pal_map
  
  # EU highlight row (heatmap)
  eu_rect <- NULL
  if ("Europe" %in% y_levels_display) {
    eu_y <- which(y_levels_display == "Europe")
    eu_rect <- data.frame(
      xmin = 0.5,
      xmax = length(ord_sector) + 0.5,
      ymin = eu_y - 0.5,
      ymax = eu_y + 0.5
    )
  }
  
  border_lw <- 0.8
  
  # -------- heatmap ----------
  p_heat <- ggplot(df_plot, aes(x = sector, y = country_full, fill = EM.mean)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = lab_mean, color = text_col), size = 3.2) +
    scale_color_identity() +
    { if (!is.null(eu_rect)) geom_rect(
      data = eu_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = NA, color = eu_red, linewidth = border_lw
    ) } +
    scale_fill_gradientn(colors = pal) +
    scale_x_discrete(labels = sector_abbrev, position = "top") +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      axis.text.x.bottom  = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      axis.title.x        = element_blank(),
      axis.text.x.top     = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      panel.grid  = element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      panel.border = element_rect(color = "black", fill = NA, linewidth = border_lw)
    )
  
  # -------- right bar ----------
  p_right <- ggplot(df_country_tot, aes(y = country_full, x = tot_mean)) +
    geom_col(aes(fill = bar_fill), height = 0.85) +
    scale_fill_identity() +
    geom_errorbarh(
      aes(xmin = pmax(0, tot_mean - tot_sd), xmax = tot_mean + tot_sd),
      height = 0.2, linewidth = 0.3
    ) +
    geom_text(
      aes(x = tot_mean + tot_sd, label = lab),
      hjust = 0,
      nudge_x = 0.1 * max(df_country_tot$tot_mean + df_country_tot$tot_sd, na.rm = TRUE),
      size = 3.2, color = "grey20"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
    ) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = border_lw) +  # bottom
    annotate("segment", x = -Inf, xend = Inf, y =  Inf, yend =  Inf, linewidth = border_lw) +  # top
    annotate("segment", x =  Inf, xend = Inf, y = -Inf, yend =  Inf, linewidth = border_lw) +  # right
    scale_x_continuous(expand = expansion(mult = c(0, 1.0)))
  
  # Combine (2 panels)
  p_heat + p_right + plot_layout(ncol = 2, widths = c(1, 0.25))
}

# ------------------------------------------------------------------------------
# Build plots
# ------------------------------------------------------------------------------
p_map <- wrap_elements(
  full = make_heatmap(df_sector, df_phase_total, "MaP", Systems, Systems.fullname, sector_abbrev) +
    plot_annotation(title = "Macroplastic emissions per capita (g/cap)") &
    theme(plot.title = element_text(size = 11, hjust = 0.5, margin = margin(b = 8)))
)

p_mp <- wrap_elements(
  full = make_heatmap(df_sector, df_phase_total, "MP", Systems, Systems.fullname, sector_abbrev) +
    plot_annotation(title = "Microplastic emissions per capita (g/cap)") &
    theme(plot.title = element_text(size = 11, hjust = 0.5, margin = margin(b = 8)))
)

p_final <- (p_map | plot_spacer() | p_mp) +
  plot_layout(widths = c(1, 0.005, 1))

# Save combined
ggsave(
  filename = "Results/Graphs/Heatmap_percap_Total_MaP_MP.png",
  plot = p_final, width = 16, height = 10
)

# Save separately
ggsave(
  filename = "Results/Graphs/Heatmap_percap_Total_MaP.png",
  plot = p_map, width = 8, height = 9
)

ggsave(
  filename = "Results/Graphs/Heatmap_percap_Total_MP.png",
  plot = p_mp, width = 8, height = 9
)
