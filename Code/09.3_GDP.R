library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(openxlsx)
library(readr)
library(scales)
library(patchwork)

# ------------------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------------------

excel_path <- file.path("Input", excel.file)

Systems <- c("AT","BE","BG","HR","CY",
             "CZ","DK","EE","FI","FR",
             "DE","EL","HU","IE","IT",
             "LV","LT","LU","MT","NL",
             "NO","PL","PT","RO","SK",
             "SI","ES","SE","CH","UK",
             "EU")

input_dir_em <- "Results/Tables/EM"

color_vector <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
                  "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
                  "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

eu_red <- "#FB8072"

quad_pal <- c(
  "High GDP / High EM" = color_vector[10],
  "High GDP / Low EM"  = color_vector[5],
  "Low GDP / High EM"  = color_vector[6],
  "Low GDP / Low EM"   = color_vector[1],
  "Unclassified"       = color_vector[9]
)

# Legend categories to show (hide Unclassified in legend only)
legend_quads <- setdiff(names(quad_pal), "Unclassified")

# Visual tuning
eu_linewidth      <- 0.8
bar_linewidth     <- 2.6
bar_alpha_non_eu  <- 0.50
bar_alpha_eu      <- 0.75
label_size        <- 3.1
legend_point_size <- 4.2

# plot point sizes (doubled)
point_size_non_eu <- 3.5
point_size_eu     <- 4.0

# ------------------------------------------------------------------------------
# 1) Read Pop + GDP per capita (directly, including EU row)
# ------------------------------------------------------------------------------

geo_raw <- openxlsx::read.xlsx(excel_path, sheet = "GeoCode")

df_pop_gdp <- geo_raw %>%
  transmute(
    country_name = as.character(.data[["Country.or.region"]]),
    country      = as.character(.data[["Abbreviation"]]),
    Pop          = readr::parse_number(as.character(.data[["Population.2020"]])),
    GDP_pc_EUR   = readr::parse_number(as.character(.data[["GDP.per.cap"]]))
  ) %>%
  filter(country %in% Systems)

# ------------------------------------------------------------------------------
# 2) Read EM per capita, split to MaP/MP, aggregate within country
# ------------------------------------------------------------------------------

df_em_pc <- purrr::map_dfr(Systems, function(cty) {
  read.csv(file.path(input_dir_em, paste0("EM_prodpol_", cty, ".csv")), check.names = FALSE) %>%
    transmute(
      country    = cty,
      dest       = as.character(.[[3]]),
      em_pc_mean = readr::parse_number(as.character(.[[8]])),
      em_pc_sd   = readr::parse_number(as.character(.[[9]]))
    )
}) %>%
  mutate(
    phase = ifelse(grepl(" \\(MP\\)$", dest), "MP", "MaP")
  ) %>%
  group_by(country, phase) %>%
  summarise(
    EM_pc_mean = sum(em_pc_mean, na.rm = TRUE),
    EM_pc_sd   = sqrt(sum(em_pc_sd^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 3) Build plot by phase (MaP / MP)
# ------------------------------------------------------------------------------

make_gdp_phase_plot <- function(phase_target) {
  df_compare <- df_em_pc %>%
    filter(phase == phase_target) %>%
    select(-phase) %>%
    inner_join(df_pop_gdp, by = "country") %>%
    filter(is.finite(GDP_pc_EUR), is.finite(EM_pc_mean))

  if (nrow(df_compare) == 0) {
    stop(paste0("No rows left for phase ", phase_target, " after merging/filtering."))
  }

  df_eu <- df_compare %>% filter(country == "EU")
  if (nrow(df_eu) != 1) {
    stop(paste0("EU row missing or duplicated for phase ", phase_target, "."))
  }

  eu_x <- df_eu$GDP_pc_EUR[1]
  eu_y <- df_eu$EM_pc_mean[1]

  df_q <- df_compare %>%
    mutate(
      quadrant = case_when(
        !is.finite(eu_x) | !is.finite(eu_y) ~ "Unclassified",
        !is.finite(GDP_pc_EUR) | !is.finite(EM_pc_mean) ~ "Unclassified",
        GDP_pc_EUR >= eu_x & EM_pc_mean >= eu_y ~ "High GDP / High EM",
        GDP_pc_EUR >= eu_x & EM_pc_mean <  eu_y ~ "High GDP / Low EM",
        GDP_pc_EUR <  eu_x & EM_pc_mean >= eu_y ~ "Low GDP / High EM",
        TRUE                                     ~ "Low GDP / Low EM"
      ),
      quadrant = factor(quadrant, levels = names(quad_pal)),
      size_sd_raw = EM_pc_sd
    )

  min_pos_sd <- df_q %>%
    summarise(m = min(size_sd_raw[is.finite(size_sd_raw) & size_sd_raw > 0], na.rm = TRUE)) %>%
    pull(m)
  if (!is.finite(min_pos_sd)) min_pos_sd <- 1

  df_q <- df_q %>%
    mutate(
      size_sd = ifelse(is.finite(size_sd_raw) & size_sd_raw > 0, size_sd_raw, min_pos_sd)
    )

  y_rng <- diff(range(df_q$EM_pc_mean, na.rm = TRUE))
  label_nudge_y <- 0.015 * y_rng

  df_bar <- df_q %>%
    mutate(
      em_sd_plot = ifelse(is.finite(size_sd_raw) & size_sd_raw >= 0, size_sd_raw, NA_real_),
      ymin = EM_pc_mean - em_sd_plot,
      ymax = EM_pc_mean + em_sd_plot
    )

  df_bar_non_eu <- df_bar %>% filter(country != "EU")
  df_bar_eu     <- df_bar %>% filter(country == "EU")

  x_breaks <- pretty(c(0, max(df_bar$GDP_pc_EUR, na.rm = TRUE)), n = 7)
  y_breaks <- pretty(c(0, max(c(df_bar$ymax, df_bar$EM_pc_mean), na.rm = TRUE)), n = 6)

  ggplot(df_bar, aes(x = GDP_pc_EUR, y = EM_pc_mean)) +
    geom_vline(
      xintercept = eu_x,
      linewidth = eu_linewidth,
      colour = eu_red,
      alpha = 0.85
    ) +
    geom_hline(
      yintercept = eu_y,
      linewidth = eu_linewidth,
      colour = eu_red,
      alpha = 0.85
    ) +

    geom_segment(
      data = dplyr::filter(df_bar_non_eu, is.finite(ymin), is.finite(ymax)),
      aes(
        x = GDP_pc_EUR, xend = GDP_pc_EUR,
        y = ymin, yend = ymax,
        colour = quadrant
      ),
      linewidth = bar_linewidth,
      alpha = bar_alpha_non_eu,
      lineend = "round",
      show.legend = FALSE
    ) +

    geom_segment(
      data = dplyr::filter(df_bar_eu, is.finite(ymin), is.finite(ymax)),
      aes(
        x = GDP_pc_EUR, xend = GDP_pc_EUR,
        y = ymin, yend = ymax
      ),
      linewidth = bar_linewidth,
      alpha = bar_alpha_eu,
      lineend = "round",
      colour = eu_red,
      show.legend = FALSE
    ) +

    geom_point(
      data = df_bar_non_eu,
      aes(fill = quadrant),
      shape = 21,
      size = point_size_non_eu,
      colour = "black",
      stroke = 0.25
    ) +

    geom_point(
      data = df_bar_eu,
      shape = 21,
      size = point_size_eu,
      fill = eu_red,
      colour = "black",
      stroke = 0.30,
      show.legend = FALSE
    ) +

    ggrepel::geom_text_repel(
      data = df_bar,
      aes(label = country),
      colour = "black",
      size = label_size,
      seed = 123,
      nudge_y = label_nudge_y,
      direction = "both",
      box.padding = 0.30,
      point.padding = 0.10,
      force = 1.0,
      force_pull = 0.1,
      max.iter = 20000,
      min.segment.length = 0,
      segment.color = "grey45",
      segment.alpha = 0.45,
      segment.size = 0.25,
      show.legend = FALSE,
      max.overlaps = Inf
    ) +

    scale_fill_manual(
      values = quad_pal,
      breaks = legend_quads,
      name = NULL,
      drop = FALSE,
      guide = guide_legend(
        override.aes = list(
          shape = 21,
          size = legend_point_size,
          colour = "black",
          stroke = 0.25,
          alpha = 1
        )
      )
    ) +
    scale_colour_manual(
      values = quad_pal,
      breaks = legend_quads,
      guide = "none",
      drop = FALSE
    ) +

    scale_x_continuous(
      limits = c(0, NA),
      breaks = x_breaks,
      labels = scales::label_number(scale = 1 / 1000, accuracy = 1),
      expand = expansion(mult = c(0.00, 0.02))
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = y_breaks,
      expand = expansion(mult = c(0.00, 0.10))
    ) +

    labs(
      x = "GDP per capita (kEUR/person)",
      y = paste0(phase_target, " emissions per capita (g/cap)")
    ) +

    coord_cartesian(clip = "off") +

    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.35),
      legend.position = "",
      legend.key = element_blank(),
      legend.text = element_text(size = 12),
      plot.margin = margin(5.5, 5.5, 5.5, 5.5),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      text = element_text(family = "Calibri")
    )
}

p_map <- make_gdp_phase_plot("MaP")
p_mp  <- make_gdp_phase_plot("MP")

ggsave("Results/Graphs/EM_GDP_MaP.png", plot = p_map, width = 12, height = 6)
ggsave("Results/Graphs/EM_GDP_MP.png",  plot = p_mp,  width = 12, height = 6)

p_combined <- p_map + p_mp + patchwork::plot_layout(ncol = 2)
ggsave("Results/Graphs/EM_GDP_MaP_MP_combined.png", plot = p_combined, width = 14, height = 6)
