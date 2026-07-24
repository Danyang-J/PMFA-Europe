library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ggrepel)

# ------------------------------------------------------------------------------
# Input / output
# ------------------------------------------------------------------------------
in_dir <- "Results/Tables/EM"
out_png <- "Results/Graphs/CountryHeterogeneity_ByProduct_MaP_MP.png"

# ------------------------------------------------------------------------------
# Product grouping (kept consistent with 09.2_HeatMap_Product.R)
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

prod_to_sector <- imap_dfr(prod_groups, ~ tibble(prod = .x, sector = .y))

# ------------------------------------------------------------------------------
# Visual settings
# ------------------------------------------------------------------------------
phase_labels <- c(
  "MaP" = "Macroplastic emissions per capita (g/cap)",
  "MP" = "Microplastic emissions per capita (g/cap)"
)

note_text <- "Boxplots: 30 European countries\nRed diamonds: EU mean ± SD"

phase_fill <- c(
  "MaP" = "#E3D6F2",
  "MP" = "#D7EEF8"
)

phase_line <- c(
  "MaP" = "#8D63C9",
  "MP" = "#0094C8"
)

eu_col <- "#FB8072"

# Final figure label overrides for sectors that need manual editorial control.
label_suppress <- tibble(
  phase = c("MaP", "MaP"),
  sector = c("Clothing", "Household textiles")
)

label_force_outliers <- tibble(
  phase = c("MaP", "MaP", "MP"),
  sector = c("Consumer packaging", "Non-consumer packaging", "Pre-consumer processes"),
  country = c("RO", "EL", "LT"),
  force_label = TRUE
)

# ------------------------------------------------------------------------------
# Read country-level product data
# ------------------------------------------------------------------------------
systems <- c("AT", "BE", "BG", "HR", "CY",
             "CZ", "DK", "EE", "FI", "FR",
             "DE", "EL", "HU", "IE", "IT",
             "LV", "LT", "LU", "MT", "NL",
             "NO", "PL", "PT", "RO", "SK",
             "SI", "ES", "SE", "CH", "UK")

read_country_file <- function(country_code) {
  read.csv(file.path(in_dir, paste0("EM_prodpol_", country_code, ".csv")), check.names = FALSE) %>%
    transmute(
      country = country_code,
      prod = .[[1]],
      dest = .[[3]],
      em_percap_mean = .[[8]],
      em_percap_sd = .[[9]]
    )
}

df_raw_country <- imap_dfr(setNames(vector("list", length(systems)), systems),
                           ~ read_country_file(.y))

country_group <- df_raw_country %>%
  group_by(country) %>%
  summarise(
    total_rule = sum(em_percap_mean, na.rm = TRUE),
    mp_amount = sum(em_percap_mean[grepl("\\(MP\\)$", dest)], na.rm = TRUE),
    coastal_amount = sum(em_percap_mean[grepl("^Coastal and ocean water", dest)], na.rm = TRUE),
    mp_fraction_rule = ifelse(total_rule > 0, 100 * mp_amount / total_rule, 0),
    coastal_total_share = ifelse(total_rule > 0, coastal_amount / total_rule, 0),
    .groups = "drop"
  ) %>%
  mutate(
    group = case_when(
      coastal_total_share >= 0.10 ~ "Marine-linked emissions",
      mp_fraction_rule >= 30 ~ "High microplastic-share emissions",
      total_rule >= 400 ~ "Soil-dominant high emissions",
      TRUE ~ "Mixed emissions profile"
    )
  )

df_country <- df_raw_country %>%
  mutate(
    phase = ifelse(grepl(" \\(MP\\)$", dest), "MP", "MaP")
  ) %>%
  inner_join(prod_to_sector, by = "prod") %>%
  group_by(country, phase, sector) %>%
  summarise(
    em_percap_mean = sum(em_percap_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(country_group %>% select(country, group), by = "country")

# ------------------------------------------------------------------------------
# Read EU benchmark from the EU file only
# ------------------------------------------------------------------------------
df_eu <- read_country_file("EU") %>%
  mutate(
    phase = ifelse(grepl(" \\(MP\\)$", dest), "MP", "MaP")
  ) %>%
  inner_join(prod_to_sector, by = "prod") %>%
  group_by(phase, sector) %>%
  summarise(
    eu_percap = sum(em_percap_mean, na.rm = TRUE),
    eu_sd = sqrt(sum(em_percap_sd^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Phase-specific ordering
# ------------------------------------------------------------------------------
phase_order <- df_country %>%
  group_by(phase, sector) %>%
  summarise(
    sector_max = max(em_percap_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(sector_max > 0) %>%
  left_join(
    df_eu %>% select(phase, sector, eu_percap),
    by = c("phase", "sector")
  ) %>%
  filter(!is.na(eu_percap)) %>%
  group_by(phase) %>%
  arrange(desc(eu_percap), .by_group = TRUE) %>%
  mutate(sector_rank = row_number()) %>%
  ungroup() %>%
  select(phase, sector, sector_rank)

df_country <- df_country %>%
  left_join(phase_order, by = c("phase", "sector")) %>%
  filter(!is.na(sector_rank)) %>%
  mutate(
    sector_plot = paste(phase, sector, sep = "__")
  )

df_eu <- df_eu %>%
  left_join(phase_order, by = c("phase", "sector")) %>%
  filter(!is.na(sector_rank)) %>%
  mutate(
    sector_plot = paste(phase, sector, sep = "__")
  )

df_country <- df_country %>%
  group_by(phase) %>%
  mutate(phase_max = max(em_percap_mean, na.rm = TRUE)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------
plot_phase <- function(phase_target,
                       sector_keep = NULL,
                       x_limits = NULL,
                       title_override = NULL,
                       show_note = TRUE,
                       plot_margins = margin(t = 6, r = 20, b = 6, l = 20),
                       y_spacing = 1) {
  df_country_phase <- df_country %>%
    filter(phase == phase_target) %>%
    {
      if (is.null(sector_keep)) {
        .
      } else {
        filter(., sector %in% sector_keep)
      }
    } %>%
    arrange(desc(sector_rank)) %>%
    mutate(
      sector = factor(sector, levels = unique(sector)),
      sector_num = y_spacing * seq_along(levels(sector))[match(sector, levels(sector))]
    )

  df_eu_phase <- df_eu %>%
    filter(phase == phase_target) %>%
    {
      if (is.null(sector_keep)) {
        .
      } else {
        filter(., sector %in% sector_keep)
      }
    } %>%
    mutate(
      sector = factor(sector, levels = levels(df_country_phase$sector)),
      sector_num = y_spacing * seq_along(levels(df_country_phase$sector))[match(sector, levels(df_country_phase$sector))]
    ) %>%
    filter(!is.na(eu_percap), !is.na(sector))

  df_outlier_phase <- df_country_phase %>%
    group_by(sector, sector_num) %>%
    mutate(
      q1 = quantile(em_percap_mean, 0.25, na.rm = TRUE, names = FALSE),
      q3 = quantile(em_percap_mean, 0.75, na.rm = TRUE, names = FALSE),
      iqr = q3 - q1,
      lower_fence = q1 - 1.5 * iqr,
      upper_fence = q3 + 1.5 * iqr,
      is_outlier = em_percap_mean < lower_fence | em_percap_mean > upper_fence,
      outlier_gap = pmax(em_percap_mean - upper_fence, lower_fence - em_percap_mean, 0),
      label_gap_threshold = pmax(0.35 * upper_fence, 0.5)
    ) %>%
    ungroup() %>%
    left_join(label_force_outliers, by = c("phase", "sector", "country")) %>%
    mutate(force_label = ifelse(is.na(force_label), FALSE, force_label)) %>%
    anti_join(label_suppress, by = c("phase", "sector")) %>%
    filter(
      is_outlier & (outlier_gap >= label_gap_threshold | force_label)
    ) %>%
    select(-force_label)

  x_max_phase <- max(df_country_phase$em_percap_mean, df_eu_phase$eu_percap + df_eu_phase$eu_sd, na.rm = TRUE)
  sector_breaks <- y_spacing * seq_along(levels(df_country_phase$sector))
  label_nudge_y <- 0.24 * y_spacing

  p <- ggplot(df_country_phase, aes(x = em_percap_mean, y = sector_num)) +
    geom_boxplot(
      aes(group = sector_num),
      orientation = "y",
      width = 0.42,
      outlier.shape = 16,
      outlier.size = 2.8,
      outlier.alpha = 0.85,
      fill = phase_fill[[phase_target]],
      color = phase_line[[phase_target]],
      median.linewidth = 0.8,
      linewidth = 0.8,
      alpha = 0.55
    ) +
    geom_segment(
      data = df_eu_phase,
      aes(
        x = pmax(0, eu_percap - eu_sd),
        xend = eu_percap + eu_sd,
        y = sector_num,
        yend = sector_num
      ),
      inherit.aes = FALSE,
      color = eu_col,
      linewidth = 0.95
    ) +
    geom_point(
      data = df_eu_phase,
      aes(x = eu_percap, y = sector_num),
      inherit.aes = FALSE,
      shape = 18,
      size = 3.8,
      color = eu_col
    ) +
    ggrepel::geom_text_repel(
      data = df_outlier_phase,
      aes(label = country),
      inherit.aes = TRUE,
      size = 3.4,
      fontface = "bold",
      seed = 123,
      nudge_y = label_nudge_y,
      direction = "y",
      hjust = 0.5,
      box.padding = 0.14,
      point.padding = 0.12,
      force = 1.0,
      force_pull = 1.4,
      max.iter = 20000,
      min.segment.length = 0,
      segment.color = "grey45",
      segment.alpha = 0.5,
      segment.size = 0.25,
      show.legend = FALSE,
      max.overlaps = Inf
    ) +
    scale_x_continuous(
      limits = if (is.null(x_limits)) c(0, NA) else x_limits,
      breaks = scales::breaks_pretty(n = 7),
      expand = if (is.null(x_limits)) expansion(mult = c(0, 0.08)) else expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      breaks = sector_breaks,
      labels = levels(df_country_phase$sector),
      limits = c(0.5 * y_spacing, (length(levels(df_country_phase$sector)) + 0.5) * y_spacing),
      expand = c(0, 0)
    ) +
    labs(
      title = if (is.null(title_override)) phase_labels[[phase_target]] else title_override,
      x = NULL,
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.35),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.background = element_rect(fill = NA, colour = NA),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5, color = "black"),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      text = element_text(color = "grey20"),
      plot.margin = plot_margins
    )

  if (show_note) {
    p <- p +
      annotate(
        "label",
        x = Inf,
        y = 0.72,
        label = note_text,
        hjust = 1.08,
        vjust = 0,
        size = 3.2,
        color = "grey25",
        fill = scales::alpha("white", 0.72),
        border.color = NA,
        label.padding = grid::unit(0.14, "lines"),
        lineheight = 1.0
      )
  }

  p
}

map_top_sectors <- df_eu %>%
  filter(phase == "MaP") %>%
  arrange(sector_rank) %>%
  slice_head(n = 2) %>%
  pull(sector)

map_bottom_sectors <- df_eu %>%
  filter(phase == "MaP", !sector %in% map_top_sectors) %>%
  arrange(sector_rank) %>%
  pull(sector)

p_map_top <- plot_phase(
  "MaP",
  sector_keep = map_top_sectors,
  title_override = phase_labels[["MaP"]],
  show_note = FALSE,
  plot_margins = margin(t = 4, r = 20, b = 2, l = 20)
) +
  theme(plot.title = element_blank())

p_map_bottom <- plot_phase(
  "MaP",
  sector_keep = map_bottom_sectors,
  x_limits = c(0, 140),
  title_override = "",
  show_note = TRUE,
  plot_margins = margin(t = 2, r = 20, b = 1, l = 20)
)

p_mp <- plot_phase(
  "MP",
  title_override = phase_labels[["MP"]],
  show_note = TRUE,
  plot_margins = margin(t = 4, r = 20, b = 1, l = 20)
) +
  theme(plot.title = element_blank())

resolve_unit_cm <- function(units_obj, target_cm, converter) {
  if (length(units_obj) == 0) {
    return(numeric(0))
  }

  unit_types <- grid::unitType(units_obj)
  unit_vals <- as.numeric(units_obj)
  abs_idx <- unit_types != "null"
  abs_total_cm <- if (any(abs_idx)) {
    sum(converter(units_obj[abs_idx], "cm", valueOnly = TRUE))
  } else {
    0
  }
  null_total <- sum(unit_vals[unit_types == "null"])
  null_cm <- if (null_total > 0) (target_cm - abs_total_cm) / null_total else 0

  vapply(seq_along(units_obj), function(i) {
    if (unit_types[i] == "null") {
      unit_vals[i] * null_cm
    } else {
      converter(units_obj[i], "cm", valueOnly = TRUE)
    }
  }, numeric(1))
}

get_panel_bounds_npc <- function(plot_obj, target_width_cm, target_height_cm) {
  g <- if (inherits(plot_obj, "gtable")) plot_obj else ggplotGrob(plot_obj)
  panel_row <- g$layout[g$layout$name == "panel", ][1, ]

  width_cm <- resolve_unit_cm(g$widths, target_width_cm, grid::convertWidth)
  height_cm <- resolve_unit_cm(g$heights, target_height_cm, grid::convertHeight)

  total_width_cm <- sum(width_cm)
  total_height_cm <- sum(height_cm)

  left_cm <- sum(width_cm[seq_len(panel_row$l - 1)])
  panel_width_cm <- sum(width_cm[panel_row$l:panel_row$r])
  top_before_cm <- sum(height_cm[seq_len(panel_row$t - 1)])
  panel_height_cm <- sum(height_cm[panel_row$t:panel_row$b])

  list(
    left = left_cm / total_width_cm,
    right = (left_cm + panel_width_cm) / total_width_cm,
    top = 1 - top_before_cm / total_height_cm,
    bottom = 1 - (top_before_cm + panel_height_cm) / total_height_cm
  )
}

map_top_xmax <- max(
  df_country %>%
    filter(phase == "MaP", sector %in% map_top_sectors) %>%
    pull(em_percap_mean),
  df_eu %>%
    filter(phase == "MaP", sector %in% map_top_sectors) %>%
    transmute(val = eu_percap + eu_sd) %>%
    pull(val),
  na.rm = TRUE
)

map_top_display_range <- c(0, map_top_xmax * 1.08)
map_bottom_display_range <- c(0, 140)

aligned_map <- cowplot::align_plots(p_map_top, p_map_bottom, align = "v", axis = "lr")

total_plot_width_cm <- 17.2 * 2.54
total_plot_height_cm <- 7.2 * 2.54
left_plot_width_cm <- total_plot_width_cm * 1.02 / (1.02 + 1)
right_plot_width_cm <- total_plot_width_cm * 1 / (1.02 + 1)

map_counts <- c(length(map_top_sectors), length(map_bottom_sectors))
map_gap <- 0.025
map_panel_height <- 1 - map_gap
map_top_height <- map_panel_height * map_counts[1] / sum(map_counts)
map_bottom_height <- map_panel_height * map_counts[2] / sum(map_counts)
map_top_bottom <- map_bottom_height + map_gap
map_bottom_top <- map_bottom_height

top_bounds <- get_panel_bounds_npc(aligned_map[[1]], left_plot_width_cm, total_plot_height_cm * map_top_height)
bottom_bounds <- get_panel_bounds_npc(aligned_map[[2]], left_plot_width_cm, total_plot_height_cm * map_bottom_height)
mp_bounds <- get_panel_bounds_npc(p_mp, right_plot_width_cm, total_plot_height_cm)

bottom_axis_adjust <- max((bottom_bounds$bottom - mp_bounds$bottom) * map_bottom_height, 0)
bottom_row_height <- ((map_bottom_height + bottom_axis_adjust) *
  (bottom_bounds$top - bottom_bounds$bottom)) / length(map_bottom_sectors)

map_top_y_spacing <- (map_top_height * (top_bounds$top - top_bounds$bottom)) /
  (length(map_top_sectors) * bottom_row_height)
mp_sector_count <- df_eu %>%
  filter(phase == "MP") %>%
  nrow()
mp_y_spacing <- (0.965 * (mp_bounds$top - mp_bounds$bottom)) /
  (mp_sector_count * bottom_row_height)

p_map_top <- plot_phase(
  "MaP",
  sector_keep = map_top_sectors,
  title_override = phase_labels[["MaP"]],
  show_note = FALSE,
  plot_margins = margin(t = 4, r = 20, b = 2, l = 20),
  y_spacing = map_top_y_spacing
) +
  theme(plot.title = element_blank())

p_map_bottom <- plot_phase(
  "MaP",
  sector_keep = map_bottom_sectors,
  x_limits = c(0, 140),
  title_override = "",
  show_note = TRUE,
  plot_margins = margin(t = 2, r = 20, b = 1, l = 20),
  y_spacing = 1
)

p_mp <- plot_phase(
  "MP",
  title_override = phase_labels[["MP"]],
  show_note = TRUE,
  plot_margins = margin(t = 4, r = 20, b = 1, l = 20),
  y_spacing = mp_y_spacing
) +
  theme(plot.title = element_blank())

aligned_map <- cowplot::align_plots(p_map_top, p_map_bottom, align = "v", axis = "lr")
top_bounds <- get_panel_bounds_npc(aligned_map[[1]], left_plot_width_cm, total_plot_height_cm * map_top_height)
bottom_bounds <- get_panel_bounds_npc(aligned_map[[2]], left_plot_width_cm, total_plot_height_cm * map_bottom_height)
mp_bounds <- get_panel_bounds_npc(p_mp, right_plot_width_cm, total_plot_height_cm)
bottom_axis_adjust <- max((bottom_bounds$bottom - mp_bounds$bottom) * map_bottom_height, 0)

top_x0 <- top_bounds$left
top_x140 <- top_bounds$left + (140 / diff(map_top_display_range)) * (top_bounds$right - top_bounds$left)
bottom_x0 <- bottom_bounds$left
bottom_x140 <- bottom_bounds$right

trap_x <- c(
  top_x0,
  top_x140,
  bottom_x140,
  bottom_x0
)

trap_y <- c(
  map_top_bottom + top_bounds$bottom * map_top_height,
  map_top_bottom + top_bounds$bottom * map_top_height,
  -bottom_axis_adjust + bottom_bounds$top * (map_bottom_height + bottom_axis_adjust),
  -bottom_axis_adjust + bottom_bounds$top * (map_bottom_height + bottom_axis_adjust)
)

trap_side_lines <- grid::segmentsGrob(
  x0 = grid::unit(trap_x[c(1, 2)], "npc"),
  y0 = grid::unit(trap_y[c(1, 2)], "npc"),
  x1 = grid::unit(trap_x[c(4, 3)], "npc"),
  y1 = grid::unit(trap_y[c(4, 3)], "npc"),
  gp = grid::gpar(
    col = phase_line[["MaP"]],
    lwd = 2.0,
    lty = "22",
    lineend = "round",
    linejoin = "round"
  )
)

make_solid_arrow_head <- function(x_start, y_start, x_end, y_end,
                                  side_cm = 0.48) {
  start_cm <- c(x_start * left_plot_width_cm, y_start * total_plot_height_cm)
  end_cm <- c(x_end * left_plot_width_cm, y_end * total_plot_height_cm)
  direction <- end_cm - start_cm
  direction <- direction / sqrt(sum(direction^2))
  normal <- c(-direction[2], direction[1])
  triangle_height <- sqrt(3) / 2 * side_cm
  base_center <- end_cm - direction * triangle_height
  base_left <- base_center + normal * side_cm / 2
  base_right <- base_center - normal * side_cm / 2

  xs_cm <- c(end_cm[1], base_left[1], base_right[1])
  ys_cm <- c(end_cm[2], base_left[2], base_right[2])

  grid::polygonGrob(
    x = grid::unit(xs_cm / left_plot_width_cm, "npc"),
    y = grid::unit(ys_cm / total_plot_height_cm, "npc"),
    gp = grid::gpar(fill = phase_line[["MaP"]], col = NA)
  )
}

trap_arrow_heads <- grid::grobTree(
  make_solid_arrow_head(trap_x[1], trap_y[1], trap_x[4], trap_y[4]),
  make_solid_arrow_head(trap_x[2], trap_y[2], trap_x[3], trap_y[3])
)

left_map_core <- cowplot::ggdraw() +
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA))) +
  cowplot::draw_grob(
    grid::polygonGrob(
      x = grid::unit(trap_x, "npc"),
      y = grid::unit(trap_y, "npc"),
      gp = grid::gpar(fill = scales::alpha(phase_fill[["MaP"]], 0.38), col = NA)
    )
  ) +
  cowplot::draw_plot(aligned_map[[1]], x = 0, y = map_top_bottom, width = 1, height = map_top_height) +
  cowplot::draw_plot(
    aligned_map[[2]],
    x = 0,
    y = -bottom_axis_adjust,
    width = 1,
    height = map_bottom_height + bottom_axis_adjust
  ) +
  cowplot::draw_grob(trap_side_lines) +
  cowplot::draw_grob(trap_arrow_heads)

left_map <- cowplot::ggdraw() +
  cowplot::draw_label(
    phase_labels[["MaP"]],
    x = 0.5,
    y = 0.995,
    hjust = 0.5,
    vjust = 1,
    fontface = "bold",
    size = 15,
    color = "black"
  ) +
  cowplot::draw_plot(left_map_core, x = 0, y = 0, width = 1, height = 0.965)

right_mp <- cowplot::ggdraw() +
  cowplot::draw_label(
    phase_labels[["MP"]],
    x = 0.5,
    y = 0.995,
    hjust = 0.5,
    vjust = 1,
    fontface = "bold",
    size = 15,
    color = "black"
  ) +
  cowplot::draw_plot(p_mp, x = 0, y = 0, width = 1, height = 0.965)

p_core <- cowplot::plot_grid(
  left_map,
  right_mp,
  ncol = 2,
  rel_widths = c(1.02, 1),
  align = "v",
  axis = "tb"
)

p <- cowplot::ggdraw() +
  cowplot::draw_plot(p_core, x = 0, y = 0.02, width = 1, height = 0.96)

ggsave(
  filename = out_png,
  plot = p,
  width = 17.2,
  height = 7.2,
  dpi = 450,
  bg = "white"
)

message("Saved: ", out_png)
