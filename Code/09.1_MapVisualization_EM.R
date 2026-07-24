library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)
library(scales)
library(ggpattern)

# Avoid spherical clipping artifacts in this workflow
sf_use_s2(FALSE)

#### European country codes ####################################################

europe_countries <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
  "DE","EL","HU","IE","IT","LV","LT","LU","MT","NL",
  "NO","PL","PT","RO","SK","SI","ES","SE","CH","UK"
)

eu_to_ne_iso2 <- c("EL" = "GR", "UK" = "GB")

europe_countries_ne <- sapply(europe_countries, function(x) {
  if (x %in% names(eu_to_ne_iso2)) eu_to_ne_iso2[[x]] else x
})

#### Load world map ############################################################

world_large <- st_make_valid(ne_countries(scale = "large", returnclass = "sf")) %>%
  mutate(iso_a2 = case_when(
    name == "France" ~ "FR",
    name == "Norway" ~ "NO",
    TRUE ~ iso_a2
  ))

model_iso <- unique(europe_countries_ne)

# Narrower map window, still covering all 30 countries
xlim_map <- c(-12.8, 39.2)
ylim_map <- c(33, 73.5)

sea_fill <- "white"

bbox_sfc <- st_as_sfc(
  st_bbox(c(
    xmin = xlim_map[1], xmax = xlim_map[2],
    ymin = ylim_map[1], ymax = ylim_map[2]
  ), crs = 4326)
)

map_region <- world_large %>%
  filter(st_intersects(geometry, bbox_sfc, sparse = FALSE)[, 1]) %>%
  st_crop(st_bbox(bbox_sfc)) %>%
  mutate(is_model_country = iso_a2 %in% model_iso)

#### Plot function #############################################################

plot_map <- function(map_sf, value_col, title, color = NULL, n_breaks = 5,
                     legend_title = "g/cap") {
  
  data_vals <- map_sf[[value_col]]
  data_vals[!map_sf$is_model_country] <- NA
  
  # Treat zeros as NA in the plot
  plot_vals <- data_vals
  plot_vals[plot_vals == 0] <- NA
  map_sf[[value_col]] <- plot_vals
  
  val_max <- max(plot_vals, na.rm = TRUE)
  if (!is.finite(val_max)) val_max <- 0
  val_range <- c(0, val_max)
  
  breaks_seq <- seq(val_range[1], val_range[2], length.out = n_breaks)
  
  make_value_labels <- function(x) {
    if (length(x) == 0) return(character(0))
    lab <- formatC(signif(x, 3), format = "fg", digits = 3, flag = "#")
    sub("\\.$", "", lab)
  }
  
  labels_check <- make_value_labels(breaks_seq)
  while (length(unique(labels_check)) < length(labels_check) && length(breaks_seq) > 2) {
    n_breaks <- n_breaks - 1
    breaks_seq <- seq(val_range[1], val_range[2], length.out = n_breaks)
    labels_check <- make_value_labels(breaks_seq)
  }
  
  use_custom <- is.character(color) && length(color) > 1
  
  cb_guide <- guide_colorbar(
    title = legend_title,
    title.position = "top",
    title.hjust = 0.5,
    title.theme = element_text(size = 11, hjust = 0.5, margin = margin(b = 16)),
    label.position = "right",
    label.theme = element_text(hjust = 0, margin = margin(l = 4, b = 6))
  )
  
  map_other <- map_sf %>% filter(!is_model_country)
  map_model <- map_sf %>% filter(is_model_country)
  
  map_other_mask <- suppressWarnings(
    st_crop(
      st_make_valid(map_other),
      xmin = xlim_map[1], xmax = xlim_map[2],
      ymin = ylim_map[1], ymax = ylim_map[2]
    )
  ) %>% st_collection_extract("POLYGON", warn = FALSE)
  
  p <- ggplot() +
    geom_sf(data = map_other_mask, fill = "white", color = NA) +
    ggpattern::geom_sf_pattern(
      data = map_other_mask,
      fill = NA,
      color = NA,
      pattern = "stripe",
      pattern_angle = 45,
      pattern_density = 0.15,
      pattern_spacing = 0.03,
      pattern_size = 0.4,
      pattern_fill = "grey85",
      pattern_colour = "grey85"
    ) +
    geom_sf(data = map_model, aes_string(fill = value_col), color = NA) +
    geom_sf(data = map_sf, fill = NA, color = "grey35", size = 0.20) +
    coord_sf(xlim = xlim_map, ylim = ylim_map, clip = "on", expand = FALSE) +
    labs(title = title, fill = NULL) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      panel.background = element_rect(fill = sea_fill, color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      
      legend.position = c(0.99, 0.50),
      legend.justification = c(1, 0.5),
      legend.direction = "vertical",
      legend.background = element_rect(fill = scales::alpha("white", 0.82), color = NA),
      legend.key = element_blank(),
      legend.title = element_text(size = 11, hjust = 0.5, margin = margin(b = 16)),
      legend.text = element_text(size = 11, hjust = 0),
      legend.margin = margin(4, 4, 8, 4),
      legend.key.height = unit(0.62, "cm"),
      legend.key.width = unit(0.38, "cm")
    )
  
  if (use_custom) {
    p <- p + scale_fill_gradientn(
      colours = color,
      oob = scales::squish,
      labels = make_value_labels,
      breaks = breaks_seq,
      na.value = "white",
      limits = val_range,
      guide = cb_guide
    )
  } else {
    p <- p + scale_fill_distiller(
      palette = color,
      direction = 1,
      oob = scales::squish,
      labels = make_value_labels,
      breaks = breaks_seq,
      na.value = "white",
      limits = val_range,
      guide = cb_guide
    )
  }
  
  p
}

#### Clean names for filenames #################################################

clean_name <- function(x) {
  x <- gsub("\\s*\\(MP\\)|\\(MaP\\)", "", x)
  x <- gsub("\\s+", "_", x)
  x
}

#### Load data #################################################################

EM_data <- read.csv("Results/Tables/EM/EM_agg_lifecycle.csv")
EM_percap_data <- read.csv("Results/Tables/EM/EM_agg_percap_lifecycle.csv")
EM_EOL_percap_data <- read.csv("Results/Tables/EM/EM_agg_EOL_percap.csv")

#### Plot EM per capita, microplastics ########################################

env_MP <- c("Soil (MP)", "Subsurface soil (MP)", "Fresh water (MP)", "Coastal and ocean water (MP)")

EM_percap_data_MP_agg <- EM_percap_data %>%
  group_by(country, dest) %>%
  summarise(
    mean = sum(mean, na.rm = TRUE),
    .groups = "drop"
  )

plot_list_EM_percap_MP <- list()

for (env in env_MP) {
  df_env <- EM_percap_data_MP_agg %>%
    filter(dest == env) %>%
    mutate(iso_a2 = ifelse(country %in% names(eu_to_ne_iso2), eu_to_ne_iso2[country], country))
  
  map_sf <- map_region %>% left_join(df_env, by = "iso_a2")
  
  p <- plot_map(
    map_sf, "mean", title = env,
    color = c("#F5F5F5", "#CAE7FA", "#9ED8F0", "#5BB7E3", "#0094C8", "#00558C", "#00388C")
  )
  
  clean_env <- clean_name(env)
  ggsave(
    filename = paste0("Results/Graphs/Maps/Detailed/EM_percap_MP_", clean_env, ".png"),
    plot = p, width = 6.5, height = 7.0
  )
  
  plot_list_EM_percap_MP[[env]] <- p
}

#### Plot total EM per capita (MP + MaP) ######################################

EM_percap_total <- EM_percap_data %>%
  group_by(country) %>%
  summarise(
    mean = sum(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(iso_a2 = ifelse(country %in% names(eu_to_ne_iso2), eu_to_ne_iso2[country], country))

p_em_total <- plot_map(
  map_region %>% left_join(EM_percap_total, by = "iso_a2"),
  "mean",
  title = "Total emissions (MP + MaP)",
  color = c("#F1EDF7", "#D8D4EA", "#B6B5D8", "#8E90C3", "#666CAE", "#414A91", "#24275F")
)

ggsave(
  filename = "Results/Graphs/Maps/Detailed/EM_percap_Total_MP_MaP.png",
  plot = p_em_total, width = 6.5, height = 7.0
)

#### Plot EM per capita, macroplastics ########################################

env_MaP <- c("Soil", "Fresh water", "Coastal and ocean water")
plot_list_EM_percap_MaP <- list()

for (env in env_MaP) {
  df_env <- EM_percap_data %>%
    filter(dest == env) %>%
    mutate(iso_a2 = ifelse(country %in% names(eu_to_ne_iso2), eu_to_ne_iso2[country], country))
  
  map_sf <- map_region %>% left_join(df_env, by = "iso_a2")
  
  p <- plot_map(
    map_sf, "mean", title = paste0(env, " (MaP)"),
    color = c("#F5F5F5", "#DFC9EE", "#C4A3E6", "#9A6FD6", "#6F3CC3", "#4B1688", "#2E0B57")
  )
  
  clean_env <- clean_name(env)
  ggsave(
    filename = paste0("Results/Graphs/Maps/Detailed/EM_percap_MaP_", clean_env, ".png"),
    plot = p, width = 6.5, height = 7.0
  )
  
  plot_list_EM_percap_MaP[[env]] <- p
}

#### Plot EM EOL per capita ####################################################

env_EOL <- c("Incineration", "Landfill")
plot_list_EM_EOL_percap <- list()

for (env in env_EOL) {
  df_env <- EM_EOL_percap_data %>%
    filter(dest == env) %>%
    mutate(
      iso_a2 = ifelse(country %in% names(eu_to_ne_iso2), eu_to_ne_iso2[country], country),
      mean = mean / 1000
    )
  
  map_sf <- map_region %>% left_join(df_env, by = "iso_a2")
  
  p <- plot_map(
    map_sf, "mean",
    title = env,
    #color = c("#F5F5F5", "#D0EDC9", "#BFE6B8", "#8FD18A", "#4FBF6A", "#1F7A3A", "#003D1F"),
    c("#F5F5F5", "#D9D9D9", "#BFBFBF", "#A6A6A6", "#8C8C8C", "#595959", "#1A1A1A"),
    legend_title = "kg/cap"
  )
  
  clean_env <- clean_name(env)
  ggsave(
    filename = paste0("Results/Graphs/Maps/Detailed/EM_EOL_percap_", clean_env, ".png"),
    plot = p, width = 6.5, height = 7.0
  )
  
  plot_list_EM_EOL_percap[[env]] <- p
}

#### Plot EM EOL Extra per capita ##############################################

env_EOL_ex <- c("Sludge amended soil", "Sludge amended soil (MP)")

for (env in env_EOL_ex) {
  phase <- ifelse(grepl(" \\(MP\\)$", env), "MP", "MaP")
  col_vec <- if (phase == "MP") {
    c("#F5F5F5","#CAE7FA","#9ED8F0", "#5BB7E3", "#0094C8", "#00558C", "#00388C")
  } else {
    c("#F5F5F5","#DFC9EE", "#C4A3E6", "#9A6FD6", "#6F3CC3", "#4B1688", "#2E0B57")
  }
  
  df_env <- EM_EOL_percap_data %>%
    filter(dest == env) %>%
    mutate(iso_a2 = ifelse(country %in% names(eu_to_ne_iso2), eu_to_ne_iso2[country], country))
  
  map_sf <- map_region %>% left_join(df_env, by = "iso_a2")
  base_env <- sub(" \\(MP\\)$", "", env)
  
  p <- plot_map(
    map_sf, "mean",
    title = paste0(base_env, " (", phase, ")"),
    color = col_vec,
    legend_title = "g/cap"
  )
  
  clean_base <- clean_name(base_env)
  
  ggsave(
    filename = paste0("Results/Graphs/Maps/Detailed/EM_EOL_percap_", clean_base, "_", phase, ".png"),
    plot = p, width = 6.5, height = 7.0
  )
  
  plot_list_EM_EOL_percap[[env]] <- p
}

#### Patchwork #################################################################

p_map_fresh <- plot_list_EM_percap_MaP[["Fresh water"]]
p_map_coast <- plot_list_EM_percap_MaP[["Coastal and ocean water"]]
p_map_soil  <- plot_list_EM_percap_MaP[["Soil"]]

p_mp_fresh   <- plot_list_EM_percap_MP[["Fresh water (MP)"]]
p_mp_coast   <- plot_list_EM_percap_MP[["Coastal and ocean water (MP)"]]
p_mp_soil    <- plot_list_EM_percap_MP[["Soil (MP)"]]
p_mp_subsoil <- plot_list_EM_percap_MP[["Subsurface soil (MP)"]]

p_eol_inc <- plot_list_EM_EOL_percap[["Incineration"]]
p_eol_lan <- plot_list_EM_EOL_percap[["Landfill"]]
p_eol_sls <- plot_list_EM_EOL_percap[["Sludge amended soil"]]
p_eol_slm <- plot_list_EM_EOL_percap[["Sludge amended soil (MP)"]]

# Effective spacing for patchwork: use per-plot margins (pt)
add_gap <- function(p) {
  p + theme(
    plot.margin = margin(12, 12, 12, 12)  # top, right, bottom, left
  )
}

# MaP
p_MaP_grid <-
  add_gap(p_map_fresh) + add_gap(p_map_coast) +
  add_gap(p_map_soil)  + add_gap(p_eol_sls) +
  plot_layout(ncol = 2, guides = "keep", widths = c(1, 1), heights = c(1, 1))

p_MaP_combined <- p_MaP_grid

ggsave(
  filename = "Results/Graphs/Maps/EM_percap_patchwork_MaP.png",
  plot = p_MaP_combined, width = 10, height = 13.3, dpi = 300
)

# MP
p_MP_grid <-
  add_gap(p_mp_fresh) + add_gap(p_mp_coast) +
  add_gap(p_mp_soil)  + add_gap(p_mp_subsoil) +
  add_gap(p_eol_slm)  + plot_spacer() +
  plot_layout(ncol = 2, guides = "keep", widths = c(1, 1), heights = c(1, 1, 1))

p_MP_combined <- p_MP_grid

ggsave(
  filename = "Results/Graphs/Maps/EM_percap_patchwork_MP.png",
  plot = p_MP_combined, width = 10, height = 20.2, dpi = 300
)

# EOL
p_EOL_grid <-
  add_gap(p_eol_inc) + add_gap(p_eol_lan) +
  plot_layout(ncol = 2, guides = "keep", widths = c(1, 1))

p_EOL_combined <- p_EOL_grid

ggsave(
  filename = "Results/Graphs/Maps/EM_EOL_percap_patchwork.png",
  plot = p_EOL_combined, width = 10, height = 7.3, dpi = 300
)

# Super combined panel (all maps)
p_super_grid <-
  add_gap(p_map_fresh) + add_gap(p_map_coast) + add_gap(p_map_soil) + add_gap(p_eol_sls) +
  add_gap(p_mp_fresh)  + add_gap(p_mp_coast)  + add_gap(p_mp_soil)  + add_gap(p_eol_slm) +
  add_gap(p_mp_subsoil)+ add_gap(p_em_total)  + add_gap(p_eol_inc)   + add_gap(p_eol_lan) +
  plot_layout(ncol = 4, guides = "keep", widths = c(1, 1, 1, 1), heights = c(1, 1, 1))

p_super_combined <- p_super_grid

ggsave(
  filename = "Results/Graphs/Maps/EM_super_patchwork.png",
  plot = p_super_combined,
  width = 17, height = 17, dpi = 200
)

#### Export map plot data #####################################################

summarise_mean_sd <- function(df, mean_col, sd_col) {
  df %>%
    group_by(country) %>%
    summarise(
      mean = sum(mean, na.rm = TRUE),
      sd = sqrt(sum(sd^2, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rename(
      !!mean_col := mean,
      !!sd_col := sd
    )
}

maps_country_names <- openxlsx::read.xlsx("Input/20260618_FeedData.xlsx", sheet = "GeoCode") %>%
  transmute(
    country = as.character(Abbreviation),
    country_name = trimws(gsub(intToUtf8(160), "", as.character(Country.or.region), fixed = TRUE))
  ) %>%
  filter(country %in% europe_countries)

maps_export <- maps_country_names %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Fresh water"),
      "MaP_Fresh_water_mean_g_cap",
      "MaP_Fresh_water_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Coastal and ocean water"),
      "MaP_Coastal_and_ocean_water_mean_g_cap",
      "MaP_Coastal_and_ocean_water_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Soil"),
      "MaP_Soil_mean_g_cap",
      "MaP_Soil_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_EOL_percap_data %>% filter(dest == "Sludge amended soil"),
      "MaP_Sludge_amended_soil_mean_g_cap",
      "MaP_Sludge_amended_soil_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Fresh water (MP)"),
      "MP_Fresh_water_mean_g_cap",
      "MP_Fresh_water_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Coastal and ocean water (MP)"),
      "MP_Coastal_and_ocean_water_mean_g_cap",
      "MP_Coastal_and_ocean_water_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Soil (MP)"),
      "MP_Soil_mean_g_cap",
      "MP_Soil_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_EOL_percap_data %>% filter(dest == "Sludge amended soil (MP)"),
      "MP_Sludge_amended_soil_mean_g_cap",
      "MP_Sludge_amended_soil_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data %>% filter(dest == "Subsurface soil (MP)"),
      "MP_Subsurface_soil_mean_g_cap",
      "MP_Subsurface_soil_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_percap_data,
      "Total_MP_MaP_mean_g_cap",
      "Total_MP_MaP_sd_g_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_EOL_percap_data %>%
        filter(dest == "Incineration") %>%
        mutate(mean = mean / 1000, sd = sd / 1000),
      "EOL_Incineration_mean_kg_cap",
      "EOL_Incineration_sd_kg_cap"
    ),
    by = "country"
  ) %>%
  left_join(
    summarise_mean_sd(
      EM_EOL_percap_data %>%
        filter(dest == "Landfill") %>%
        mutate(mean = mean / 1000, sd = sd / 1000),
      "EOL_Landfill_mean_kg_cap",
      "EOL_Landfill_sd_kg_cap"
    ),
    by = "country"
  ) %>%
  arrange(country_name)

write.csv(
  maps_export,
  file = "Results/Graphs/Maps/EM_super_patchwork_data.csv",
  row.names = FALSE
)
