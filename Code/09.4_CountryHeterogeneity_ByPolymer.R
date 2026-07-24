library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(cowplot)

# ------------------------------------------------------------------------------
# Input / output
# ------------------------------------------------------------------------------
in_dir <- "Results/Tables/EM"
out_png <- "Results/Graphs/CountryHeterogeneity_ByPolymer_MaP_MP.png"

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

# ------------------------------------------------------------------------------
# Read country-level polymer data
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
      polymer = .[[2]],
      dest = .[[3]],
      em_percap_mean = .[[8]],
      em_percap_sd = .[[9]]
    )
}

df_raw_country <- imap_dfr(
  setNames(vector("list", length(systems)), systems),
  ~ read_country_file(.y)
)

df_country <- df_raw_country %>%
  mutate(
    phase = ifelse(grepl(" \\(MP\\)$", dest), "MP", "MaP")
  ) %>%
  group_by(country, phase, polymer) %>%
  summarise(
    em_percap_mean = sum(em_percap_mean, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Read EU benchmark from the EU file only
# ------------------------------------------------------------------------------
df_eu <- read_country_file("EU") %>%
  mutate(
    phase = ifelse(grepl(" \\(MP\\)$", dest), "MP", "MaP")
  ) %>%
  group_by(phase, polymer) %>%
  summarise(
    eu_percap = sum(em_percap_mean, na.rm = TRUE),
    eu_sd = sqrt(sum(em_percap_sd^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Phase-specific ordering
# ------------------------------------------------------------------------------
phase_order <- df_country %>%
  group_by(phase, polymer) %>%
  summarise(
    polymer_max = max(em_percap_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(polymer_max > 0) %>%
  left_join(
    df_eu %>% select(phase, polymer, eu_percap),
    by = c("phase", "polymer")
  ) %>%
  filter(!is.na(eu_percap)) %>%
  group_by(phase) %>%
  arrange(desc(eu_percap), .by_group = TRUE) %>%
  mutate(polymer_rank = row_number()) %>%
  ungroup() %>%
  select(phase, polymer, polymer_rank)

df_country <- df_country %>%
  left_join(phase_order, by = c("phase", "polymer")) %>%
  filter(!is.na(polymer_rank))

df_eu <- df_eu %>%
  left_join(phase_order, by = c("phase", "polymer")) %>%
  filter(!is.na(polymer_rank))

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------
plot_phase <- function(phase_target,
                       title_override = NULL,
                       show_note = TRUE,
                       plot_margins = margin(t = 6, r = 20, b = 6, l = 20)) {
  df_country_phase <- df_country %>%
    filter(phase == phase_target) %>%
    arrange(desc(polymer_rank)) %>%
    mutate(
      polymer = factor(polymer, levels = unique(polymer)),
      polymer_num = seq_along(levels(polymer))[match(polymer, levels(polymer))]
    )

  df_eu_phase <- df_eu %>%
    filter(phase == phase_target) %>%
    mutate(
      polymer = factor(polymer, levels = levels(df_country_phase$polymer)),
      polymer_num = seq_along(levels(df_country_phase$polymer))[match(polymer, levels(df_country_phase$polymer))]
    ) %>%
    filter(!is.na(eu_percap), !is.na(polymer))

  df_outlier_phase <- df_country_phase %>%
    group_by(polymer, polymer_num) %>%
    mutate(
      q1 = quantile(em_percap_mean, 0.25, na.rm = TRUE, names = FALSE),
      q3 = quantile(em_percap_mean, 0.75, na.rm = TRUE, names = FALSE),
      iqr = q3 - q1,
      lower_fence = q1 - 1.5 * iqr,
      upper_fence = q3 + 1.5 * iqr,
      is_outlier = em_percap_mean < lower_fence | em_percap_mean > upper_fence
    ) %>%
    ungroup() %>%
    filter(is_outlier)

  p <- ggplot(df_country_phase, aes(x = em_percap_mean, y = polymer_num)) +
    geom_boxplot(
      aes(group = polymer_num),
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
        y = polymer_num,
        yend = polymer_num
      ),
      inherit.aes = FALSE,
      color = eu_col,
      linewidth = 0.95
    ) +
    geom_point(
      data = df_eu_phase,
      aes(x = eu_percap, y = polymer_num),
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
      nudge_y = 0.28,
      direction = "both",
      hjust = 0.5,
      box.padding = 0.22,
      point.padding = 0.18,
      force = 1.25,
      force_pull = 1.5,
      max.iter = 20000,
      min.segment.length = 0,
      segment.color = "grey45",
      segment.alpha = 0.5,
      segment.size = 0.25,
      show.legend = FALSE,
      max.overlaps = Inf
    ) +
    scale_x_continuous(
      limits = c(0, NA),
      breaks = scales::breaks_pretty(n = 7),
      expand = expansion(mult = c(0, 0.14))
    ) +
    scale_y_continuous(
      breaks = seq_along(levels(df_country_phase$polymer)),
      labels = levels(df_country_phase$polymer),
      limits = c(0.5, length(levels(df_country_phase$polymer)) + 0.5),
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

p_map <- plot_phase(
  "MaP",
  title_override = phase_labels[["MaP"]],
  show_note = TRUE,
  plot_margins = margin(t = 6, r = 30, b = 6, l = 20)
)

p_mp <- plot_phase(
  "MP",
  title_override = phase_labels[["MP"]],
  show_note = TRUE,
  plot_margins = margin(t = 6, r = 30, b = 6, l = 20)
)

p <- cowplot::plot_grid(
  p_map,
  p_mp,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "v",
  axis = "tb"
)

ggsave(
  filename = out_png,
  plot = p,
  width = 13.4,
  height = 5.2,
  dpi = 450,
  bg = "white"
)

message("Saved: ", out_png)
