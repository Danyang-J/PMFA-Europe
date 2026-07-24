library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

# ------------------------------------------------------------------------------
# Country total-emission versus MP-fraction scatter plot
# ------------------------------------------------------------------------------
in_dir <- "Results/Tables/EM"
out_png <- "Results/Graphs/Country_MP_MaP_Scatter.png"

group_cols <- c(
  "Soil-dominant high emissions" = "#DD8585",
  "Marine-linked emissions" = "#74A9CF",
  "High microplastic-share emissions" = "#B981B9",
  "Mixed emissions profile" = "#858585"
)

group_labels <- c(
  "Soil-dominant high emissions" = "G1: Soil-dominant high emissions",
  "Marine-linked emissions" = "G2: Marine-linked emissions",
  "High microplastic-share emissions" = "G3: High microplastic-share emissions",
  "Mixed emissions profile" = "G4: Mixed emissions profile"
)

# Visual tuning
bar_linewidth <- 1.0
bar_alpha <- 0.40

files <- list.files(in_dir, pattern = "^EM_prodpol_[A-Z]{2}\\.csv$", full.names = TRUE)
country_codes <- tools::file_path_sans_ext(sub("^EM_prodpol_", "", basename(files)))
keep <- country_codes != "EU"
files <- files[keep]
country_codes <- country_codes[keep]

country_df <- bind_rows(lapply(seq_along(files), function(i) {
  d <- read.csv(files[i], check.names = FALSE)
  is_mp <- grepl("\\(MP\\)", d$dest)

  total <- sum(d$EM.percap.mean, na.rm = TRUE)
  total_sd <- sqrt(sum(d$EM.percap.sd^2, na.rm = TRUE))
  mp_amount <- sum(d$EM.percap.mean[is_mp], na.rm = TRUE)
  map_amount <- sum(d$EM.percap.mean[!is_mp], na.rm = TRUE)
  mp_sd <- sqrt(sum(d$EM.percap.sd[is_mp]^2, na.rm = TRUE))
  map_sd <- sqrt(sum(d$EM.percap.sd[!is_mp]^2, na.rm = TRUE))

  mp_fraction <- ifelse(total > 0, 100 * mp_amount / total, 0)
  mp_fraction_sd <- ifelse(
    total > 0,
    100 * sqrt((map_amount / total^2)^2 * mp_sd^2 +
                 (mp_amount / total^2)^2 * map_sd^2),
    0
  )

  coastal_amount <- sum(d$EM.percap.mean[grepl("^Coastal and ocean water", d$dest)], na.rm = TRUE)
  coastal_share <- ifelse(total > 0, coastal_amount / total, 0)

  tibble(
    country = country_codes[i],
    total = total,
    total_sd = total_sd,
    mp_fraction = mp_fraction,
    mp_fraction_sd = mp_fraction_sd,
    coastal_share = coastal_share
  )
})) %>%
  mutate(
    group = case_when(
      coastal_share >= 0.10 ~ "Marine-linked emissions",
      mp_fraction >= 30 ~ "High microplastic-share emissions",
      total >= 400 ~ "Soil-dominant high emissions",
      TRUE ~ "Mixed emissions profile"
    ),
    group = factor(group, levels = names(group_cols)),
    xmin = pmax(0, total - total_sd),
    xmax = total + total_sd,
    ymin = pmax(0, mp_fraction - mp_fraction_sd),
    ymax = mp_fraction + mp_fraction_sd
  ) %>%
  filter(is.finite(total), is.finite(mp_fraction))

x_upper <- max(country_df$xmax, na.rm = TRUE) * 1.07
y_upper <- max(country_df$ymax, na.rm = TRUE) * 1.10

p <- ggplot(country_df, aes(x = total, y = mp_fraction)) +
  geom_vline(xintercept = 400, colour = "#FB8072", alpha = 0.85, linewidth = 0.8) +
  geom_hline(yintercept = 30, colour = "#FB8072", alpha = 0.85, linewidth = 0.8) +
  geom_segment(
    data = dplyr::filter(country_df, is.finite(xmin), is.finite(xmax)),
    aes(
      x = xmin, xend = xmax,
      y = mp_fraction, yend = mp_fraction,
      colour = group
    ),
    linewidth = bar_linewidth,
    alpha = bar_alpha,
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_segment(
    data = dplyr::filter(country_df, is.finite(ymin), is.finite(ymax)),
    aes(
      x = total, xend = total,
      y = ymin, yend = ymax,
      colour = group
    ),
    linewidth = bar_linewidth,
    alpha = bar_alpha,
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_point(
    aes(fill = group),
    shape = 21, size = 3.6, colour = "black", stroke = 0.25
  ) +
  ggrepel::geom_text_repel(
    aes(label = country),
    colour = "black", size = 3.1, seed = 123,
    box.padding = 0.30, point.padding = 0.10,
    force = 1.0, force_pull = 0.1,
    max.iter = 20000, max.overlaps = Inf,
    min.segment.length = 0,
    segment.color = "grey45", segment.alpha = 0.45, segment.size = 0.25,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = group_cols,
    breaks = names(group_cols),
    labels = unname(group_labels[names(group_cols)]),
    name = NULL,
    guide = guide_legend(
      override.aes = list(shape = 21, size = 4.2, colour = "black", stroke = 0.25)
    )
  ) +
  scale_colour_manual(values = group_cols, guide = "none") +
  scale_x_continuous(
    name = "Total emissions per capita (g/cap)",
    limits = c(0, x_upper),
    breaks = seq(0, x_upper, by = 200),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    name = "Microplastic fraction of total emissions (%)",
    limits = c(0, y_upper),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(clip = "off") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.35),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.85),
      colour = "grey70",
      linewidth = 0
    ),
    legend.key = element_blank(),
    legend.text = element_text(size = 8.5),
    plot.margin = margin(5.5, 12, 5.5, 5.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    text = element_text(family = "Calibri")
  )+
  guides(fill = guide_legend(nrow = 4))

ggsave(out_png, plot = p, width = 7, height = 5, dpi = 400, bg = "white")
message("Saved: ", out_png)
