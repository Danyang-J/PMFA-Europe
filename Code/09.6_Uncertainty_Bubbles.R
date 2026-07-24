library(dplyr)

in_dir <- "Results/Emissions"
out_dir <- "Results/Graphs/Uncertainty"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
plot_family <- "sans"

excel.path <- file.path("Input", excel.file)

build_label_map <- function() {
  if (!file.exists(excel.path)) {
    return(NULL)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }

  lab <- tryCatch(
    openxlsx::read.xlsx(excel.path, sheet = "Label"),
    error = function(e) NULL
  )
  if (is.null(lab) || !is.data.frame(lab)) {
    return(NULL)
  }

  nms <- names(lab)
  name_col <- nms[tolower(nms) == "name"]
  short_col <- nms[tolower(nms) %in% c("shortnicelabel", "short_nice_label", "label")]

  if (length(name_col) == 0 || length(short_col) == 0) {
    return(NULL)
  }

  out <- lab %>%
    transmute(
      Name = as.character(.data[[name_col[1]]]),
      ShortNiceLabel = as.character(.data[[short_col[1]]])
    ) %>%
    filter(!is.na(Name), Name != "") %>%
    mutate(
      ShortNiceLabel = ifelse(is.na(ShortNiceLabel) | ShortNiceLabel == "", Name, ShortNiceLabel)
    )

  if (nrow(out) == 0) {
    return(NULL)
  }

  setNames(out$ShortNiceLabel, out$Name)
}

label_map <- build_label_map()

build_label_order <- function() {
  if (!file.exists(excel.path) || !requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }
  lab <- tryCatch(openxlsx::read.xlsx(excel.path, sheet = "Label"), error = function(e) NULL)
  if (is.null(lab) || !is.data.frame(lab)) {
    return(NULL)
  }
  nms <- names(lab)
  name_col <- nms[tolower(nms) == "name"]
  if (length(name_col) == 0) {
    return(NULL)
  }
  out <- as.character(lab[[name_col[1]]])
  out <- out[!is.na(out) & out != ""]
  unique(out)
}

build_country_fullname_map <- function() {
  if (!file.exists(excel.path) || !requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }
  geo <- tryCatch(openxlsx::read.xlsx(excel.path, sheet = "GeoCode"), error = function(e) NULL)
  if (is.null(geo) || !is.data.frame(geo)) {
    return(NULL)
  }
  nms <- names(geo)
  abbr_col <- nms[tolower(nms) %in% c("abbreviation", "abbr", "code")]
  full_col <- nms[tolower(nms) %in% c("country.or.region", "country", "country_region")]
  if (length(abbr_col) == 0 || length(full_col) == 0) {
    return(NULL)
  }
  abbr <- trimws(as.character(geo[[abbr_col[1]]]))
  full <- as.character(geo[[full_col[1]]])
  full <- gsub("^[[:space:]\u00A0]+|[[:space:]\u00A0]+$", "", full, perl = TRUE)
  ok <- !is.na(abbr) & abbr != "" & !is.na(full) & full != ""
  if (!any(ok)) {
    return(NULL)
  }
  setNames(full[ok], abbr[ok])
}

label_order <- build_label_order()
country_fullname_map <- build_country_fullname_map()

plot_bubbles_subset <- function(data_mat, sub_set = NULL, y_axis_side = 2) {
  if (is.null(sub_set)) {
    sub_set <- seq_len(ncol(data_mat))
  }

  data_sub <- data_mat[, sub_set, drop = FALSE]

  cex_to_plot <- ((data_sub / max(data_mat, na.rm = TRUE))^(1 / 2)) * 6

  plot(c(1, nrow(data_sub)), c(1, ncol(data_sub)), type = "n",
       xlab = "", ylab = "", axes = FALSE,
       xlim = c(0.8, nrow(data_sub) + 0.4), ylim = c(1 + 1.5, ncol(data_sub) - 1.5))

  abline(h = seq_len(ncol(data_sub)), col = "gray95", lwd = 2)
  abline(v = seq_len(nrow(data_sub)), col = "gray95", lwd = 2)

  axis(y_axis_side, seq_len(ncol(data_sub)), colnames(data_sub), las = 2, cex.axis = 1.0)
  axis(1, seq_len(nrow(data_sub)), rownames(data_sub), las = 1, cex.axis = 1.4, font = 2)
  box()
  my_palette <- colorRampPalette(c("firebrick4", "tomato", "darkorange",
                                   "gold", "chartreuse3", "forestgreen"))(9)

  colors <- matrix(NA_character_, nrow(data_sub), ncol(data_sub),
                   dimnames = dimnames(data_sub))

  for (mat_i in seq_len(nrow(data_sub))) {
    for (comp_i in seq_len(ncol(data_sub))) {
      v <- data_sub[mat_i, comp_i]
      if (is.na(v)) {
        next
      } else if (v >= 0.8) {
        colors[mat_i, comp_i] <- my_palette[1]
      } else if (v >= 0.7) {
        colors[mat_i, comp_i] <- my_palette[2]
      } else if (v >= 0.6) {
        colors[mat_i, comp_i] <- my_palette[3]
      } else if (v >= 0.5) {
        colors[mat_i, comp_i] <- my_palette[4]
      } else if (v >= 0.4) {
        colors[mat_i, comp_i] <- my_palette[5]
      } else if (v >= 0.3) {
        colors[mat_i, comp_i] <- my_palette[6]
      } else if (v >= 0.2) {
        colors[mat_i, comp_i] <- my_palette[7]
      } else if (v >= 0.1) {
        colors[mat_i, comp_i] <- my_palette[8]
      } else {
        colors[mat_i, comp_i] <- my_palette[9]
      }
    }
  }

  for (mat_i in seq_len(nrow(data_sub))) {
    for (comp_i in seq_len(ncol(data_sub))) {
      themat <- rownames(data_sub)[mat_i]
      thecomp <- colnames(data_sub)[comp_i]
      vv <- data_sub[themat, thecomp]
      if (is.na(vv)) next

      points(mat_i, comp_i, pch = 21, cex = cex_to_plot[themat, thecomp],
             bg = colors[themat, thecomp], col = "black")
      text(x = mat_i + 0.3, y = comp_i, labels = round(vv * 100, digits = 0), adj = 0, cex = 0.82)
    }
  }
}

plot_country <- function(unc_mat, out_file, country_code) {
  n_comp <- ncol(unc_mat)
  split_idx <- floor(n_comp / 2)

  left_set <- seq_len(split_idx)
  right_set <- seq(from = split_idx + 1, to = n_comp)

  png(filename = out_file, width = 7200, height = 5200, res = 320)
  on.exit(dev.off(), add = TRUE)

  par(mfrow = c(1, 2), mgp = c(3, 1, 0), xpd = FALSE, oma = c(0, 0, 2.3, 0), family = plot_family)

  country_title <- if (!is.null(country_fullname_map) && country_code %in% names(country_fullname_map)) {
    country_fullname_map[[country_code]]
  } else {
    country_code
  }

  par(mar = c(3, 18, 1.2, 0.05))
  plot_bubbles_subset(unc_mat, right_set, y_axis_side = 2)

  par(mar = c(3, 0.8, 1.2, 18))
  plot_bubbles_subset(unc_mat, left_set, y_axis_side = 4)

  mtext(
    paste0("Relative Uncertainty in ", country_title, " (%)"),
    side = 3, outer = TRUE, line = 0.0, cex = 1.6, font = 2
  )
}

proc_files <- list.files(in_dir, pattern = "^ProcessedMass_[A-Z][A-Z][.]Rdata$", full.names = TRUE)
if (length(proc_files) == 0) {
  stop("No ProcessedMass_*.Rdata files found in Results/Emissions")
}

country_codes <- gsub("ProcessedMass_|[.]Rdata", "", basename(proc_files))
ord <- order(country_codes)
proc_files <- proc_files[ord]
country_codes <- country_codes[ord]

if (!"EU" %in% country_codes) {
  stop("ProcessedMass_EU.Rdata is missing. EU aggregated plot cannot be generated.")
}

for (idx in seq_along(proc_files)) {
  cc <- country_codes[idx]

  env <- new.env(parent = emptyenv())
  load(proc_files[idx], envir = env)

  if (!exists("Masses", envir = env)) {
    stop("Masses object missing in ", proc_files[idx])
  }

  Masses <- get("Masses", envir = env)
  if (length(dim(Masses)) != 3) {
    stop("Masses in ", proc_files[idx], " is not a 3D array.")
  }

  unc_mat <- apply(
    Masses,
    c(1, 2),
    function(x) {
      if (all(x == 0)) {
        return(NA_real_)
      }
      m <- mean(x)
      if (!is.finite(m) || m == 0) {
        return(NA_real_)
      }
      sd(x) / m
    }
  )

  comp_names <- colnames(unc_mat)
  comp_names <- comp_names[!(comp_names %in% c("ExcludedFlow"))]

  if (!is.null(label_order)) {
    comp_names <- label_order[label_order %in% comp_names]
  }

  comp_names <- rev(comp_names)

  unc_mat <- unc_mat[, comp_names, drop = FALSE]

  if (!is.null(label_map)) {
    nice_comp <- ifelse(comp_names %in% names(label_map), label_map[comp_names], comp_names)
    colnames(unc_mat) <- unname(nice_comp)
  }

  out_file <- file.path(out_dir, paste0("Uncertainty_Bubbles_", cc, ".png"))
  plot_country(unc_mat, out_file, cc)

  message("Saved: ", out_file)
}

message("Done. Generated ", length(proc_files), " uncertainty bubble plots in ", out_dir)
