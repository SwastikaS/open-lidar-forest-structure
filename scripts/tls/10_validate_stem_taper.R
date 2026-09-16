# Validate TLS stem-taper estimates against published TLS measurements

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

source("R/tls_processing.R")

measurement_heights <- c(1.3, 2, 4, 6)
reference_columns <- c("d1.3", "d2", "d4", "d6")

# Differences of 5 cm or more are queued for investigation.
# This threshold does not determine which measurement is correct.
comparison_difference_threshold_cm <- 5

standardise_tree_id <- function(x) {
  x <- toupper(trimws(x))

  sub(
    "^([A-Z]+)0+([0-9]+)$",
    "\\1\\2",
    x
  )
}

# Locate all isolated-tree TLS files --------------------------------------

tls_files <- list.files(
  "data/raw/tls",
  pattern = "[.]laz$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(tls_files) == 0) {
  stop("No TLS .laz files were found under data/raw/tls.")
}

tree_inventory <- data.frame(
  tree_id = toupper(
    sub("_.*$", "", basename(tls_files))
  ),
  file_name = basename(tls_files),
  file_path = tls_files,
  stringsAsFactors = FALSE
)

tree_inventory$match_id <-
  standardise_tree_id(tree_inventory$tree_id)

# Read published TLS measurements ----------------------------------------

reference_file <-
  "data/reference/tls/Tree Parameters TLS AD WL.csv"

if (!file.exists(reference_file)) {
  stop("Reference table was not found: ", reference_file)
}

reference <- read.csv(
  reference_file,
  sep = ";",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

required_reference_columns <- c(
  "ID",
  "species",
  reference_columns
)

missing_reference_columns <- setdiff(
  required_reference_columns,
  names(reference)
)

if (length(missing_reference_columns) > 0) {
  stop(
    "Missing reference columns: ",
    paste(missing_reference_columns, collapse = ", ")
  )
}

reference$match_id <-
  standardise_tree_id(reference$ID)

reference_rows <- match(
  tree_inventory$match_id,
  reference$match_id
)

tree_inventory$species <-
  reference$species[reference_rows]

# Process every tree ------------------------------------------------------

taper_results <- vector(
  "list",
  nrow(tree_inventory)
)

tree_summaries <- vector(
  "list",
  nrow(tree_inventory)
)

for (i in seq_len(nrow(tree_inventory))) {
  cat(
    "Processing",
    i,
    "of",
    nrow(tree_inventory),
    ":",
    tree_inventory$tree_id[i],
    "\n"
  )

  reference_row <- reference_rows[i]

  reference_diameters_cm <- rep(
    NA_real_,
    length(measurement_heights)
  )

  if (!is.na(reference_row)) {
    reference_diameters_cm <- as.numeric(
      reference[
        reference_row,
        reference_columns
      ]
    ) * 100
  }

  taper_result <- tryCatch(
    estimate_stem_taper(
      file_path = tree_inventory$file_path[i],
      measurement_heights = measurement_heights
    ),
    error = function(e) e
  )

  if (inherits(taper_result, "error")) {
    taper_results[[i]] <- data.frame(
      tree_id = tree_inventory$tree_id[i],
      species = tree_inventory$species[i],
      height_m = measurement_heights,
      reference_diameter_cm = reference_diameters_cm,
      estimated_diameter_cm = NA_real_,
      error_cm = NA_real_,
      absolute_error_cm = NA_real_,
      circle_rmse_mm = NA_real_,
      circumference_completeness_percent = NA_real_,
      points_used = NA_integer_,
      quality_flag = "failed",
      processing_status = "failed",
      error_message = conditionMessage(taper_result),
      stringsAsFactors = FALSE
    )

    tree_summaries[[i]] <- data.frame(
      tree_id = tree_inventory$tree_id[i],
      species = tree_inventory$species[i],
      successful_measurements = 0,
      requested_measurements = length(measurement_heights),
      mean_taper_cm_per_m = NA_real_,
      linear_taper_cm_per_m = NA_real_,
      lower_stem_displacement_m = NA_real_,
      lower_stem_lean_degrees = NA_real_,
      processing_status = "failed",
      error_message = conditionMessage(taper_result),
      stringsAsFactors = FALSE
    )

    next
  }

  tree_table <- taper_result$taper_table

  tree_table$tree_id <- tree_inventory$tree_id[i]
  tree_table$species <- tree_inventory$species[i]
  tree_table$reference_diameter_cm <- reference_diameters_cm
  tree_table$error_cm <-
    tree_table$estimated_diameter_cm -
    tree_table$reference_diameter_cm
  tree_table$absolute_error_cm <- abs(tree_table$error_cm)

  tree_table$processing_status <- ifelse(
    is.finite(tree_table$estimated_diameter_cm),
    "success",
    "failed"
  )

  tree_table$error_message <- NA_character_

  taper_results[[i]] <- tree_table[
    ,
    c(
      "tree_id",
      "species",
      "height_m",
      "reference_diameter_cm",
      "estimated_diameter_cm",
      "error_cm",
      "absolute_error_cm",
      "circle_rmse_mm",
      "circumference_completeness_percent",
      "points_used",
      "quality_flag",
      "processing_status",
      "error_message"
    )
  ]

  summary_row <- taper_result$summary

  tree_summaries[[i]] <- data.frame(
    tree_id = tree_inventory$tree_id[i],
    species = tree_inventory$species[i],
    successful_measurements = summary_row$successful_measurements,
    requested_measurements = summary_row$requested_measurements,
    mean_taper_cm_per_m = summary_row$mean_taper_cm_per_m,
    linear_taper_cm_per_m = summary_row$linear_taper_cm_per_m,
    lower_stem_displacement_m =
      summary_row$lower_stem_displacement_m,
    lower_stem_lean_degrees =
      summary_row$lower_stem_lean_degrees,
    processing_status = ifelse(
      summary_row$successful_measurements > 0,
      "success",
      "failed"
    ),
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )

  invisible(gc())
}

taper_results <- do.call(
  rbind,
  taper_results
)

tree_summaries <- do.call(
  rbind,
  tree_summaries
)

row.names(taper_results) <- NULL
row.names(tree_summaries) <- NULL

# Add comparison flags ----------------------------------------------------

has_both_values <-
  is.finite(taper_results$estimated_diameter_cm) &
  is.finite(taper_results$reference_diameter_cm)

large_difference <-
  has_both_values &
  taper_results$absolute_error_cm >=
  comparison_difference_threshold_cm

taper_results$comparison_flag <- "no comparison available"

taper_results$comparison_flag[has_both_values] <-
  "below review threshold"

taper_results$comparison_flag[large_difference] <-
  "large disagreement: investigate"

taper_results$comparison_note <- ""

ad11_4m <-
  taper_results$tree_id == "AD11" &
  is.finite(taper_results$height_m) &
  abs(taper_results$height_m - 4) < 0.001

taper_results$comparison_note[ad11_4m] <- paste(
  "One clear ring in 10 cm and 20 cm sections;",
  "reason for published TLS difference unresolved."
)

comparison_review <- taper_results[
  large_difference,
  c(
    "tree_id",
    "species",
    "height_m",
    "reference_diameter_cm",
    "estimated_diameter_cm",
    "error_cm",
    "absolute_error_cm",
    "circle_rmse_mm",
    "circumference_completeness_percent",
    "points_used",
    "quality_flag",
    "comparison_flag",
    "comparison_note"
  )
]

comparison_review <- comparison_review[
  order(
    comparison_review$absolute_error_cm,
    decreasing = TRUE
  ),
]

row.names(comparison_review) <- NULL

# Calculate validation statistics ----------------------------------------

valid_results <- taper_results[
  taper_results$processing_status == "success" &
    is.finite(taper_results$reference_diameter_cm) &
    is.finite(taper_results$estimated_diameter_cm),
]

calculate_accuracy <- function(data) {
  data.frame(
    measurements = nrow(data),
    mean_error_cm = mean(data$error_cm),
    mean_absolute_error_cm = mean(data$absolute_error_cm),
    root_mean_square_error_cm = sqrt(mean(data$error_cm^2)),
    within_1_cm_percent =
      mean(data$absolute_error_cm <= 1) * 100,
    within_2_cm_percent =
      mean(data$absolute_error_cm <= 2) * 100,
    within_5_cm_percent =
      mean(data$absolute_error_cm <= 5) * 100
  )
}

accuracy_by_height <- do.call(
  rbind,
  lapply(
    split(valid_results, valid_results$height_m),
    function(data) {
      data.frame(
        height_m = unique(data$height_m),
        calculate_accuracy(data)
      )
    }
  )
)

accuracy_by_height_and_quality <- do.call(
  rbind,
  lapply(
    split(
      valid_results,
      list(
        valid_results$height_m,
        valid_results$quality_flag
      ),
      drop = TRUE
    ),
    function(data) {
      data.frame(
        height_m = unique(data$height_m),
        quality_flag = unique(data$quality_flag),
        calculate_accuracy(data)
      )
    }
  )
)

row.names(accuracy_by_height) <- NULL
row.names(accuracy_by_height_and_quality) <- NULL

# Save tables -------------------------------------------------------------

dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  taper_results,
  "outputs/tls/tables/tls_stem_taper_validation.csv",
  row.names = FALSE
)

write.csv(
  taper_results,
  paste0(
    "outputs/tls/tables/",
    "tls_stem_taper_comparison_flags.csv"
  ),
  row.names = FALSE
)

write.csv(
  comparison_review,
  paste0(
    "outputs/tls/tables/",
    "tls_stem_taper_comparison_review.csv"
  ),
  row.names = FALSE
)

write.csv(
  tree_summaries,
  "outputs/tls/tables/tls_stem_structure_batch.csv",
  row.names = FALSE
)

write.csv(
  accuracy_by_height,
  paste0(
    "outputs/tls/tables/",
    "tls_taper_accuracy_by_height.csv"
  ),
  row.names = FALSE
)

write.csv(
  accuracy_by_height_and_quality,
  paste0(
    "outputs/tls/tables/",
    "tls_taper_accuracy_by_height_and_quality.csv"
  ),
  row.names = FALSE
)

# Create validation figure -----------------------------------------------

dir.create(
  "outputs/tls/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "outputs/tls/figures/tls_stem_taper_validation.png",
  width = 2200,
  height = 1800,
  res = 220
)

par(
  mfrow = c(2, 2),
  mar = c(4.5, 4.5, 3, 1)
)

for (height in measurement_heights) {
  plot_data <- valid_results[
    valid_results$height_m == height,
  ]

  if (nrow(plot_data) == 0) {
    plot.new()
    title(
      main = paste(
        "Stem height:",
        height,
        "m — no valid comparisons"
      )
    )
    next
  }

  plot_range <- range(
    c(
      plot_data$reference_diameter_cm,
      plot_data$estimated_diameter_cm
    ),
    na.rm = TRUE
  )

  plot_range <- plot_range + c(-2, 2)

  point_colours <- ifelse(
    plot_data$quality_flag == "acceptable",
    "#347847",
    "#E67E22"
  )

  point_borders <- ifelse(
    plot_data$comparison_flag ==
      "large disagreement: investigate",
    "#C62828",
    point_colours
  )

  point_sizes <- ifelse(
    plot_data$comparison_flag ==
      "large disagreement: investigate",
    1.4,
    1
  )

  plot(
    plot_data$reference_diameter_cm,
    plot_data$estimated_diameter_cm,
    pch = 21,
    bg = point_colours,
    col = point_borders,
    cex = point_sizes,
    lwd = 1.5,
    xlim = plot_range,
    ylim = plot_range,
    xlab = "Published TLS diameter (cm)",
    ylab = "Our estimated diameter (cm)",
    main = paste("Stem height:", height, "m")
  )

  abline(
    a = 0,
    b = 1,
    lty = 2,
    lwd = 2,
    col = "grey35"
  )

  flagged_here <- plot_data[
    plot_data$comparison_flag ==
      "large disagreement: investigate",
  ]

  if (nrow(flagged_here) > 0) {
    text(
      flagged_here$reference_diameter_cm,
      flagged_here$estimated_diameter_cm,
      labels = flagged_here$tree_id,
      pos = 3,
      cex = 0.65,
      col = "#A51C30"
    )
  }
}

dev.off()

# Print results -----------------------------------------------------------

cat("\nAccuracy by measurement height:\n")
print(accuracy_by_height)

cat("\nAccuracy by height and geometric quality:\n")
print(accuracy_by_height_and_quality)

cat(
  "\nComparisons differing by at least",
  comparison_difference_threshold_cm,
  "cm:\n"
)

print(
  comparison_review[
    ,
    c(
      "tree_id",
      "height_m",
      "reference_diameter_cm",
      "estimated_diameter_cm",
      "error_cm",
      "quality_flag",
      "comparison_flag"
    )
  ],
  row.names = FALSE
)

cat(
  "\nLarge disagreements:",
  nrow(comparison_review),
  "\n"
)

cat("\nValidation outputs saved under outputs/tls/.\n")
