# Finalise TLS crown validation without reprocessing point clouds

# This script uses the results produced by:
# scripts/tls/14_validate_crown_metrics.R
#
# Important interpretation:
# - crown_length_r behaves like the TLS point-cloud vertical extent.
# - It is not the same as tree height minus field crown-base height.
# - Crown diameter 2 is our preferred crown-width measurement.
# - Our calculated crown area is specifically a projected convex-hull area.
# - Large differences are flagged for investigation, not labelled as failures.

input_file <-
  "outputs/tls/tables/tls_crown_metrics_validation.csv"

if (!file.exists(input_file)) {
  stop(
    "The crown validation table was not found: ",
    input_file
  )
}

crown_results <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Find columns safely -----------------------------------------------------

find_column <- function(data, possible_names, description) {
  available <- possible_names[
    possible_names %in% names(data)
  ]

  if (length(available) == 0) {
    stop(
      "Could not find the column for ",
      description,
      ".\nAvailable columns are:\n",
      paste(names(data), collapse = ", ")
    )
  }

  available[1]
}

tree_id_column <- find_column(
  crown_results,
  c("tree_id", "ID"),
  "tree ID"
)

species_column <- find_column(
  crown_results,
  c("species", "Species"),
  "species"
)

calculated_height_column <- find_column(
  crown_results,
  c(
    "calculated_tree_height_m",
    "calculated_height_m"
  ),
  "calculated point-cloud height"
)

reference_height_column <- find_column(
  crown_results,
  c(
    "reference_tree_height_m",
    "tree_height"
  ),
  "reference tree height"
)

reference_vertical_extent_column <- find_column(
  crown_results,
  c(
    "reference_crown_length_m",
    "crown_length_r"
  ),
  "reference crown_length_r variable"
)

calculated_diameter_1_column <- find_column(
  crown_results,
  c(
    "calculated_raw_crown_diameter_1_m",
    "calculated_crown_diameter_1_m",
    "calculated_crown_dia1_m"
  ),
  "calculated crown diameter 1"
)

calculated_diameter_1_pca_column <- find_column(
  crown_results,
  c(
    "calculated_pca_crown_diameter_1_m",
    "calculated_crown_diameter_1_pca_m",
    "calculated_pca_crown_diameter_1_m",
    "calculated_crown_dia1_pca_m"
  ),
  "PCA-rotated crown diameter"
)

reference_diameter_1_column <- find_column(
  crown_results,
  c(
    "reference_crown_diameter_1_m",
    "crown_dia1"
  ),
  "reference crown diameter 1"
)

calculated_diameter_2_column <- find_column(
  crown_results,
  c(
    "calculated_crown_diameter_2_m",
    "calculated_crown_dia2_m"
  ),
  "calculated crown diameter 2"
)

reference_diameter_2_column <- find_column(
  crown_results,
  c(
    "reference_crown_diameter_2_m",
    "crown_dia2"
  ),
  "reference crown diameter 2"
)

calculated_area_column <- find_column(
  crown_results,
  c(
    "calculated_crown_area_m2",
    "calculated_convex_hull_area_m2",
    "projected_convex_hull_area_m2"
  ),
  "calculated crown area"
)

reference_area_column <- find_column(
  crown_results,
  c(
    "reference_crown_area_m2",
    "crown_area"
  ),
  "reference crown area"
)

# Create a clearly named corrected table ---------------------------------

corrected_results <- data.frame(
  tree_id =
    crown_results[[tree_id_column]],

  species =
    crown_results[[species_column]],

  calculated_point_cloud_height_m =
    as.numeric(
      crown_results[[calculated_height_column]]
    ),

  reference_tree_height_m =
    as.numeric(
      crown_results[[reference_height_column]]
    ),

  reference_tls_vertical_extent_m =
    as.numeric(
      crown_results[[reference_vertical_extent_column]]
    ),

  calculated_crown_diameter_1_xy_m =
    as.numeric(
      crown_results[[calculated_diameter_1_column]]
    ),

  calculated_crown_diameter_1_pca_m =
    as.numeric(
      crown_results[[calculated_diameter_1_pca_column]]
    ),

  reference_crown_diameter_1_m =
    as.numeric(
      crown_results[[reference_diameter_1_column]]
    ),

  calculated_crown_diameter_2_hull_m =
    as.numeric(
      crown_results[[calculated_diameter_2_column]]
    ),

  reference_crown_diameter_2_m =
    as.numeric(
      crown_results[[reference_diameter_2_column]]
    ),

  calculated_projected_convex_hull_area_m2 =
    as.numeric(
      crown_results[[calculated_area_column]]
    ),

  reference_crown_area_m2 =
    as.numeric(
      crown_results[[reference_area_column]]
    ),

  stringsAsFactors = FALSE
)

# Calculate differences --------------------------------------------------

corrected_results$vertical_extent_difference_m <-
  corrected_results$calculated_point_cloud_height_m -
  corrected_results$reference_tls_vertical_extent_m

corrected_results$crown_diameter_1_xy_error_m <-
  corrected_results$calculated_crown_diameter_1_xy_m -
  corrected_results$reference_crown_diameter_1_m

corrected_results$crown_diameter_1_pca_error_m <-
  corrected_results$calculated_crown_diameter_1_pca_m -
  corrected_results$reference_crown_diameter_1_m

corrected_results$crown_diameter_2_error_m <-
  corrected_results$calculated_crown_diameter_2_hull_m -
  corrected_results$reference_crown_diameter_2_m

corrected_results$crown_diameter_2_absolute_error_m <-
  abs(corrected_results$crown_diameter_2_error_m)

corrected_results$projected_area_difference_m2 <-
  corrected_results$
  calculated_projected_convex_hull_area_m2 -
  corrected_results$reference_crown_area_m2

# Use crown diameter 2 as the preferred width result ---------------------

corrected_results$preferred_crown_diameter_m <-
  corrected_results$calculated_crown_diameter_2_hull_m

corrected_results$preferred_reference_diameter_m <-
  corrected_results$reference_crown_diameter_2_m

# A 1 m difference is used only to create an investigation queue.
# It is not a pass/fail test of geometric validity.

diameter_review_threshold_m <- 1

has_diameter_comparison <-
  is.finite(
    corrected_results$preferred_crown_diameter_m
  ) &
  is.finite(
    corrected_results$preferred_reference_diameter_m
  )

corrected_results$comparison_flag <-
  "comparison unavailable"

corrected_results$comparison_flag[
  has_diameter_comparison
] <- "below review threshold"

corrected_results$comparison_flag[
  has_diameter_comparison &
    corrected_results$
    crown_diameter_2_absolute_error_m >=
    diameter_review_threshold_m
] <- "large disagreement: investigate"

# Add withheld-point information if the audit is available ---------------

audit_file <-
  "outputs/tls/tables/tls_withheld_point_audit.csv"

corrected_results$withheld_points <- NA_real_
corrected_results$withheld_percent <- NA_real_
corrected_results$withheld_point_note <- ""

if (file.exists(audit_file)) {
  withheld_audit <- read.csv(
    audit_file,
    stringsAsFactors = FALSE
  )

  if (
    all(
      c(
        "tree_id",
        "withheld_points",
        "withheld_percent"
      ) %in% names(withheld_audit)
    )
  ) {
    audit_rows <- match(
      corrected_results$tree_id,
      withheld_audit$tree_id
    )

    corrected_results$withheld_points <-
      withheld_audit$withheld_points[audit_rows]

    corrected_results$withheld_percent <-
      withheld_audit$withheld_percent[audit_rows]

    all_withheld <-
      is.finite(
        corrected_results$withheld_percent
      ) &
      corrected_results$withheld_percent >= 99.999

    corrected_results$withheld_point_note[
      all_withheld
    ] <- paste(
      "All points carry the withheld flag;",
      "points were retained because this behaves",
      "as a file-level export flag."
    )
  }
}

# Accuracy calculations --------------------------------------------------

calculate_accuracy <- function(
    calculated,
    reference,
    metric,
    unit,
    interpretation
) {
  valid <-
    is.finite(calculated) &
    is.finite(reference)

  calculated <- calculated[valid]
  reference <- reference[valid]

  if (length(calculated) == 0) {
    return(
      data.frame(
        metric = metric,
        unit = unit,
        comparisons = 0,
        mean_error = NA_real_,
        mean_absolute_error = NA_real_,
        root_mean_square_error = NA_real_,
        correlation = NA_real_,
        interpretation = interpretation,
        stringsAsFactors = FALSE
      )
    )
  }

  error <- calculated - reference

  data.frame(
    metric = metric,
    unit = unit,
    comparisons = length(error),
    mean_error = mean(error),
    mean_absolute_error = mean(abs(error)),
    root_mean_square_error =
      sqrt(mean(error^2)),
    correlation = if (
      length(error) >= 2 &&
      stats::sd(calculated) > 0 &&
      stats::sd(reference) > 0
    ) {
      stats::cor(calculated, reference)
    } else {
      NA_real_
    },
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

accuracy_summary <- rbind(
  calculate_accuracy(
    calculated =
      corrected_results$
      calculated_point_cloud_height_m,
    reference =
      corrected_results$
      reference_tls_vertical_extent_m,
    metric =
      "TLS vertical extent diagnostic",
    unit = "m",
    interpretation = paste(
      "Compares point-cloud height with crown_length_r.",
      "This is not field-derived crown length."
    )
  ),

  calculate_accuracy(
    calculated =
      corrected_results$
      calculated_crown_diameter_1_xy_m,
    reference =
      corrected_results$
      reference_crown_diameter_1_m,
    metric =
      "Crown diameter 1: original XY mean extent",
    unit = "m",
    interpretation =
      "Direct comparison with the published diameter-1 variable."
  ),

  calculate_accuracy(
    calculated =
      corrected_results$
      calculated_crown_diameter_1_pca_m,
    reference =
      corrected_results$
      reference_crown_diameter_1_m,
    metric =
      "Crown diameter 1: PCA-rotated mean extent",
    unit = "m",
    interpretation =
      "Alternative orientation-adjusted diameter estimate."
  ),

  calculate_accuracy(
    calculated =
      corrected_results$
      calculated_crown_diameter_2_hull_m,
    reference =
      corrected_results$
      reference_crown_diameter_2_m,
    metric =
      "Crown diameter 2: convex-hull method",
    unit = "m",
    interpretation =
      "Preferred crown-width result in this workflow."
  ),

  calculate_accuracy(
    calculated =
      corrected_results$
      calculated_projected_convex_hull_area_m2,
    reference =
      corrected_results$
      reference_crown_area_m2,
    metric =
      "Projected 2D convex-hull area diagnostic",
    unit = "m2",
    interpretation = paste(
      "Systematic disagreement indicates that the two area",
      "variables are not currently methodologically equivalent."
    )
  )
)

# Create the diameter inspection queue -----------------------------------

diameter_inspection_queue <- corrected_results[
  corrected_results$comparison_flag ==
    "large disagreement: investigate",
  c(
    "tree_id",
    "species",
    "preferred_reference_diameter_m",
    "preferred_crown_diameter_m",
    "crown_diameter_2_error_m",
    "crown_diameter_2_absolute_error_m",
    "comparison_flag",
    "withheld_points",
    "withheld_percent",
    "withheld_point_note"
  )
]

diameter_inspection_queue <-
  diameter_inspection_queue[
    order(
      diameter_inspection_queue$
        crown_diameter_2_absolute_error_m,
      decreasing = TRUE
    ),
  ]

row.names(diameter_inspection_queue) <- NULL

# Save corrected tables --------------------------------------------------

output_directory <- "outputs/tls/tables"

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  corrected_results,
  file.path(
    output_directory,
    "tls_crown_validation_corrected.csv"
  ),
  row.names = FALSE
)

write.csv(
  accuracy_summary,
  file.path(
    output_directory,
    "tls_crown_accuracy_summary_corrected.csv"
  ),
  row.names = FALSE
)

write.csv(
  diameter_inspection_queue,
  file.path(
    output_directory,
    "tls_crown_diameter_inspection_queue.csv"
  ),
  row.names = FALSE
)

# Create corrected validation figure ------------------------------------

figure_file <- paste0(
  "outputs/tls/figures/",
  "tls_crown_validation_corrected.png"
)

dir.create(
  dirname(figure_file),
  recursive = TRUE,
  showWarnings = FALSE
)

padded_range <- function(x, padding_fraction = 0.05) {
  limits <- range(x, finite = TRUE)

  if (!all(is.finite(limits))) {
    return(c(0, 1))
  }

  width <- diff(limits)

  if (width == 0) {
    width <- max(abs(limits), 1) * 0.1
  }

  limits + c(-1, 1) * width * padding_fraction
}

plot_comparison <- function(
    reference,
    calculated,
    x_label,
    y_label,
    main_title,
    labels = NULL,
    flagged = NULL
) {
  valid <-
    is.finite(reference) &
    is.finite(calculated)

  reference <- reference[valid]
  calculated <- calculated[valid]

  if (!is.null(labels)) {
    labels <- labels[valid]
  }

  if (!is.null(flagged)) {
    flagged <- flagged[valid]
  }

  if (length(reference) == 0) {
    plot.new()
    title(main = paste(main_title, "— no valid data"))
    return(invisible(NULL))
  }

  limits <- padded_range(
    c(reference, calculated)
  )

  point_background <- rep(
    "#347847",
    length(reference)
  )

  point_border <- rep(
    "#245632",
    length(reference)
  )

  point_size <- rep(
    1,
    length(reference)
  )

  if (!is.null(flagged)) {
    point_background[flagged] <- "#F4A261"
    point_border[flagged] <- "#B22222"
    point_size[flagged] <- 1.35
  }

  plot(
    reference,
    calculated,
    pch = 21,
    bg = point_background,
    col = point_border,
    cex = point_size,
    lwd = 1.4,
    xlim = limits,
    ylim = limits,
    xlab = x_label,
    ylab = y_label,
    main = main_title
  )

  abline(
    a = 0,
    b = 1,
    lty = 2,
    lwd = 2,
    col = "grey40"
  )

  if (
    !is.null(flagged) &&
    !is.null(labels) &&
    any(flagged)
  ) {
    text(
      reference[flagged],
      calculated[flagged],
      labels = labels[flagged],
      pos = 3,
      cex = 0.65,
      col = "#A51C30"
    )
  }

  invisible(NULL)
}

png(
  figure_file,
  width = 2200,
  height = 1800,
  res = 220
)

par(
  mfrow = c(2, 2),
  mar = c(4.7, 4.7, 3.4, 1.2)
)

plot_comparison(
  reference =
    corrected_results$
    reference_tls_vertical_extent_m,
  calculated =
    corrected_results$
    calculated_point_cloud_height_m,
  x_label =
    "Reference crown_length_r variable (m)",
  y_label =
    "Calculated point-cloud height (m)",
  main_title =
    "TLS vertical-extent diagnostic"
)

plot_comparison(
  reference =
    corrected_results$
    reference_crown_diameter_1_m,
  calculated =
    corrected_results$
    calculated_crown_diameter_1_xy_m,
  x_label =
    "Reference crown diameter 1 (m)",
  y_label =
    "Calculated XY mean extent (m)",
  main_title =
    "Crown diameter 1"
)

diameter_2_flagged <-
  corrected_results$comparison_flag ==
  "large disagreement: investigate"

plot_comparison(
  reference =
    corrected_results$
    reference_crown_diameter_2_m,
  calculated =
    corrected_results$
    calculated_crown_diameter_2_hull_m,
  x_label =
    "Reference crown diameter 2 (m)",
  y_label =
    "Calculated hull diameter (m)",
  main_title =
    "Preferred crown diameter",
  labels = corrected_results$tree_id,
  flagged = diameter_2_flagged
)

plot_comparison(
  reference =
    corrected_results$reference_crown_area_m2,
  calculated =
    corrected_results$
    calculated_projected_convex_hull_area_m2,
  x_label =
    "Published crown-area variable (m²)",
  y_label =
    "Projected convex-hull area (m²)",
  main_title =
    "Crown-area method mismatch"
)

dev.off()

# Print final results -----------------------------------------------------

cat(
  "\nCorrected crown-validation summary:\n"
)

print(
  accuracy_summary,
  row.names = FALSE
)

cat(
  "\nCrown-diameter comparisons requiring investigation:",
  nrow(diameter_inspection_queue),
  "\n"
)

if (nrow(diameter_inspection_queue) > 0) {
  print(
    diameter_inspection_queue[
      ,
      c(
        "tree_id",
        "species",
        "preferred_reference_diameter_m",
        "preferred_crown_diameter_m",
        "crown_diameter_2_error_m",
        "crown_diameter_2_absolute_error_m"
      )
    ],
    row.names = FALSE
  )
}

cat(
  paste0(
    "\nImportant interpretation:\n",
    "- crown_length_r closely follows point-cloud vertical extent.\n",
    "- It is not treated as field-derived crown length.\n",
    "- Crown diameter 2 is the preferred crown-width result.\n",
    "- Projected convex-hull area is retained as a separate ",
    "experimental metric.\n"
  )
)

cat(
  "\nCorrected tables saved under outputs/tls/tables/.\n"
)

cat(
  "Corrected figure saved to:",
  figure_file,
  "\n"
)