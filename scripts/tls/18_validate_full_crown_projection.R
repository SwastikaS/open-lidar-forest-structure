# Validate crown dimensions using complete tree projections

# This script recalculates crown dimensions for all isolated-tree TLS files.
# It does not use field crown-base height to remove points.
#
# Each input file already contains one segmented tree. The complete XY
# projection is therefore used to calculate projected crown dimensions.
#
# Existing crown-validation outputs are not overwritten.

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

# Settings ----------------------------------------------------------------

reference_file <-
  "data/reference/tls/Tree Parameters TLS AD WL.csv"

diameter_review_threshold_m <- 1

table_directory <- "outputs/tls/tables"
figure_directory <- "outputs/tls/figures"

dir.create(
  table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

# Helper functions --------------------------------------------------------

standardise_tree_id <- function(x) {
  x <- toupper(trimws(x))

  sub(
    "^([A-Z]+)0+([0-9]+)$",
    "\\1\\2",
    x
  )
}

polygon_area <- function(x, y) {
  if (length(x) < 3) {
    return(NA_real_)
  }

  next_index <- c(
    2:length(x),
    1
  )

  abs(
    sum(
      x * y[next_index] -
        y * x[next_index]
    )
  ) / 2
}

read_las_without_withheld_warning <- function(file_path) {
  withCallingHandlers(
    readLAS(
      file_path,
      select = "xyz"
    ),
    warning = function(warning_condition) {
      warning_text <- conditionMessage(
        warning_condition
      )

      if (
        grepl(
          "withheld",
          warning_text,
          ignore.case = TRUE
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

calculate_pca_diameter <- function(x, y) {
  centre_x <- mean(x)
  centre_y <- mean(y)

  centred_x <- x - centre_x
  centred_y <- y - centre_y

  variance_x <- stats::var(centred_x)
  variance_y <- stats::var(centred_y)
  covariance_xy <- stats::cov(
    centred_x,
    centred_y
  )

  rotation_angle <- 0.5 * atan2(
    2 * covariance_xy,
    variance_x - variance_y
  )

  rotated_axis_1 <-
    centred_x * cos(rotation_angle) +
    centred_y * sin(rotation_angle)

  rotated_axis_2 <-
    -centred_x * sin(rotation_angle) +
    centred_y * cos(rotation_angle)

  rotated_extent_1 <- diff(
    range(
      rotated_axis_1,
      na.rm = TRUE
    )
  )

  rotated_extent_2 <- diff(
    range(
      rotated_axis_2,
      na.rm = TRUE
    )
  )

  mean(
    c(
      rotated_extent_1,
      rotated_extent_2
    )
  )
}

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
        mean_percentage_error = NA_real_,
        mean_absolute_percentage_error =
          NA_real_,
        interpretation = interpretation,
        stringsAsFactors = FALSE
      )
    )
  }

  error <- calculated - reference

  nonzero_reference <-
    is.finite(reference) &
    reference != 0

  percentage_error <- rep(
    NA_real_,
    length(error)
  )

  percentage_error[nonzero_reference] <-
    error[nonzero_reference] /
    reference[nonzero_reference] * 100

  correlation <- if (
    length(calculated) >= 2 &&
    stats::sd(calculated) > 0 &&
    stats::sd(reference) > 0
  ) {
    stats::cor(
      calculated,
      reference
    )
  } else {
    NA_real_
  }

  data.frame(
    metric = metric,
    unit = unit,
    comparisons = length(error),
    mean_error = mean(error),
    mean_absolute_error = mean(abs(error)),
    root_mean_square_error =
      sqrt(mean(error^2)),
    correlation = correlation,
    mean_percentage_error =
      mean(
        percentage_error,
        na.rm = TRUE
      ),
    mean_absolute_percentage_error =
      mean(
        abs(percentage_error),
        na.rm = TRUE
      ),
    interpretation = interpretation,
    stringsAsFactors = FALSE
  )
}

# Locate all TLS files ----------------------------------------------------

tls_files <- list.files(
  "data/raw/tls",
  pattern = "[.]laz$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(tls_files) == 0) {
  stop(
    "No TLS .laz files were found under data/raw/tls."
  )
}

tree_inventory <- data.frame(
  tree_id = toupper(
    sub(
      "_.*$",
      "",
      basename(tls_files)
    )
  ),
  file_name = basename(tls_files),
  file_path = tls_files,
  stringsAsFactors = FALSE
)

tree_inventory$match_id <-
  standardise_tree_id(
    tree_inventory$tree_id
  )

# Read reference measurements --------------------------------------------

if (!file.exists(reference_file)) {
  stop(
    "Reference table was not found: ",
    reference_file
  )
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
  "tree_height",
  "crown_base_height",
  "crown_length_r",
  "crown_dia1",
  "crown_dia2",
  "crown_area"
)

missing_reference_columns <- setdiff(
  required_reference_columns,
  names(reference)
)

if (length(missing_reference_columns) > 0) {
  stop(
    "Missing reference columns: ",
    paste(
      missing_reference_columns,
      collapse = ", "
    )
  )
}

reference$match_id <-
  standardise_tree_id(reference$ID)

reference_rows <- match(
  tree_inventory$match_id,
  reference$match_id
)

tree_inventory$reference_match <-
  !is.na(reference_rows)

cat(
  "TLS files:",
  nrow(tree_inventory),
  "\n"
)

cat(
  "Matched to reference:",
  sum(tree_inventory$reference_match),
  "\n"
)

cat(
  "Unmatched:",
  sum(!tree_inventory$reference_match),
  "\n\n"
)

# Process one tree --------------------------------------------------------

process_tree_projection <- function(
    file_path,
    tree_id,
    file_name,
    reference_row
) {
  if (is.na(reference_row)) {
    stop(
      "No matching reference row was found."
    )
  }

  tls_tree <- read_las_without_withheld_warning(
    file_path
  )

  if (is.empty(tls_tree)) {
    stop("Point cloud is empty.")
  }

  points <- as.data.frame(
    tls_tree@data
  )[
    ,
    c("X", "Y", "Z")
  ]

  finite_coordinates <-
    is.finite(points$X) &
    is.finite(points$Y) &
    is.finite(points$Z)

  points <- points[
    finite_coordinates,
  ]

  if (nrow(points) < 3) {
    stop(
      "Fewer than three finite points were available."
    )
  }

  point_count <- nrow(points)

  calculated_tree_height_m <-
    diff(
      range(
        points$Z,
        na.rm = TRUE
      )
    )

  # Original XY mean extent

  x_extent_m <- diff(
    range(
      points$X,
      na.rm = TRUE
    )
  )

  y_extent_m <- diff(
    range(
      points$Y,
      na.rm = TRUE
    )
  )

  calculated_raw_diameter_1_m <-
    mean(
      c(
        x_extent_m,
        y_extent_m
      )
    )

  # PCA-rotated mean extent

  calculated_pca_diameter_1_m <-
    calculate_pca_diameter(
      points$X,
      points$Y
    )

  # Complete projected convex hull

  hull_indices <- chull(
    points$X,
    points$Y
  )

  hull_points <- points[
    hull_indices,
    c("X", "Y")
  ]

  if (nrow(hull_points) < 3) {
    stop(
      "A valid projected convex hull could not be created."
    )
  }

  calculated_convex_hull_area_m2 <-
    polygon_area(
      hull_points$X,
      hull_points$Y
    )

  hull_distance_matrix <- as.matrix(
    stats::dist(
      hull_points[, c("X", "Y")]
    )
  )

  farthest_distance_by_vertex <- apply(
    hull_distance_matrix,
    1,
    max
  )

  calculated_diameter_2_m <- mean(
    farthest_distance_by_vertex,
    na.rm = TRUE
  )

  maximum_hull_distance_m <- max(
    hull_distance_matrix,
    na.rm = TRUE
  )

  # Reference values

  species <-
    reference$species[reference_row]

  reference_tree_height_m <-
    as.numeric(
      reference$tree_height[reference_row]
    )

  reference_crown_base_height_m <-
    as.numeric(
      reference$crown_base_height[
        reference_row
      ]
    )

  reference_tls_vertical_extent_m <-
    as.numeric(
      reference$crown_length_r[
        reference_row
      ]
    )

  reference_diameter_1_m <-
    as.numeric(
      reference$crown_dia1[
        reference_row
      ]
    )

  reference_diameter_2_m <-
    as.numeric(
      reference$crown_dia2[
        reference_row
      ]
    )

  reference_area_m2 <-
    as.numeric(
      reference$crown_area[
        reference_row
      ]
    )

  data.frame(
    tree_id = tree_id,
    species = species,
    file_name = file_name,
    point_count = point_count,
    convex_hull_vertex_count =
      nrow(hull_points),

    reference_tree_height_m =
      reference_tree_height_m,
    calculated_point_cloud_height_m =
      calculated_tree_height_m,
    tree_height_error_m =
      calculated_tree_height_m -
      reference_tree_height_m,

    reference_crown_base_height_m =
      reference_crown_base_height_m,

    reference_tls_vertical_extent_m =
      reference_tls_vertical_extent_m,
    vertical_extent_error_m =
      calculated_tree_height_m -
      reference_tls_vertical_extent_m,

    reference_crown_diameter_1_m =
      reference_diameter_1_m,
    calculated_raw_crown_diameter_1_m =
      calculated_raw_diameter_1_m,
    raw_crown_diameter_1_error_m =
      calculated_raw_diameter_1_m -
      reference_diameter_1_m,

    calculated_pca_crown_diameter_1_m =
      calculated_pca_diameter_1_m,
    pca_crown_diameter_1_error_m =
      calculated_pca_diameter_1_m -
      reference_diameter_1_m,

    reference_crown_diameter_2_m =
      reference_diameter_2_m,
    calculated_crown_diameter_2_m =
      calculated_diameter_2_m,
    crown_diameter_2_error_m =
      calculated_diameter_2_m -
      reference_diameter_2_m,
    crown_diameter_2_absolute_error_m =
      abs(
        calculated_diameter_2_m -
          reference_diameter_2_m
      ),

    maximum_hull_distance_m =
      maximum_hull_distance_m,

    reference_crown_area_m2 =
      reference_area_m2,
    calculated_projected_convex_hull_area_m2 =
      calculated_convex_hull_area_m2,
    projected_crown_area_error_m2 =
      calculated_convex_hull_area_m2 -
      reference_area_m2,

    processing_status = "success",
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
}

# Batch processing --------------------------------------------------------

projection_results <- vector(
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

  result <- tryCatch(
    process_tree_projection(
      file_path =
        tree_inventory$file_path[i],
      tree_id =
        tree_inventory$tree_id[i],
      file_name =
        tree_inventory$file_name[i],
      reference_row =
        reference_rows[i]
    ),
    error = function(error_condition) {
      reference_row <- reference_rows[i]

      species <- if (
        is.na(reference_row)
      ) {
        NA_character_
      } else {
        reference$species[reference_row]
      }

      data.frame(
        tree_id =
          tree_inventory$tree_id[i],
        species = species,
        file_name =
          tree_inventory$file_name[i],
        point_count = NA_integer_,
        convex_hull_vertex_count =
          NA_integer_,

        reference_tree_height_m =
          NA_real_,
        calculated_point_cloud_height_m =
          NA_real_,
        tree_height_error_m =
          NA_real_,

        reference_crown_base_height_m =
          NA_real_,

        reference_tls_vertical_extent_m =
          NA_real_,
        vertical_extent_error_m =
          NA_real_,

        reference_crown_diameter_1_m =
          NA_real_,
        calculated_raw_crown_diameter_1_m =
          NA_real_,
        raw_crown_diameter_1_error_m =
          NA_real_,

        calculated_pca_crown_diameter_1_m =
          NA_real_,
        pca_crown_diameter_1_error_m =
          NA_real_,

        reference_crown_diameter_2_m =
          NA_real_,
        calculated_crown_diameter_2_m =
          NA_real_,
        crown_diameter_2_error_m =
          NA_real_,
        crown_diameter_2_absolute_error_m =
          NA_real_,

        maximum_hull_distance_m =
          NA_real_,

        reference_crown_area_m2 =
          NA_real_,
        calculated_projected_convex_hull_area_m2 =
          NA_real_,
        projected_crown_area_error_m2 =
          NA_real_,

        processing_status = "failed",
        error_message =
          conditionMessage(error_condition),
        stringsAsFactors = FALSE
      )
    }
  )

  projection_results[[i]] <- result

  rm(result)
  invisible(gc())
}

projection_results <- do.call(
  rbind,
  projection_results
)

row.names(projection_results) <- NULL

# Add transparent comparison flags ---------------------------------------

has_diameter_comparison <-
  projection_results$processing_status ==
  "success" &
  is.finite(
    projection_results$
      calculated_crown_diameter_2_m
  ) &
  is.finite(
    projection_results$
      reference_crown_diameter_2_m
  )

projection_results$comparison_flag <-
  "comparison unavailable"

projection_results$comparison_flag[
  has_diameter_comparison
] <- "below review threshold"

projection_results$comparison_flag[
  has_diameter_comparison &
    projection_results$
    crown_diameter_2_absolute_error_m >=
    diameter_review_threshold_m
] <- "large disagreement: investigate"

# Calculate corrected accuracy -------------------------------------------

successful_results <- projection_results[
  projection_results$processing_status ==
    "success",
]

accuracy_summary <- rbind(
  calculate_accuracy(
    calculated =
      successful_results$
      calculated_point_cloud_height_m,
    reference =
      successful_results$
      reference_tls_vertical_extent_m,
    metric =
      "TLS vertical extent diagnostic",
    unit = "m",
    interpretation = paste(
      "Compares complete point-cloud height",
      "with crown_length_r; this is not",
      "field-derived crown length."
    )
  ),

  calculate_accuracy(
    calculated =
      successful_results$
      calculated_raw_crown_diameter_1_m,
    reference =
      successful_results$
      reference_crown_diameter_1_m,
    metric =
      "Crown diameter 1: complete XY projection",
    unit = "m",
    interpretation = paste(
      "Mean X and Y extent calculated from",
      "the complete isolated-tree projection."
    )
  ),

  calculate_accuracy(
    calculated =
      successful_results$
      calculated_pca_crown_diameter_1_m,
    reference =
      successful_results$
      reference_crown_diameter_1_m,
    metric =
      "Crown diameter 1: complete PCA projection",
    unit = "m",
    interpretation = paste(
      "Orientation-adjusted mean extent from",
      "the complete isolated-tree projection."
    )
  ),

  calculate_accuracy(
    calculated =
      successful_results$
      calculated_crown_diameter_2_m,
    reference =
      successful_results$
      reference_crown_diameter_2_m,
    metric =
      "Crown diameter 2: complete convex hull",
    unit = "m",
    interpretation = paste(
      "Preferred crown-width result;",
      "mean farthest distance among",
      "projected hull vertices."
    )
  ),

  calculate_accuracy(
    calculated =
      successful_results$
      calculated_projected_convex_hull_area_m2,
    reference =
      successful_results$
      reference_crown_area_m2,
    metric =
      "Complete projected convex-hull area diagnostic",
    unit = "m2",
    interpretation = paste(
      "Retained as a separate experimental metric",
      "because the published area definition",
      "has not been reproduced."
    )
  )
)

# Create the inspection queue --------------------------------------------

inspection_queue <- projection_results[
  projection_results$comparison_flag ==
    "large disagreement: investigate",
  c(
    "tree_id",
    "species",
    "reference_crown_base_height_m",
    "reference_crown_diameter_2_m",
    "calculated_crown_diameter_2_m",
    "crown_diameter_2_error_m",
    "crown_diameter_2_absolute_error_m",
    "maximum_hull_distance_m",
    "point_count",
    "convex_hull_vertex_count",
    "comparison_flag"
  )
]

inspection_queue <- inspection_queue[
  order(
    inspection_queue$
      crown_diameter_2_absolute_error_m,
    decreasing = TRUE
  ),
]

row.names(inspection_queue) <- NULL

# Batch summary -----------------------------------------------------------

batch_summary <- data.frame(
  total_trees = nrow(projection_results),
  successful_trees = sum(
    projection_results$processing_status ==
      "success"
  ),
  failed_trees = sum(
    projection_results$processing_status ==
      "failed"
  ),
  diameter_comparisons = sum(
    has_diameter_comparison
  ),
  differences_below_1_m = sum(
    has_diameter_comparison &
      projection_results$
      crown_diameter_2_absolute_error_m < 1
  ),
  differences_at_least_1_m = sum(
    has_diameter_comparison &
      projection_results$
      crown_diameter_2_absolute_error_m >= 1
  ),
  differences_within_0_5_m = sum(
    has_diameter_comparison &
      projection_results$
      crown_diameter_2_absolute_error_m <= 0.5
  ),
  differences_within_1_m = sum(
    has_diameter_comparison &
      projection_results$
      crown_diameter_2_absolute_error_m <= 1
  ),
  stringsAsFactors = FALSE
)

# Save tables -------------------------------------------------------------

results_file <- file.path(
  table_directory,
  "tls_full_crown_projection_validation.csv"
)

accuracy_file <- file.path(
  table_directory,
  "tls_full_crown_projection_accuracy_summary.csv"
)

inspection_file <- file.path(
  table_directory,
  "tls_full_crown_projection_inspection_queue.csv"
)

batch_file <- file.path(
  table_directory,
  "tls_full_crown_projection_batch_summary.csv"
)

write.csv(
  projection_results,
  results_file,
  row.names = FALSE
)

write.csv(
  accuracy_summary,
  accuracy_file,
  row.names = FALSE
)

write.csv(
  inspection_queue,
  inspection_file,
  row.names = FALSE
)

write.csv(
  batch_summary,
  batch_file,
  row.names = FALSE
)

# Create validation figure -----------------------------------------------

padded_range <- function(x, padding_fraction = 0.05) {
  limits <- range(
    x,
    finite = TRUE
  )

  if (!all(is.finite(limits))) {
    return(c(0, 1))
  }

  width <- diff(limits)

  if (width == 0) {
    width <- max(
      abs(limits),
      1
    ) * 0.1
  }

  limits +
    c(-1, 1) *
    width *
    padding_fraction
}

plot_comparison <- function(
    reference_values,
    calculated_values,
    x_label,
    y_label,
    title,
    labels = NULL,
    flagged = NULL
) {
  valid <-
    is.finite(reference_values) &
    is.finite(calculated_values)

  reference_values <-
    reference_values[valid]

  calculated_values <-
    calculated_values[valid]

  if (!is.null(labels)) {
    labels <- labels[valid]
  }

  if (!is.null(flagged)) {
    flagged <- flagged[valid]
  }

  if (length(reference_values) == 0) {
    plot.new()
    title(
      main = paste(
        title,
        "— no valid comparisons"
      )
    )
    return(invisible(NULL))
  }

  limits <- padded_range(
    c(
      reference_values,
      calculated_values
    )
  )

  point_fill <- rep(
    "#347847",
    length(reference_values)
  )

  point_border <- rep(
    "#245632",
    length(reference_values)
  )

  point_size <- rep(
    1,
    length(reference_values)
  )

  if (!is.null(flagged)) {
    point_fill[flagged] <- "#F4A261"
    point_border[flagged] <- "#B22222"
    point_size[flagged] <- 1.35
  }

  plot(
    reference_values,
    calculated_values,
    pch = 21,
    bg = point_fill,
    col = point_border,
    cex = point_size,
    lwd = 1.4,
    xlim = limits,
    ylim = limits,
    xlab = x_label,
    ylab = y_label,
    main = title
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
      reference_values[flagged],
      calculated_values[flagged],
      labels = labels[flagged],
      pos = 3,
      cex = 0.65,
      col = "#A51C30"
    )
  }

  invisible(NULL)
}

figure_file <- file.path(
  figure_directory,
  "tls_full_crown_projection_validation.png"
)

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
  reference_values =
    projection_results$
    reference_tls_vertical_extent_m,
  calculated_values =
    projection_results$
    calculated_point_cloud_height_m,
  x_label =
    "Reference crown_length_r variable (m)",
  y_label =
    "Calculated point-cloud height (m)",
  title =
    "TLS vertical-extent diagnostic"
)

plot_comparison(
  reference_values =
    projection_results$
    reference_crown_diameter_1_m,
  calculated_values =
    projection_results$
    calculated_raw_crown_diameter_1_m,
  x_label =
    "Reference crown diameter 1 (m)",
  y_label =
    "Complete XY mean extent (m)",
  title =
    "Crown diameter 1"
)

diameter_flagged <-
  projection_results$comparison_flag ==
  "large disagreement: investigate"

plot_comparison(
  reference_values =
    projection_results$
    reference_crown_diameter_2_m,
  calculated_values =
    projection_results$
    calculated_crown_diameter_2_m,
  x_label =
    "Reference crown diameter 2 (m)",
  y_label =
    "Complete hull diameter (m)",
  title =
    "Preferred crown diameter",
  labels = projection_results$tree_id,
  flagged = diameter_flagged
)

plot_comparison(
  reference_values =
    projection_results$
    reference_crown_area_m2,
  calculated_values =
    projection_results$
    calculated_projected_convex_hull_area_m2,
  x_label =
    "Published crown-area variable (m²)",
  y_label =
    "Complete projected convex-hull area (m²)",
  title =
    "Crown-area method comparison"
)

dev.off()

# Print results -----------------------------------------------------------

cat(
  "\nFull-projection batch summary:\n"
)

print(
  batch_summary,
  row.names = FALSE
)

cat(
  "\nFull-projection accuracy summary:\n"
)

print(
  accuracy_summary,
  row.names = FALSE
)

cat(
  "\nCrown-diameter comparisons requiring investigation:",
  nrow(inspection_queue),
  "\n"
)

if (nrow(inspection_queue) > 0) {
  print(
    inspection_queue,
    row.names = FALSE
  )
} else {
  cat(
    "None of the diameter-2 differences reached ",
    diameter_review_threshold_m,
    " m.\n",
    sep = ""
  )
}

wl20_result <- projection_results[
  projection_results$tree_id == "WL20",
  c(
    "tree_id",
    "reference_crown_diameter_2_m",
    "calculated_crown_diameter_2_m",
    "crown_diameter_2_error_m",
    "comparison_flag"
  )
]

cat(
  "\nWL20 result using the complete projection:\n"
)

print(
  wl20_result,
  row.names = FALSE
)

cat(
  "\nOutputs saved:\n",
  results_file,
  "\n",
  accuracy_file,
  "\n",
  inspection_file,
  "\n",
  batch_file,
  "\n",
  figure_file,
  "\n",
  sep = ""
)