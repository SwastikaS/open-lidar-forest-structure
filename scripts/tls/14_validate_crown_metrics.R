# Validate TLS crown metrics across all available individual trees

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

# Settings ---------------------------------------------------------------

reference_file <- paste0(
  "data/reference/tls/",
  "Tree Parameters TLS AD WL.csv"
)

output_table_directory <-
  "outputs/tls/tables"

output_figure_directory <-
  "outputs/tls/figures"

# Helper functions -------------------------------------------------------

standardise_tree_id <- function(x) {

  x <- toupper(
    trimws(x)
  )

  sub(
    "^([A-Z]+)0+([0-9]+)$",
    "\\1\\2",
    x
  )
}

calculate_polygon_area <- function(
    x,
    y
) {

  closed_x <- c(
    x,
    x[1]
  )

  closed_y <- c(
    y,
    y[1]
  )

  0.5 * abs(
    sum(
      closed_x[-length(closed_x)] *
        closed_y[-1] -
        closed_x[-1] *
        closed_y[-length(closed_y)]
    )
  )
}

calculate_accuracy <- function(
    estimated,
    reference,
    metric,
    unit
) {

  valid <- is.finite(estimated) &
    is.finite(reference)

  estimated <- estimated[valid]
  reference <- reference[valid]

  if (length(estimated) == 0) {
    return(
      data.frame(
        metric = metric,
        unit = unit,
        comparisons = 0,
        mean_error = NA_real_,
        mean_absolute_error = NA_real_,
        root_mean_square_error = NA_real_,
        mean_percentage_error = NA_real_,
        mean_absolute_percentage_error = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  error <- estimated - reference

  percentage_error <- rep(
    NA_real_,
    length(error)
  )

  nonzero_reference <-
    is.finite(reference) &
    reference != 0

  percentage_error[nonzero_reference] <-
    error[nonzero_reference] /
    reference[nonzero_reference] *
    100

  data.frame(
    metric = metric,
    unit = unit,
    comparisons = length(error),
    mean_error =
      mean(error),
    mean_absolute_error =
      mean(abs(error)),
    root_mean_square_error =
      sqrt(mean(error^2)),
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
    stringsAsFactors = FALSE
  )
}

plot_validation_panel <- function(
    reference,
    estimated,
    title,
    x_label,
    y_label,
    point_colour
) {

  valid <- is.finite(reference) &
    is.finite(estimated)

  reference <- reference[valid]
  estimated <- estimated[valid]

  if (length(reference) == 0) {
    plot.new()
    title(
      main = paste(
        title,
        "— no valid comparisons"
      )
    )
    return(
      invisible(NULL)
    )
  }

  plot_range <- range(
    c(
      reference,
      estimated
    ),
    na.rm = TRUE
  )

  padding <- diff(plot_range) * 0.05

  if (
    !is.finite(padding) ||
    padding == 0
  ) {
    padding <- 1
  }

  plot_range <- plot_range +
    c(
      -padding,
      padding
    )

  plot(
    reference,
    estimated,
    pch = 16,
    col = point_colour,
    xlim = plot_range,
    ylim = plot_range,
    xlab = x_label,
    ylab = y_label,
    main = title
  )

  abline(
    a = 0,
    b = 1,
    lty = 2,
    lwd = 2,
    col = "grey35"
  )
}

# Locate TLS files -------------------------------------------------------

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

# Read reference table ---------------------------------------------------

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
  standardise_tree_id(
    reference$ID
  )

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

# Process every tree -----------------------------------------------------

crown_results <- vector(
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

  result <- tryCatch(
    {

      if (is.na(reference_row)) {
        stop(
          "No matching reference record was found."
        )
      }

      reference_tree_height_m <- as.numeric(
        reference$tree_height[
          reference_row
        ]
      )

      reference_crown_base_height_m <- as.numeric(
        reference$crown_base_height[
          reference_row
        ]
      )

      reference_crown_length_m <- as.numeric(
        reference$crown_length_r[
          reference_row
        ]
      )

      reference_crown_diameter_1_m <- as.numeric(
        reference$crown_dia1[
          reference_row
        ]
      )

      reference_crown_diameter_2_m <- as.numeric(
        reference$crown_dia2[
          reference_row
        ]
      )

      reference_crown_area_m2 <- as.numeric(
        reference$crown_area[
          reference_row
        ]
      )

      if (
        !is.finite(
          reference_crown_base_height_m
        )
      ) {
        stop(
          "Reference crown-base height is missing."
        )
      }

      tls_tree <- readLAS(
        tree_inventory$file_path[i],
        select = "xyz"
      )

      if (is.empty(tls_tree)) {
        stop(
          "Point cloud is empty."
        )
      }

      points <- as.data.frame(
        tls_tree@data
      )[
        ,
        c(
          "X",
          "Y",
          "Z"
        )
      ]

      points <- points[
        is.finite(points$X) &
          is.finite(points$Y) &
          is.finite(points$Z),
        ,
        drop = FALSE
      ]

      if (nrow(points) < 3) {
        stop(
          "Fewer than three valid points were found."
        )
      }

      tree_base_z <- min(
        points$Z,
        na.rm = TRUE
      )

      points$height_m <-
        points$Z - tree_base_z

      calculated_tree_height_m <- max(
        points$height_m,
        na.rm = TRUE
      )

      crown_points <- points[
        points$height_m >=
          reference_crown_base_height_m,
        ,
        drop = FALSE
      ]

      if (nrow(crown_points) < 3) {
        stop(
          "Fewer than three crown points were found."
        )
      }

      calculated_crown_length_m <-
        calculated_tree_height_m -
        reference_crown_base_height_m

      # Crown diameter method 1:
      # mean extent in the original X and Y axes.

      raw_x_extent_m <- diff(
        range(
          crown_points$X,
          na.rm = TRUE
        )
      )

      raw_y_extent_m <- diff(
        range(
          crown_points$Y,
          na.rm = TRUE
        )
      )

      raw_mean_crown_width_m <- mean(
        c(
          raw_x_extent_m,
          raw_y_extent_m
        )
      )

      # PCA-rotated crown diameter method 1.

      xy_coordinates <- cbind(
        crown_points$X,
        crown_points$Y
      )

      xy_centre <- colMeans(
        xy_coordinates
      )

      centred_xy <- sweep(
        xy_coordinates,
        2,
        xy_centre,
        FUN = "-"
      )

      xy_covariance <- cov(
        centred_xy
      )

      pca_axes <- eigen(
        xy_covariance
      )$vectors

      rotated_xy <-
        centred_xy %*% pca_axes

      pca_x_extent_m <- diff(
        range(
          rotated_xy[, 1],
          na.rm = TRUE
        )
      )

      pca_y_extent_m <- diff(
        range(
          rotated_xy[, 2],
          na.rm = TRUE
        )
      )

      pca_mean_crown_width_m <- mean(
        c(
          pca_x_extent_m,
          pca_y_extent_m
        )
      )

      # Convex-hull diameter and area.

      unique_xy <- unique(
        crown_points[
          ,
          c(
            "X",
            "Y"
          )
        ]
      )

      if (nrow(unique_xy) < 3) {
        stop(
          "Fewer than three unique XY coordinates were found."
        )
      }

      hull_indices <- chull(
        unique_xy$X,
        unique_xy$Y
      )

      crown_hull <- unique_xy[
        hull_indices,
        ,
        drop = FALSE
      ]

      convex_hull_area_m2 <-
        calculate_polygon_area(
          crown_hull$X,
          crown_hull$Y
        )

      hull_distance_matrix <- as.matrix(
        dist(
          crown_hull[
            ,
            c(
              "X",
              "Y"
            )
          ]
        )
      )

      furthest_distances_m <- apply(
        hull_distance_matrix,
        1,
        max
      )

      convex_hull_diameter_m <- mean(
        furthest_distances_m
      )

      data.frame(
        tree_id =
          tree_inventory$tree_id[i],

        species =
          reference$species[
            reference_row
          ],

        file_name =
          tree_inventory$file_name[i],

        point_count =
          nrow(points),

        crown_point_count =
          nrow(crown_points),

        convex_hull_point_count =
          nrow(crown_hull),

        reference_tree_height_m =
          reference_tree_height_m,

        calculated_tree_height_m =
          calculated_tree_height_m,

        tree_height_error_m =
          calculated_tree_height_m -
          reference_tree_height_m,

        reference_crown_base_height_m =
          reference_crown_base_height_m,

        reference_crown_length_m =
          reference_crown_length_m,

        calculated_crown_length_m =
          calculated_crown_length_m,

        crown_length_error_m =
          calculated_crown_length_m -
          reference_crown_length_m,

        reference_crown_diameter_1_m =
          reference_crown_diameter_1_m,

        calculated_raw_crown_diameter_1_m =
          raw_mean_crown_width_m,

        raw_crown_diameter_1_error_m =
          raw_mean_crown_width_m -
          reference_crown_diameter_1_m,

        calculated_pca_crown_diameter_1_m =
          pca_mean_crown_width_m,

        pca_crown_diameter_1_error_m =
          pca_mean_crown_width_m -
          reference_crown_diameter_1_m,

        reference_crown_diameter_2_m =
          reference_crown_diameter_2_m,

        calculated_crown_diameter_2_m =
          convex_hull_diameter_m,

        crown_diameter_2_error_m =
          convex_hull_diameter_m -
          reference_crown_diameter_2_m,

        reference_crown_area_m2 =
          reference_crown_area_m2,

        calculated_convex_hull_area_m2 =
          convex_hull_area_m2,

        crown_area_error_m2 =
          convex_hull_area_m2 -
          reference_crown_area_m2,

        processing_status =
          "success",

        error_message =
          NA_character_,

        stringsAsFactors = FALSE
      )
    },
    error = function(e) {

      data.frame(
        tree_id =
          tree_inventory$tree_id[i],

        species = if (
          !is.na(reference_row)
        ) {
          reference$species[
            reference_row
          ]
        } else {
          NA_character_
        },

        file_name =
          tree_inventory$file_name[i],

        point_count =
          NA_integer_,

        crown_point_count =
          NA_integer_,

        convex_hull_point_count =
          NA_integer_,

        reference_tree_height_m =
          NA_real_,

        calculated_tree_height_m =
          NA_real_,

        tree_height_error_m =
          NA_real_,

        reference_crown_base_height_m =
          NA_real_,

        reference_crown_length_m =
          NA_real_,

        calculated_crown_length_m =
          NA_real_,

        crown_length_error_m =
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

        reference_crown_area_m2 =
          NA_real_,

        calculated_convex_hull_area_m2 =
          NA_real_,

        crown_area_error_m2 =
          NA_real_,

        processing_status =
          "failed",

        error_message =
          conditionMessage(e),

        stringsAsFactors = FALSE
      )
    }
  )

  crown_results[[i]] <- result

  rm(result)

  if (exists("tls_tree")) {
    rm(tls_tree)
  }

  if (exists("points")) {
    rm(points)
  }

  if (exists("crown_points")) {
    rm(crown_points)
  }

  if (exists("centred_xy")) {
    rm(centred_xy)
  }

  if (exists("rotated_xy")) {
    rm(rotated_xy)
  }

  invisible(gc())
}

crown_results <- do.call(
  rbind,
  crown_results
)

row.names(crown_results) <- NULL

# Calculate accuracy summaries ------------------------------------------

accuracy_summary <- rbind(
  calculate_accuracy(
    estimated =
      crown_results$calculated_crown_length_m,
    reference =
      crown_results$reference_crown_length_m,
    metric =
      "Crown length",
    unit =
      "m"
  ),

  calculate_accuracy(
    estimated =
      crown_results$calculated_raw_crown_diameter_1_m,
    reference =
      crown_results$reference_crown_diameter_1_m,
    metric =
      "Crown diameter 1: original XY extent",
    unit =
      "m"
  ),

  calculate_accuracy(
    estimated =
      crown_results$calculated_pca_crown_diameter_1_m,
    reference =
      crown_results$reference_crown_diameter_1_m,
    metric =
      "Crown diameter 1: PCA-rotated extent",
    unit =
      "m"
  ),

  calculate_accuracy(
    estimated =
      crown_results$calculated_crown_diameter_2_m,
    reference =
      crown_results$reference_crown_diameter_2_m,
    metric =
      "Crown diameter 2: convex-hull method",
    unit =
      "m"
  ),

  calculate_accuracy(
    estimated =
      crown_results$calculated_convex_hull_area_m2,
    reference =
      crown_results$reference_crown_area_m2,
    metric =
      "Projected 2D convex-hull area",
    unit =
      "m2"
  )
)

row.names(accuracy_summary) <- NULL

batch_summary <- data.frame(
  total_trees =
    nrow(crown_results),

  successful_trees =
    sum(
      crown_results$processing_status ==
        "success"
    ),

  failed_trees =
    sum(
      crown_results$processing_status ==
        "failed"
    ),

  stringsAsFactors = FALSE
)

# Save tables ------------------------------------------------------------

dir.create(
  output_table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

results_output_file <- file.path(
  output_table_directory,
  "tls_crown_metrics_validation.csv"
)

accuracy_output_file <- file.path(
  output_table_directory,
  "tls_crown_accuracy_summary.csv"
)

batch_output_file <- file.path(
  output_table_directory,
  "tls_crown_batch_summary.csv"
)

write.csv(
  crown_results,
  results_output_file,
  row.names = FALSE
)

write.csv(
  accuracy_summary,
  accuracy_output_file,
  row.names = FALSE
)

write.csv(
  batch_summary,
  batch_output_file,
  row.names = FALSE
)

# Create validation figure ----------------------------------------------

dir.create(
  output_figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_output_file <- file.path(
  output_figure_directory,
  "tls_crown_metrics_validation.png"
)

png(
  figure_output_file,
  width = 2200,
  height = 1800,
  res = 220
)

par(
  mfrow = c(2, 2),
  mar = c(4.8, 4.8, 3.5, 1)
)

plot_validation_panel(
  reference =
    crown_results$reference_crown_length_m,

  estimated =
    crown_results$calculated_crown_length_m,

  title =
    "Crown length",

  x_label =
    "Reference crown length (m)",

  y_label =
    "Calculated crown length (m)",

  point_colour =
    "#355C7D"
)

plot_validation_panel(
  reference =
    crown_results$reference_crown_diameter_1_m,

  estimated =
    crown_results$calculated_pca_crown_diameter_1_m,

  title =
    "PCA-rotated crown diameter 1",

  x_label =
    "Reference crown diameter (m)",

  y_label =
    "Calculated crown diameter (m)",

  point_colour =
    "#4C78A8"
)

plot_validation_panel(
  reference =
    crown_results$reference_crown_diameter_2_m,

  estimated =
    crown_results$calculated_crown_diameter_2_m,

  title =
    "Convex-hull crown diameter 2",

  x_label =
    "Reference crown diameter (m)",

  y_label =
    "Calculated crown diameter (m)",

  point_colour =
    "#347847"
)

plot_validation_panel(
  reference =
    crown_results$reference_crown_area_m2,

  estimated =
    crown_results$calculated_convex_hull_area_m2,

  title =
    "Projected convex-hull area",

  x_label =
    "Reference crown area (m²)",

  y_label =
    "Calculated convex-hull area (m²)",

  point_colour =
    "#D55E00"
)

dev.off()

# Print results ----------------------------------------------------------

cat("\nBatch-processing summary:\n")
print(
  batch_summary,
  row.names = FALSE
)

cat("\nCrown-metric accuracy summary:\n")
print(
  accuracy_summary,
  row.names = FALSE
)

failed_results <- crown_results[
  crown_results$processing_status ==
    "failed",
  c(
    "tree_id",
    "species",
    "error_message"
  ),
  drop = FALSE
]

if (nrow(failed_results) > 0) {
  cat("\nFailed trees:\n")
  print(
    failed_results,
    row.names = FALSE
  )
}

cat(
  "\nResults saved to:",
  results_output_file,
  "\n"
)

cat(
  "Accuracy summary saved to:",
  accuracy_output_file,
  "\n"
)

cat(
  "Batch summary saved to:",
  batch_output_file,
  "\n"
)

cat(
  "Validation figure saved to:",
  figure_output_file,
  "\n"
)