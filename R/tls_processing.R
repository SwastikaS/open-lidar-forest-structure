# Reusable functions for processing individual TLS tree point clouds

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)
library(MASS)


# Circle fitting ----------------------------------------------------------

fit_circle_rlm <- function(stem_points) {

  if (nrow(stem_points) < 30) {
    stop("Insufficient points for circle fitting.")
  }

  origin_x <- median(stem_points$X)
  origin_y <- median(stem_points$Y)

  stem_points$x_local <- stem_points$X - origin_x
  stem_points$y_local <- stem_points$Y - origin_y

  model <- MASS::rlm(
    I(-(x_local^2 + y_local^2)) ~ x_local + y_local,
    data = stem_points,
    maxit = 200
  )

  coefficients <- coef(model)

  centre_x_local <-
    -coefficients["x_local"] / 2

  centre_y_local <-
    -coefficients["y_local"] / 2

  radius_squared <-
    centre_x_local^2 +
    centre_y_local^2 -
    coefficients["(Intercept)"]

  if (
    !is.finite(radius_squared) ||
    radius_squared <= 0
  ) {
    stop("The fitted circle has an invalid radius.")
  }

  radius_m <- sqrt(radius_squared)

  centre_x <- origin_x + centre_x_local
  centre_y <- origin_y + centre_y_local

  radial_distance <- sqrt(
    (stem_points$X - centre_x)^2 +
      (stem_points$Y - centre_y)^2
  )

  radial_residual <-
    radial_distance - radius_m

  angles <- atan2(
    stem_points$Y - centre_y,
    stem_points$X - centre_x
  )

  angle_bins <- cut(
    angles,
    breaks = seq(-pi, pi, length.out = 37),
    include.lowest = TRUE
  )

  circumference_completeness <-
    length(unique(angle_bins[!is.na(angle_bins)])) /
    36 * 100

  data.frame(
    centre_x = unname(centre_x),
    centre_y = unname(centre_y),
    radius_m = unname(radius_m),
    estimated_dbh_cm = unname(radius_m * 200),
    circle_rmse_mm =
      sqrt(mean(radial_residual^2)) * 1000,
    circumference_completeness_percent =
      circumference_completeness,
    retained_points = nrow(stem_points)
  )
}

# Direct DBH method -------------------------------------------------------

estimate_dbh_direct <- function(points) {

  dbh_slice <- points[
    points$height_above_base_m >= 1.25 &
      points$height_above_base_m <= 1.35,
  ]

  if (nrow(dbh_slice) < 30) {
    stop("Insufficient points in the DBH slice.")
  }

  initial_x <- median(dbh_slice$X)
  initial_y <- median(dbh_slice$Y)

  distance <- sqrt(
    (dbh_slice$X - initial_x)^2 +
      (dbh_slice$Y - initial_y)^2
  )

  distance_mad <- mad(distance)

  if (
    !is.finite(distance_mad) ||
    distance_mad == 0
  ) {
    distance_mad <- 0.01
  }

  cutoff <-
    median(distance) + 6 * distance_mad

  clean_slice <- dbh_slice[
    distance <= cutoff,
  ]

  fit <- fit_circle_rlm(clean_slice)

  fit$original_slice_points <- nrow(dbh_slice)
  fit$removed_points <-
    nrow(dbh_slice) - nrow(clean_slice)
  fit$accepted_tracking_heights <- NA_integer_

  fit
}

# Base-to-DBH tracking method --------------------------------------------

estimate_dbh_tracked <- function(points) {

  starting_slice <- points[
    points$height_above_base_m >= 0.25 &
      points$height_above_base_m <= 0.35,
  ]

  if (nrow(starting_slice) < 30) {
    stop("Insufficient points at the 0.3 m starting height.")
  }

  current_fit <- fit_circle_rlm(starting_slice)
  current_fit$height_m <- 0.3

  if (
    current_fit$radius_m < 0.025 ||
    current_fit$radius_m > 0.75
  ) {
    stop("Implausible starting stem radius.")
  }

  tracking_results <- current_fit

  for (height in seq(0.4, 1.3, by = 0.1)) {

    height_slice <- points[
      points$height_above_base_m >= height - 0.05 &
        points$height_above_base_m <= height + 0.05,
    ]

    if (nrow(height_slice) < 30) {
      next
    }

    distance_from_previous_centre <- sqrt(
      (height_slice$X - current_fit$centre_x)^2 +
        (height_slice$Y - current_fit$centre_y)^2
    )

    stem_candidates <- height_slice[
      abs(
        distance_from_previous_centre -
          current_fit$radius_m
      ) <= 0.08,
    ]

    if (nrow(stem_candidates) < 30) {
      next
    }

    new_fit <- tryCatch(
      fit_circle_rlm(stem_candidates),
      error = function(e) NULL
    )

    if (is.null(new_fit)) {
      next
    }

    centre_shift <- sqrt(
      (new_fit$centre_x - current_fit$centre_x)^2 +
        (new_fit$centre_y - current_fit$centre_y)^2
    )

    radius_change <- abs(
      new_fit$radius_m - current_fit$radius_m
    )

    if (
      centre_shift > 0.12 ||
      radius_change > 0.05 ||
      new_fit$radius_m < 0.025 ||
      new_fit$radius_m > 0.75
    ) {
      next
    }

    new_fit$height_m <- height

    tracking_results <- rbind(
      tracking_results,
      new_fit
    )

    current_fit <- new_fit
  }

  dbh_row <- which(
    abs(tracking_results$height_m - 1.3) < 0.001
  )

  if (length(dbh_row) == 0) {
    stop("Stem tracking did not reach 1.3 m.")
  }

  final_fit <- tracking_results[dbh_row[1], ]

  complete_dbh_slice <- points[
    points$height_above_base_m >= 1.25 &
      points$height_above_base_m <= 1.35,
  ]

  final_fit$original_slice_points <-
    nrow(complete_dbh_slice)

  final_fit$removed_points <-
    nrow(complete_dbh_slice) -
    final_fit$retained_points

  final_fit$accepted_tracking_heights <-
    nrow(tracking_results)

  final_fit
}

# Hybrid method -----------------------------------------------------------

estimate_tree_dbh <- function(file_path) {

  tls_tree <- readLAS(
    file_path,
    select = "xyz"
  )

  if (is.empty(tls_tree)) {
    stop("Point cloud is empty.")
  }

  points <- as.data.frame(tls_tree@data)[
    ,
    c("X", "Y", "Z")
  ]

  tree_base_z <- min(points$Z, na.rm = TRUE)

  points$height_above_base_m <-
    points$Z - tree_base_z

  tracked_fit <- tryCatch(
    estimate_dbh_tracked(points),
    error = function(e) NULL
  )

  if (!is.null(tracked_fit)) {
    tracked_fit$method_used <-
      "base-to-DBH tracking"
    tracked_fit$tree_base_z <- tree_base_z

    return(tracked_fit)
  }

  direct_fit <- estimate_dbh_direct(points)

  relative_rmse <-
    direct_fit$circle_rmse_mm /
    (direct_fit$radius_m * 1000)

  direct_fit_is_reliable <-
    direct_fit$radius_m >= 0.025 &&
    direct_fit$radius_m <= 0.75 &&
    relative_rmse <= 0.10 &&
    direct_fit$circumference_completeness_percent >= 70 &&
    direct_fit$retained_points >= 100

  if (!direct_fit_is_reliable) {
    stop(
      paste(
        "Stem tracking failed and the direct fit",
        "did not pass quality control."
      )
    )
  }

  direct_fit$method_used <-
    "direct-fit fallback"

  direct_fit$tree_base_z <- tree_base_z

  direct_fit
}

# Complete processing function --------------------------------------------

process_tls_tree <- function(file_path) {

  file_name <- basename(file_path)

  tryCatch(
    {
      tls_tree <- lidR::readLAS(
        file_path,
        select = "xyz"
      )

      if (lidR::is.empty(tls_tree)) {
        stop("The point cloud is empty.")
      }

      points <- as.data.frame(tls_tree@data)[
        ,
        c("X", "Y", "Z")
      ]

      if (
        nrow(points) == 0 ||
        any(!is.finite(range(points$Z)))
      ) {
        stop("The point cloud does not contain valid XYZ coordinates.")
      }

      tree_base_z <- min(points$Z)
      tree_top_z <- max(points$Z)
      calculated_height_m <- tree_top_z - tree_base_z

      dbh_fit <- estimate_tree_dbh(file_path)

      basal_area_m2 <-
        pi * dbh_fit$radius_m^2

      quality_flag <- ifelse(
        dbh_fit$circle_rmse_mm <= 20 &&
          dbh_fit$circumference_completeness_percent >= 70 &&
          dbh_fit$retained_points >= 100,
        "acceptable",
        "inspect"
      )

      data.frame(
        file_name = file_name,
        point_count = nrow(points),
        calculated_height_m =
          calculated_height_m,
        estimated_dbh_cm =
          dbh_fit$estimated_dbh_cm,
        basal_area_m2 =
          basal_area_m2,
        method_used =
          dbh_fit$method_used,
        circle_rmse_mm =
          dbh_fit$circle_rmse_mm,
        circumference_completeness_percent =
          dbh_fit$circumference_completeness_percent,
        retained_stem_points =
          dbh_fit$retained_points,
        quality_flag =
          quality_flag,
        processing_status =
          "success",
        error_message =
          NA_character_,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      data.frame(
        file_name = file_name,
        point_count = NA_integer_,
        calculated_height_m = NA_real_,
        estimated_dbh_cm = NA_real_,
        basal_area_m2 = NA_real_,
        method_used = NA_character_,
        circle_rmse_mm = NA_real_,
        circumference_completeness_percent =
          NA_real_,
        retained_stem_points = NA_integer_,
        quality_flag = "failed",
        processing_status = "failed",
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )
}

# Prepare measurements and visualisation data -----------------------------

process_tls_tree_visual <- function(
    file_path,
    maximum_display_points = 50000
) {

  file_name <- basename(file_path)

  tryCatch(
    {
      tls_tree <- lidR::readLAS(
        file_path,
        select = "xyz"
      )

      if (lidR::is.empty(tls_tree)) {
        stop("The point cloud is empty.")
      }

      points <- as.data.frame(tls_tree@data)[
        ,
        c("X", "Y", "Z")
      ]

      if (
        nrow(points) == 0 ||
        any(!is.finite(range(points$Z)))
      ) {
        stop(
          "The point cloud does not contain valid XYZ coordinates."
        )
      }

      tree_base_z <- min(
        points$Z,
        na.rm = TRUE
      )

      tree_top_z <- max(
        points$Z,
        na.rm = TRUE
      )

      points$height_above_base_m <-
        points$Z - tree_base_z

      calculated_height_m <-
        tree_top_z - tree_base_z

      dbh_fit <- estimate_tree_dbh(
        file_path
      )

      basal_area_m2 <-
        pi * dbh_fit$radius_m^2

      quality_flag <- ifelse(
        dbh_fit$circle_rmse_mm <= 20 &&
          dbh_fit$circumference_completeness_percent >= 70 &&
          dbh_fit$retained_points >= 100,
        "acceptable",
        "inspect"
      )

      summary_result <- data.frame(
        file_name = file_name,
        point_count = nrow(points),
        calculated_height_m =
          calculated_height_m,
        estimated_dbh_cm =
          dbh_fit$estimated_dbh_cm,
        basal_area_m2 =
          basal_area_m2,
        method_used =
          dbh_fit$method_used,
        circle_rmse_mm =
          dbh_fit$circle_rmse_mm,
        circumference_completeness_percent =
          dbh_fit$circumference_completeness_percent,
        retained_stem_points =
          dbh_fit$retained_points,
        quality_flag =
          quality_flag,
        processing_status =
          "success",
        error_message =
          NA_character_,
        stringsAsFactors = FALSE
      )

      if (nrow(points) > maximum_display_points) {

        set.seed(42)

        display_rows <- sample(
          seq_len(nrow(points)),
          maximum_display_points
        )

        display_points <- points[
          display_rows,
        ]

      } else {

        display_points <- points
      }

      dbh_slice <- points[
        points$height_above_base_m >= 1.25 &
          points$height_above_base_m <= 1.35,
      ]

      distance_from_circle <- sqrt(
        (dbh_slice$X - dbh_fit$centre_x)^2 +
          (dbh_slice$Y - dbh_fit$centre_y)^2
      )

      display_radius <-
        dbh_fit$radius_m + 0.30

      dbh_display_points <- dbh_slice[
        distance_from_circle <= display_radius,
      ]

      circle_angle <- seq(
        0,
        2 * pi,
        length.out = 500
      )

      fitted_circle <- data.frame(
        X =
          dbh_fit$centre_x +
          dbh_fit$radius_m *
          cos(circle_angle),

        Y =
          dbh_fit$centre_y +
          dbh_fit$radius_m *
          sin(circle_angle)
      )

      list(
        summary = summary_result,
        display_points = display_points,
        dbh_display_points = dbh_display_points,
        fitted_circle = fitted_circle,
        circle_centre = data.frame(
          X = dbh_fit$centre_x,
          Y = dbh_fit$centre_y
        ),
        success = TRUE
      )
    },

    error = function(e) {

      failed_summary <- data.frame(
        file_name = file_name,
        point_count = NA_integer_,
        calculated_height_m = NA_real_,
        estimated_dbh_cm = NA_real_,
        basal_area_m2 = NA_real_,
        method_used = NA_character_,
        circle_rmse_mm = NA_real_,
        circumference_completeness_percent =
          NA_real_,
        retained_stem_points = NA_integer_,
        quality_flag = "failed",
        processing_status = "failed",
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )

      list(
        summary = failed_summary,
        display_points = NULL,
        dbh_display_points = NULL,
        fitted_circle = NULL,
        circle_centre = NULL,
        success = FALSE
      )
    }
  )
}

# Batch processing ---------------------------------------------------------

process_tls_batch <- function(file_paths, display_names = basename(file_paths),
                              progress_callback = NULL) {

  if (length(file_paths) == 0) {
    stop("No TLS files were supplied.")
  }

  if (length(display_names) != length(file_paths)) {
    stop("File paths and display names must have the same length.")
  }

  results <- vector("list", length(file_paths))

  for (i in seq_along(file_paths)) {
    if (is.function(progress_callback)) {
      progress_callback(i, length(file_paths), display_names[i])
    }

    result <- process_tls_tree(file_paths[i])
    result$file_name <- display_names[i]
    results[[i]] <- result
  }

  batch_results <- do.call(rbind, results)
  row.names(batch_results) <- NULL
  batch_results
}

# Lower-stem taper ---------------------------------------------------------

estimate_stem_taper <- function(
    file_path,
    measurement_heights = c(1.3, 2, 4, 6),
    tracking_step_m = 0.1,
    slice_half_width_m = 0.05,
    radial_tolerance_m = 0.08
) {

  measurement_heights <- sort(unique(measurement_heights))

  if (
    length(measurement_heights) == 0 ||
    any(!is.finite(measurement_heights)) ||
    any(measurement_heights < 1.3)
  ) {
    stop("Measurement heights must be finite and at least 1.3 m.")
  }

  tls_tree <- lidR::readLAS(
    file_path,
    select = "xyz"
  )

  if (lidR::is.empty(tls_tree)) {
    stop("The point cloud is empty.")
  }

  points <- as.data.frame(tls_tree@data)[
    ,
    c("X", "Y", "Z")
  ]

  tree_base_z <- min(points$Z, na.rm = TRUE)
  points$height_above_base_m <- points$Z - tree_base_z

  dbh_fit <- estimate_tree_dbh(file_path)
  dbh_fit$height_m <- 1.3
  dbh_fit$tracking_status <- "accepted"

  tracking_columns <- c(
    "centre_x",
    "centre_y",
    "radius_m",
    "estimated_dbh_cm",
    "circle_rmse_mm",
    "circumference_completeness_percent",
    "retained_points",
    "height_m",
    "tracking_status"
  )

  tracking_results <- dbh_fit[, tracking_columns]
  current_fit <- tracking_results

  maximum_height <- max(measurement_heights)
  tracking_heights <- seq(
    1.3 + tracking_step_m,
    maximum_height,
    by = tracking_step_m
  )

  for (height in tracking_heights) {

    height_slice <- points[
      points$height_above_base_m >= height - slice_half_width_m &
        points$height_above_base_m <= height + slice_half_width_m,
    ]

    if (nrow(height_slice) < 30) {
      next
    }

    distance_from_previous_centre <- sqrt(
      (height_slice$X - current_fit$centre_x)^2 +
        (height_slice$Y - current_fit$centre_y)^2
    )

    stem_candidates <- height_slice[
      abs(
        distance_from_previous_centre - current_fit$radius_m
      ) <= radial_tolerance_m,
    ]

    if (nrow(stem_candidates) < 30) {
      next
    }

    new_fit <- tryCatch(
      fit_circle_rlm(stem_candidates),
      error = function(e) NULL
    )

    if (is.null(new_fit)) {
      next
    }

    centre_shift <- sqrt(
      (new_fit$centre_x - current_fit$centre_x)^2 +
        (new_fit$centre_y - current_fit$centre_y)^2
    )

    radius_change <- abs(
      new_fit$radius_m - current_fit$radius_m
    )

    plausible_fit <-
      centre_shift <= 0.12 &&
      radius_change <= 0.05 &&
      new_fit$radius_m >= 0.025 &&
      new_fit$radius_m <= 0.75

    if (!plausible_fit) {
      next
    }

    new_fit$height_m <- round(height, 2)
    new_fit$tracking_status <- "accepted"
    new_fit <- new_fit[, tracking_columns]

    tracking_results <- rbind(
      tracking_results,
      new_fit
    )

    current_fit <- new_fit
  }

  target_results <- lapply(
    measurement_heights,
    function(target_height) {

      matching_row <- which(
        abs(tracking_results$height_m - target_height) < 0.001
      )

      if (length(matching_row) == 0) {
        return(data.frame(
          height_m = target_height,
          centre_x = NA_real_,
          centre_y = NA_real_,
          radius_m = NA_real_,
          estimated_diameter_cm = NA_real_,
          points_used = NA_integer_,
          circle_rmse_mm = NA_real_,
          circumference_completeness_percent = NA_real_,
          quality_flag = "failed",
          stringsAsFactors = FALSE
        ))
      }

      fit <- tracking_results[matching_row[1], ]

      quality_flag <- ifelse(
        fit$circle_rmse_mm <= 20 &&
          fit$circumference_completeness_percent >= 70 &&
          fit$retained_points >= 100,
        "acceptable",
        "inspect"
      )

      data.frame(
        height_m = target_height,
        centre_x = fit$centre_x,
        centre_y = fit$centre_y,
        radius_m = fit$radius_m,
        estimated_diameter_cm = fit$radius_m * 200,
        points_used = fit$retained_points,
        circle_rmse_mm = fit$circle_rmse_mm,
        circumference_completeness_percent =
          fit$circumference_completeness_percent,
        quality_flag = quality_flag,
        stringsAsFactors = FALSE
      )
    }
  )

  taper_table <- do.call(rbind, target_results)
  row.names(taper_table) <- NULL

  valid_rows <- is.finite(taper_table$estimated_diameter_cm)
  valid_taper <- taper_table[valid_rows, ]

  if (nrow(valid_taper) >= 2) {
    taper_model <- lm(
      estimated_diameter_cm ~ height_m,
      data = valid_taper
    )

    linear_taper_cm_per_m <-
      -unname(coef(taper_model)["height_m"])

    first_row <- valid_taper[1, ]
    last_row <- valid_taper[nrow(valid_taper), ]

    mean_taper_cm_per_m <-
      (first_row$estimated_diameter_cm -
        last_row$estimated_diameter_cm) /
      (last_row$height_m - first_row$height_m)

    stem_displacement_m <- sqrt(
      (last_row$centre_x - first_row$centre_x)^2 +
        (last_row$centre_y - first_row$centre_y)^2
    )

    measured_vertical_span_m <-
      last_row$height_m - first_row$height_m

    lower_stem_lean_degrees <-
      atan2(stem_displacement_m, measured_vertical_span_m) *
      180 / pi
  } else {
    linear_taper_cm_per_m <- NA_real_
    mean_taper_cm_per_m <- NA_real_
    stem_displacement_m <- NA_real_
    lower_stem_lean_degrees <- NA_real_
    measured_vertical_span_m <- NA_real_
  }

  summary <- data.frame(
    file_name = basename(file_path),
    requested_measurements = length(measurement_heights),
    successful_measurements = sum(valid_rows),
    acceptable_measurements = sum(
      taper_table$quality_flag == "acceptable"
    ),
    measurements_requiring_inspection = sum(
      taper_table$quality_flag == "inspect"
    ),
    failed_measurements = sum(
      taper_table$quality_flag == "failed"
    ),
    mean_taper_cm_per_m = mean_taper_cm_per_m,
    linear_taper_cm_per_m = linear_taper_cm_per_m,
    lower_stem_displacement_m = stem_displacement_m,
    measured_vertical_span_m = measured_vertical_span_m,
    lower_stem_lean_degrees = lower_stem_lean_degrees,
    stringsAsFactors = FALSE
  )

  list(
    taper_table = taper_table,
    tracking_results = tracking_results,
    summary = summary
  )
}
