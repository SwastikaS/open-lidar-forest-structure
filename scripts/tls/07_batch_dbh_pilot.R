# Pilot validation of hybrid TLS DBH estimation

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)
library(MASS)

pilot_ids <- c(
  "WL01",
  "WL12",
  "WL20",
  "AD04",
  "AD06",
  "AD13"
)

# Prepare pilot inventory --------------------------------------------------

tls_files <- list.files(
  "data/raw/tls",
  pattern = "[.]laz$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

pilot_inventory <- data.frame(
  tree_id = toupper(sub("_.*$", "", basename(tls_files))),
  file_path = tls_files,
  stringsAsFactors = FALSE
)

pilot_inventory <- pilot_inventory[
  pilot_inventory$tree_id %in% pilot_ids,
]

reference <- read.csv(
  "data/reference/tls/Tree Parameters TLS AD WL.csv",
  sep = ";",
  check.names = FALSE
)

standardise_tree_id <- function(x) {
  x <- toupper(trimws(x))

  sub(
    "^([A-Z]+)0+([0-9]+)$",
    "\\1\\2",
    x
  )
}

pilot_inventory$match_id <-
  standardise_tree_id(pilot_inventory$tree_id)

reference$match_id <-
  standardise_tree_id(reference$ID)

reference_row <- match(
  pilot_inventory$match_id,
  reference$match_id
)

pilot_inventory$species <-
  reference$species[reference_row]

pilot_inventory$reference_dbh_cm <-
  reference[["d1.3"]][reference_row] * 100

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

# Process pilot trees -----------------------------------------------------

pilot_results <- vector(
  "list",
  nrow(pilot_inventory)
)

for (i in seq_len(nrow(pilot_inventory))) {

  cat(
    "Processing",
    i,
    "of",
    nrow(pilot_inventory),
    ":",
    pilot_inventory$tree_id[i],
    "\n"
  )

  pilot_results[[i]] <- tryCatch(
    {
      fit <- estimate_tree_dbh(
        pilot_inventory$file_path[i]
      )

      data.frame(
        tree_id = pilot_inventory$tree_id[i],
        species = pilot_inventory$species[i],
        method_used = fit$method_used,
        reference_dbh_cm =
          pilot_inventory$reference_dbh_cm[i],
        estimated_dbh_cm =
          fit$estimated_dbh_cm,
        error_cm =
          fit$estimated_dbh_cm -
          pilot_inventory$reference_dbh_cm[i],
        absolute_error_cm =
          abs(
            fit$estimated_dbh_cm -
              pilot_inventory$reference_dbh_cm[i]
          ),
        circle_rmse_mm =
          fit$circle_rmse_mm,
        circumference_completeness_percent =
          fit$circumference_completeness_percent,
        original_slice_points =
          fit$original_slice_points,
        retained_points =
          fit$retained_points,
        removed_points =
          fit$removed_points,
        accepted_tracking_heights =
          fit$accepted_tracking_heights,
        processing_status = "success",
        error_message = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      data.frame(
        tree_id = pilot_inventory$tree_id[i],
        species = pilot_inventory$species[i],
        method_used = NA_character_,
        reference_dbh_cm =
          pilot_inventory$reference_dbh_cm[i],
        estimated_dbh_cm = NA_real_,
        error_cm = NA_real_,
        absolute_error_cm = NA_real_,
        circle_rmse_mm = NA_real_,
        circumference_completeness_percent =
          NA_real_,
        original_slice_points = NA_real_,
        retained_points = NA_real_,
        removed_points = NA_real_,
        accepted_tracking_heights = NA_real_,
        processing_status = "failed",
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    }
  )

  invisible(gc())
}

pilot_results <- do.call(
  rbind,
  pilot_results
)

# Quality flags -----------------------------------------------------------

pilot_results$quality_flag <- ifelse(
  pilot_results$processing_status == "failed",
  "failed",
  ifelse(
    pilot_results$circle_rmse_mm <= 20 &
      pilot_results$circumference_completeness_percent >= 70 &
      pilot_results$retained_points >= 100,
    "acceptable",
    "inspect"
  )
)

successful_rows <-
  pilot_results$processing_status == "success"

pilot_summary <- data.frame(
  pilot_trees = nrow(pilot_results),
  successful_fits = sum(successful_rows),
  failed_fits = sum(!successful_rows),
  tracking_fits = sum(
    pilot_results$method_used ==
      "base-to-DBH tracking",
    na.rm = TRUE
  ),
  direct_fallback_fits = sum(
    pilot_results$method_used ==
      "direct-fit fallback",
    na.rm = TRUE
  ),
  acceptable_fits = sum(
    pilot_results$quality_flag == "acceptable",
    na.rm = TRUE
  ),
  fits_requiring_inspection = sum(
    pilot_results$quality_flag == "inspect",
    na.rm = TRUE
  ),
  mean_absolute_error_cm = mean(
    pilot_results$absolute_error_cm[
      successful_rows
    ],
    na.rm = TRUE
  ),
  root_mean_square_error_cm = sqrt(
    mean(
      pilot_results$error_cm[
        successful_rows
      ]^2,
      na.rm = TRUE
    )
  )
)

# Save outputs ------------------------------------------------------------

dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  pilot_results,
  "outputs/tls/tables/tls_dbh_pilot_results.csv",
  row.names = FALSE
)

write.csv(
  pilot_summary,
  "outputs/tls/tables/tls_dbh_pilot_summary.csv",
  row.names = FALSE
)

print(pilot_results)
print(pilot_summary)