library(lidR)
library(MASS)

tls_tree <- readLAS(
  "data/raw/tls/site1_WL_group2/WL12_FagSyl_2020-12-04.laz"
)

points <- as.data.frame(tls_tree@data)[, c("X", "Y", "Z")]
points$height_m <- points$Z - min(points$Z)

fit_circle_rlm <- function(d) {

  origin_x <- median(d$X)
  origin_y <- median(d$Y)

  d$x_local <- d$X - origin_x
  d$y_local <- d$Y - origin_y

  model <- MASS::rlm(
    I(-(x_local^2 + y_local^2)) ~ x_local + y_local,
    data = d,
    maxit = 200
  )

  b <- coef(model)

  cx_local <- -b["x_local"] / 2
  cy_local <- -b["y_local"] / 2

  radius <- sqrt(
    cx_local^2 +
      cy_local^2 -
      b["(Intercept)"]
  )

  centre_x <- origin_x + cx_local
  centre_y <- origin_y + cy_local

  radial_residual <- sqrt(
    (d$X - centre_x)^2 +
      (d$Y - centre_y)^2
  ) - radius

  data.frame(
    centre_x = unname(centre_x),
    centre_y = unname(centre_y),
    radius_m = unname(radius),
    points_used = nrow(d),
    circle_rmse_mm =
      sqrt(mean(radial_residual^2)) * 1000
  )
}

# Establish the reliable starting circle at 1.3 m
initial_slice <- points[
  points$height_m >= 1.25 &
    points$height_m <= 1.35,
]

initial_x <- median(initial_slice$X)
initial_y <- median(initial_slice$Y)

initial_slice$distance <- sqrt(
  (initial_slice$X - initial_x)^2 +
    (initial_slice$Y - initial_y)^2
)

initial_cutoff <- median(initial_slice$distance) +
  6 * mad(initial_slice$distance)

initial_clean <- initial_slice[
  initial_slice$distance <= initial_cutoff,
]

current_fit <- fit_circle_rlm(initial_clean)
current_fit$height_m <- 1.3

tracking_results <- current_fit

# Track the stem upwards in small 10 cm steps
tracking_heights <- seq(1.4, 6, by = 0.1)

for (height in tracking_heights) {

  height_slice <- points[
    points$height_m >= height - 0.05 &
      points$height_m <= height + 0.05,
  ]

  distance_from_previous_centre <- sqrt(
    (height_slice$X - current_fit$centre_x)^2 +
      (height_slice$Y - current_fit$centre_y)^2
  )

  # Retain points close to the preceding stem circumference
  stem_candidates <- height_slice[
    abs(
      distance_from_previous_centre -
        current_fit$radius_m
    ) <= 0.08,
  ]

  if (nrow(stem_candidates) < 30) {
    warning(
      paste("Insufficient stem points at", height, "m")
    )
    next
  }

  new_fit <- fit_circle_rlm(stem_candidates)

  # Reject implausible jumps caused by branches
  centre_shift <- sqrt(
    (new_fit$centre_x - current_fit$centre_x)^2 +
      (new_fit$centre_y - current_fit$centre_y)^2
  )

  radius_change <- abs(
    new_fit$radius_m - current_fit$radius_m
  )

  if (
    centre_shift > 0.12 ||
    radius_change > 0.04 ||
    new_fit$radius_m > 0.35
  ) {
    warning(
      paste("Rejected unreliable fit at", height, "m")
    )
    next
  }

  new_fit$height_m <- height

  tracking_results <- rbind(
    tracking_results,
    new_fit
  )

  current_fit <- new_fit
}

# Extract target measurement heights
measurement_heights <- c(1.3, 2, 4, 6)

taper_results <- do.call(
  rbind,
  lapply(
    measurement_heights,
    function(target_height) {
      tracking_results[
        which.min(
          abs(tracking_results$height_m - target_height)
        ),
      ]
    }
  )
)

tree_parameters <- read.csv(
  "data/reference/tls/Tree Parameters TLS AD WL.csv",
  sep = ";",
  check.names = FALSE
)

reference <- tree_parameters[
  tree_parameters$ID == "WL12",
]

taper_results$reference_diameter_m <- c(
  reference[["d1.3"]],
  reference[["d2"]],
  reference[["d4"]],
  reference[["d6"]]
)

taper_results$estimated_diameter_cm <-
  taper_results$radius_m * 200

taper_results$reference_diameter_cm <-
  taper_results$reference_diameter_m * 100

taper_results$error_cm <-
  taper_results$estimated_diameter_cm -
  taper_results$reference_diameter_cm

taper_results$quality <- ifelse(
  taper_results$circle_rmse_mm <= 20,
  "acceptable",
  "inspect"
)

row.names(taper_results) <- NULL

print(taper_results)

plot(
  taper_results$estimated_diameter_cm,
  taper_results$height_m,
  type = "b",
  pch = 16,
  lwd = 2,
  col = "steelblue4",
  xlab = "Stem diameter (cm)",
  ylab = "Height above base (m)",
  main = "WL12 stem taper"
)

lines(
  taper_results$reference_diameter_cm,
  taper_results$height_m,
  type = "b",
  pch = 17,
  lwd = 2,
  col = "red3"
)

legend(
  "topright",
  legend = c("Stem-tracked estimate", "Dataset reference"),
  col = c("steelblue4", "red3"),
  pch = c(16, 17),
  lty = 1,
  bty = "n"
)

write.csv(
  taper_results,
  "outputs/tls/tables/WL12_stem_taper.csv",
  row.names = FALSE
)



# Inspect the flagged cross-section at 2 m

# Select the fitted result nearest to exactly 2 m
fit_2m <- taper_results[
  which.min(abs(taper_results$height_m - 2)),
]

# Extract points between 1.95 and 2.05 m above the tree base
slice_2m <- points[
  points$height_m >= 1.95 &
    points$height_m <= 2.05,
]

# Calculate distance from the fitted stem centre
distance_from_centre <- sqrt(
  (slice_2m$X - fit_2m$centre_x)^2 +
    (slice_2m$Y - fit_2m$centre_y)^2
)

# Retain the local area surrounding the stem
display_points <- slice_2m[
  is.finite(distance_from_centre) &
    distance_from_centre <= 0.45,
]

cat("All points in 2 m slice:", nrow(slice_2m), "\n")
cat("Points displayed near stem:", nrow(display_points), "\n")

if (nrow(display_points) == 0) {
  stop("No points were found around the fitted stem centre.")
}

# Create fitted-circle coordinates
theta <- seq(0, 2 * pi, length.out = 500)

circle_x <- fit_2m$centre_x +
  fit_2m$radius_m * cos(theta)

circle_y <- fit_2m$centre_y +
  fit_2m$radius_m * sin(theta)

# Plot the cross-section
plot(
  display_points$X,
  display_points$Y,
  asp = 1,
  pch = 16,
  cex = 0.4,
  col = rgb(0, 0, 0, 0.35),
  xlab = "Local X (m)",
  ylab = "Local Y (m)",
  main = "WL12 stem cross-section at 2 m"
)

lines(
  circle_x,
  circle_y,
  col = "red",
  lwd = 2
)

points(
  fit_2m$centre_x,
  fit_2m$centre_y,
  pch = 3,
  col = "blue",
  cex = 1.2,
  lwd = 2
)

# Summarise lower-stem structure

lowest_fit <- taper_results[
  which.min(taper_results$height_m),
]

highest_fit <- taper_results[
  which.max(taper_results$height_m),
]

vertical_interval_m <-
  highest_fit$height_m - lowest_fit$height_m

horizontal_displacement_m <- sqrt(
  (highest_fit$centre_x - lowest_fit$centre_x)^2 +
    (highest_fit$centre_y - lowest_fit$centre_y)^2
)

lower_stem_lean_degrees <- atan(
  horizontal_displacement_m / vertical_interval_m
) * 180 / pi

mean_taper_cm_per_m <- (
  lowest_fit$estimated_diameter_cm -
    highest_fit$estimated_diameter_cm
) / vertical_interval_m

taper_model <- lm(
  estimated_diameter_cm ~ height_m,
  data = taper_results
)

linear_taper_cm_per_m <-
  -unname(coef(taper_model)["height_m"])

estimated_dbh_m <- taper_results$estimated_diameter_cm[
  which.min(abs(taper_results$height_m - 1.3))
] / 100

basal_area_m2 <- pi * (estimated_dbh_m / 2)^2

stem_structure_summary <- data.frame(
  tree_id = "WL12",
  species = "Fagus sylvatica",
  estimated_dbh_cm = estimated_dbh_m * 100,
  calculated_height_m = max(points$height_m),
  basal_area_m2 = basal_area_m2,
  diameter_at_6m_cm = highest_fit$estimated_diameter_cm,
  mean_taper_cm_per_m = mean_taper_cm_per_m,
  linear_taper_cm_per_m = linear_taper_cm_per_m,
  lower_stem_displacement_m = horizontal_displacement_m,
  lower_stem_lean_degrees = lower_stem_lean_degrees,
  mean_absolute_diameter_error_cm =
    mean(abs(taper_results$error_cm))
)

print(stem_structure_summary)

write.csv(
  stem_structure_summary,
  "outputs/tls/tables/WL12_stem_structure_summary.csv",
  row.names = FALSE
)

write.csv(
  tracking_results,
  "outputs/tls/tables/WL12_tracked_stem_centres.csv",
  row.names = FALSE
)
cat("Accepted tracking heights:", nrow(tracking_results), "\n")
range(tracking_results$height_m)