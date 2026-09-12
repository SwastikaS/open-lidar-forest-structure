library(MASS)

# Load the previously extracted breast-height slice
dbh_slice <- read.csv(
  "data/processed/tls/WL12_DBH_slice_raw.csv"
)

# Load reference measurements
tree_parameters <- read.csv(
  "data/reference/tls/Tree Parameters TLS AD WL.csv",
  sep = ";",
  check.names = FALSE
)

reference <- tree_parameters[
  tree_parameters$ID == "WL12",
]

# Preliminary centre estimated using coordinate medians
preliminary_x <- median(dbh_slice$X)
preliminary_y <- median(dbh_slice$Y)

dbh_slice$preliminary_distance <- sqrt(
  (dbh_slice$X - preliminary_x)^2 +
    (dbh_slice$Y - preliminary_y)^2
)

# Remove points abnormally far from the main stem ring
median_distance <- median(dbh_slice$preliminary_distance)
distance_mad <- mad(dbh_slice$preliminary_distance)

maximum_distance <- median_distance + 6 * distance_mad

clean_slice <- dbh_slice[
  dbh_slice$preliminary_distance <= maximum_distance,
]

cat("Original slice points:", nrow(dbh_slice), "\n")
cat("Retained stem points:", nrow(clean_slice), "\n")
cat("Removed outliers:", nrow(dbh_slice) - nrow(clean_slice), "\n")

# Centre coordinates to improve numerical stability
origin_x <- median(clean_slice$X)
origin_y <- median(clean_slice$Y)

clean_slice$x_centred <- clean_slice$X - origin_x
clean_slice$y_centred <- clean_slice$Y - origin_y

# Robust algebraic circle fit
circle_model <- MASS::rlm(
  I(-(x_centred^2 + y_centred^2)) ~
    x_centred + y_centred,
  data = clean_slice,
  maxit = 200
)

coefficients_circle <- coef(circle_model)

centre_x_relative <- -coefficients_circle["x_centred"] / 2
centre_y_relative <- -coefficients_circle["y_centred"] / 2

radius_m <- sqrt(
  centre_x_relative^2 +
    centre_y_relative^2 -
    coefficients_circle["(Intercept)"]
)

centre_x <- centre_x_relative + origin_x
centre_y <- centre_y_relative + origin_y

dbh_m <- 2 * radius_m
dbh_cm <- dbh_m * 100

# Radial fitting residuals
radial_distances <- sqrt(
  (clean_slice$X - centre_x)^2 +
    (clean_slice$Y - centre_y)^2
)

circle_rmse_mm <- sqrt(
  mean((radial_distances - radius_m)^2)
) * 1000

# Circumferential completeness using 36 angular sectors
angles <- atan2(
  clean_slice$Y - centre_y,
  clean_slice$X - centre_x
)

angle_bins <- cut(
  angles,
  breaks = seq(-pi, pi, length.out = 37),
  include.lowest = TRUE
)

circumference_completeness <- (
  sum(table(angle_bins) > 0) / 36
) * 100

# Reference measurements
measured_dbh_m <- reference[["d1.3"]]
qsm_dbh_m <- reference[["QSM_DBH"]]

dbh_error_cm <- (dbh_m - measured_dbh_m) * 100
dbh_error_percent <- (
  (dbh_m - measured_dbh_m) / measured_dbh_m
) * 100

dbh_results <- data.frame(
  tree_id = "WL12",
  species = "Fagus sylvatica",
  estimated_dbh_cm = dbh_cm,
  measured_dbh_cm = measured_dbh_m * 100,
  qsm_dbh_cm = qsm_dbh_m * 100,
  estimation_error_cm = dbh_error_cm,
  estimation_error_percent = dbh_error_percent,
  circle_rmse_mm = circle_rmse_mm,
  circumference_completeness_percent =
    circumference_completeness,
  original_points = nrow(dbh_slice),
  retained_points = nrow(clean_slice)
)

print(dbh_results)

# Coordinates for the fitted circle
theta <- seq(0, 2 * pi, length.out = 500)

circle_x <- centre_x + radius_m * cos(theta)
circle_y <- centre_y + radius_m * sin(theta)

plot_dbh_fit <- function() {
  plot(
    dbh_slice$X,
    dbh_slice$Y,
    asp = 1,
    pch = 16,
    cex = 0.45,
    col = "grey75",
    xlab = "X (m)",
    ylab = "Y (m)",
    main = paste0(
      "WL12 DBH estimate: ",
      round(dbh_cm, 2),
      " cm"
    )
  )
  
  points(
    clean_slice$X,
    clean_slice$Y,
    pch = 16,
    cex = 0.45,
    col = adjustcolor("steelblue4", alpha.f = 0.5)
  )
  
  lines(
    circle_x,
    circle_y,
    col = "red3",
    lwd = 2
  )
  
  points(
    centre_x,
    centre_y,
    pch = 3,
    col = "red3",
    cex = 1.3,
    lwd = 2
  )
  
  legend(
    "topright",
    legend = c("Removed/outlying points", "Retained stem", "Fitted circle"),
    col = c("grey75", "steelblue4", "red3"),
    pch = c(16, 16, NA),
    lty = c(NA, NA, 1),
    bty = "n"
  )
}

plot_dbh_fit()

png(
  "outputs/figures/tls/WL12_DBH_circle_fit.png",
  width = 1400,
  height = 1400,
  res = 180
)

plot_dbh_fit()
dev.off()

write.csv(
  dbh_results,
  "outputs/tables/tls/WL12_DBH_results.csv",
  row.names = FALSE
)

row.names(dbh_results) <- NULL
