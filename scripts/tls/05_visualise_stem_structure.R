library(lidR)

tls_tree <- readLAS(
  "data/raw/tls/site1_WL_group2/WL12_FagSyl_2020-12-04.laz"
)

points <- as.data.frame(tls_tree@data)[, c("X", "Y", "Z")]
points$height_m <- points$Z - min(points$Z)

tracking <- read.csv(
  "outputs/tables/tls/WL12_tracked_stem_centres.csv"
)

# Retain the vertical range covered by stem tracking
lower_points <- points[
  points$height_m >= min(tracking$height_m) &
    points$height_m <= max(tracking$height_m),
]

# Interpolate the stem centre and radius at every point height
lower_points$centre_x <- approx(
  tracking$height_m,
  tracking$centre_x,
  xout = lower_points$height_m,
  rule = 2
)$y

lower_points$centre_y <- approx(
  tracking$height_m,
  tracking$centre_y,
  xout = lower_points$height_m,
  rule = 2
)$y

lower_points$radius_m <- approx(
  tracking$height_m,
  tracking$radius_m,
  xout = lower_points$height_m,
  rule = 2
)$y

lower_points$radial_distance <- sqrt(
  (lower_points$X - lower_points$centre_x)^2 +
    (lower_points$Y - lower_points$centre_y)^2
)

# Select returns close to the reconstructed stem surface
stem_surface <- lower_points[
  abs(
    lower_points$radial_distance -
      lower_points$radius_m
  ) <= 0.04,
]

cat("Points used in stem visualisation:", nrow(stem_surface), "\n")

draw_stem_profile <- function() {
  
  par(mfrow = c(1, 2))
  
  plot(
    stem_surface$X,
    stem_surface$height_m,
    pch = 16,
    cex = 0.2,
    col = rgb(0.25, 0.25, 0.25, 0.25),
    xlab = "Local X (m)",
    ylab = "Height above base (m)",
    main = "X–height stem profile"
  )
  
  lines(
    tracking$centre_x,
    tracking$height_m,
    col = "blue",
    lwd = 2
  )
  
  lines(
    tracking$centre_x - tracking$radius_m,
    tracking$height_m,
    col = "red",
    lwd = 2
  )
  
  lines(
    tracking$centre_x + tracking$radius_m,
    tracking$height_m,
    col = "red",
    lwd = 2
  )
  
  plot(
    stem_surface$Y,
    stem_surface$height_m,
    pch = 16,
    cex = 0.2,
    col = rgb(0.25, 0.25, 0.25, 0.25),
    xlab = "Local Y (m)",
    ylab = "Height above base (m)",
    main = "Y–height stem profile"
  )
  
  lines(
    tracking$centre_y,
    tracking$height_m,
    col = "blue",
    lwd = 2
  )
  
  lines(
    tracking$centre_y - tracking$radius_m,
    tracking$height_m,
    col = "red",
    lwd = 2
  )
  
  lines(
    tracking$centre_y + tracking$radius_m,
    tracking$height_m,
    col = "red",
    lwd = 2
  )
  
  par(mfrow = c(1, 1))
}

# Display in RStudio
draw_stem_profile()

# Save the figure
dir.create(
  "outputs/figures/tls",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "outputs/figures/tls/WL12_lower_stem_profile.png",
  width = 1800,
  height = 900,
  res = 160
)

draw_stem_profile()
dev.off()