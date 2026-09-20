# Compare methods for calculating TLS crown width and projected area

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

# Input files ------------------------------------------------------------

tls_file <- paste0(
  "data/raw/tls/site1_WL_group2/",
  "WL12_FagSyl_2020-12-04.laz"
)

reference_file <- paste0(
  "data/reference/tls/",
  "Tree Parameters TLS AD WL.csv"
)

if (!file.exists(tls_file)) {
  stop("TLS point-cloud file was not found: ", tls_file)
}

if (!file.exists(reference_file)) {
  stop("Reference table was not found: ", reference_file)
}

# Read reference measurements -------------------------------------------

reference <- read.csv(
  reference_file,
  sep = ";",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

wl12_reference <- reference[
  reference$ID == "WL12",
  ,
  drop = FALSE
]

if (nrow(wl12_reference) != 1) {
  stop("Exactly one WL12 reference record was expected.")
}

reference_crown_base_height_m <-
  as.numeric(wl12_reference$crown_base_height)

reference_crown_diameter_1_m <-
  as.numeric(wl12_reference$crown_dia1)

reference_crown_diameter_2_m <-
  as.numeric(wl12_reference$crown_dia2)

reference_crown_area_m2 <-
  as.numeric(wl12_reference$crown_area)

# Read TLS point cloud ---------------------------------------------------

tls_tree <- readLAS(
  tls_file,
  select = "xyz"
)

if (is.empty(tls_tree)) {
  stop("The TLS point cloud is empty.")
}

points <- as.data.frame(tls_tree@data)[
  ,
  c("X", "Y", "Z")
]

points <- points[
  is.finite(points$X) &
    is.finite(points$Y) &
    is.finite(points$Z),
]

tree_base_z <- min(
  points$Z,
  na.rm = TRUE
)

points$height_m <-
  points$Z - tree_base_z

# Reference-assisted crown selection ------------------------------------

crown_points <- points[
  points$height_m >=
    reference_crown_base_height_m,
  ,
  drop = FALSE
]

if (nrow(crown_points) < 3) {
  stop("Fewer than three crown points were available.")
}

# Original-coordinate crown widths --------------------------------------

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

raw_mean_width_m <- mean(
  c(
    raw_x_extent_m,
    raw_y_extent_m
  )
)

# Rotate the crown using principal component analysis -------------------

# Rotation avoids making the crown width depend entirely on the
# original X and Y orientation of the point cloud.

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

rotated_xy <- centred_xy %*% pca_axes

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

pca_mean_width_m <- mean(
  c(
    pca_x_extent_m,
    pca_y_extent_m
  )
)

# Calculate robust PCA widths -------------------------------------------

# These widths exclude the outermost one per cent of points on each
# side. They are diagnostic values, not replacements for the full width.

pca_x_limits <- quantile(
  rotated_xy[, 1],
  probabilities = c(0.01, 0.99),
  na.rm = TRUE,
  names = FALSE
)

pca_y_limits <- quantile(
  rotated_xy[, 2],
  probabilities = c(0.01, 0.99),
  na.rm = TRUE,
  names = FALSE
)

robust_pca_x_extent_m <-
  pca_x_limits[2] - pca_x_limits[1]

robust_pca_y_extent_m <-
  pca_y_limits[2] - pca_y_limits[1]

robust_pca_mean_width_m <- mean(
  c(
    robust_pca_x_extent_m,
    robust_pca_y_extent_m
  )
)

# Convex-hull calculations ----------------------------------------------

hull_indices <- chull(
  crown_points$X,
  crown_points$Y
)

crown_hull <- crown_points[
  hull_indices,
  c("X", "Y"),
  drop = FALSE
]

closed_crown_hull <- rbind(
  crown_hull,
  crown_hull[1, , drop = FALSE]
)

x_coordinates <- closed_crown_hull$X
y_coordinates <- closed_crown_hull$Y

convex_hull_area_m2 <- 0.5 * abs(
  sum(
    x_coordinates[-length(x_coordinates)] *
      y_coordinates[-1] -
      x_coordinates[-1] *
      y_coordinates[-length(y_coordinates)]
  )
)

hull_distance_matrix <- as.matrix(
  dist(
    crown_hull[
      ,
      c("X", "Y")
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

# Occupied-grid area -----------------------------------------------------

# Project the crown onto grids of different resolutions.
# A grid cell is counted once if it contains at least one crown point.

calculate_occupied_grid <- function(
    x,
    y,
    grid_size_m
) {

  minimum_x <- min(
    x,
    na.rm = TRUE
  )

  minimum_y <- min(
    y,
    na.rm = TRUE
  )

  x_cell <- floor(
    (x - minimum_x) /
      grid_size_m
  )

  y_cell <- floor(
    (y - minimum_y) /
      grid_size_m
  )

  number_of_x_cells <-
    max(x_cell) + 1

  cell_id <-
    x_cell +
    y_cell * number_of_x_cells

  occupied_cells <- length(
    unique(cell_id)
  )

  data.frame(
    grid_size_m = grid_size_m,
    occupied_cells = occupied_cells,
    occupied_area_m2 =
      occupied_cells *
      grid_size_m^2,
    stringsAsFactors = FALSE
  )
}

grid_sizes_m <- c(
  0.02,
  0.04,
  0.05,
  0.10,
  0.20,
  0.25,
  0.50
)

grid_area_results <- do.call(
  rbind,
  lapply(
    grid_sizes_m,
    function(grid_size) {
      calculate_occupied_grid(
        x = crown_points$X,
        y = crown_points$Y,
        grid_size_m = grid_size
      )
    }
  )
)

row.names(grid_area_results) <- NULL

grid_area_results$reference_crown_area_m2 <-
  reference_crown_area_m2

grid_area_results$area_error_m2 <-
  grid_area_results$occupied_area_m2 -
  reference_crown_area_m2

grid_area_results$absolute_area_error_m2 <-
  abs(
    grid_area_results$area_error_m2
  )

closest_grid_row <- which.min(
  grid_area_results$absolute_area_error_m2
)

grid_area_results$closest_to_reference <- FALSE

grid_area_results$closest_to_reference[
  closest_grid_row
] <- TRUE

# Prepare width-comparison table ----------------------------------------

width_results <- data.frame(
  method = c(
    "Original X-Y mean extent",
    "PCA-rotated mean extent",
    "Robust PCA mean extent",
    "Convex-hull diameter method",
    "Reference crown diameter 1",
    "Reference crown diameter 2"
  ),
  crown_width_m = c(
    raw_mean_width_m,
    pca_mean_width_m,
    robust_pca_mean_width_m,
    convex_hull_diameter_m,
    reference_crown_diameter_1_m,
    reference_crown_diameter_2_m
  ),
  stringsAsFactors = FALSE
)

# Prepare area-comparison table -----------------------------------------

area_results <- rbind(
  data.frame(
    method = "Complete 2D convex hull",
    grid_size_m = NA_real_,
    occupied_cells = NA_integer_,
    calculated_area_m2 =
      convex_hull_area_m2,
    reference_area_m2 =
      reference_crown_area_m2,
    area_error_m2 =
      convex_hull_area_m2 -
      reference_crown_area_m2,
    stringsAsFactors = FALSE
  ),
  data.frame(
    method = paste0(
      "Occupied grid: ",
      grid_area_results$grid_size_m,
      " m"
    ),
    grid_size_m =
      grid_area_results$grid_size_m,
    occupied_cells =
      grid_area_results$occupied_cells,
    calculated_area_m2 =
      grid_area_results$occupied_area_m2,
    reference_area_m2 =
      reference_crown_area_m2,
    area_error_m2 =
      grid_area_results$area_error_m2,
    stringsAsFactors = FALSE
  )
)

area_results$absolute_area_error_m2 <-
  abs(
    area_results$area_error_m2
  )

# Save tables ------------------------------------------------------------

dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

width_output_file <- paste0(
  "outputs/tls/tables/",
  "WL12_crown_width_method_comparison.csv"
)

area_output_file <- paste0(
  "outputs/tls/tables/",
  "WL12_crown_area_method_comparison.csv"
)

write.csv(
  width_results,
  width_output_file,
  row.names = FALSE
)

write.csv(
  area_results,
  area_output_file,
  row.names = FALSE
)

# Create diagnostic figure ----------------------------------------------

dir.create(
  "outputs/tls/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

figure_output_file <- paste0(
  "outputs/tls/figures/",
  "WL12_crown_area_method_comparison.png"
)

png(
  figure_output_file,
  width = 2300,
  height = 1200,
  res = 220
)

par(
  mfrow = c(1, 2),
  mar = c(5, 5, 3.5, 1)
)

# Occupied projected area by grid size

plot(
  grid_area_results$grid_size_m,
  grid_area_results$occupied_area_m2,
  type = "b",
  pch = 16,
  lwd = 2,
  col = "#355C7D",
  xlab = "Grid-cell size (m)",
  ylab = "Occupied projected area (m²)",
  main = "Occupied crown area by grid size"
)

abline(
  h = reference_crown_area_m2,
  col = "#D55E00",
  lwd = 2,
  lty = 2
)

abline(
  h = convex_hull_area_m2,
  col = "#347847",
  lwd = 2,
  lty = 3
)

legend(
  "bottomright",
  legend = c(
    "Occupied-grid area",
    "Reference crown area",
    "Complete convex-hull area"
  ),
  col = c(
    "#355C7D",
    "#D55E00",
    "#347847"
  ),
  lty = c(1, 2, 3),
  pch = c(16, NA, NA),
  lwd = 2,
  bty = "n",
  cex = 0.8
)

# Crown-width comparison

bar_colours <- c(
  "#355C7D",
  "#4C78A8",
  "#72A0C1",
  "#347847",
  "#D55E00",
  "#E69F00"
)

barplot(
  height = width_results$crown_width_m,
  names.arg = c(
    "Raw\nextent",
    "PCA\nextent",
    "Robust\nPCA",
    "Hull\ndiameter",
    "Reference\ndiameter 1",
    "Reference\ndiameter 2"
  ),
  col = bar_colours,
  las = 2,
  ylab = "Crown width (m)",
  main = "Comparison of crown-width methods",
  ylim = c(
    0,
    max(width_results$crown_width_m) * 1.15
  )
)

dev.off()

# Print results ----------------------------------------------------------

cat("\nCrown-width method comparison:\n")

print(
  width_results,
  row.names = FALSE
)

cat("\nProjected crown-area comparison:\n")

print(
  area_results,
  row.names = FALSE
)

cat(
  "\nGrid size closest to the reference area:",
  grid_area_results$grid_size_m[
    closest_grid_row
  ],
  "m\n"
)

cat(
  "Its occupied projected area:",
  grid_area_results$occupied_area_m2[
    closest_grid_row
  ],
  "m2\n"
)

cat(
  "\nWidth comparison saved to:",
  width_output_file,
  "\n"
)

cat(
  "Area comparison saved to:",
  area_output_file,
  "\n"
)

cat(
  "Figure saved to:",
  figure_output_file,
  "\n"
)