# Calculate crown dimensions for the WL12 TLS tree

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

reference_tree_height_m <-
  as.numeric(wl12_reference$tree_height)

reference_crown_base_height_m <-
  as.numeric(wl12_reference$crown_base_height)

reference_crown_length_m <-
  as.numeric(wl12_reference$crown_length_r)

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

if (nrow(points) == 0) {
  stop("No valid XYZ coordinates were found.")
}

# Calculate height above the tree base ----------------------------------

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

# Select crown points ----------------------------------------------------

# This first version uses the published reference crown-base height.
# Automatic crown-base detection will be developed separately.

crown_points <- points[
  points$height_m >=
    reference_crown_base_height_m,
  ,
  drop = FALSE
]

if (nrow(crown_points) < 3) {
  stop("Fewer than three crown points were available.")
}

# Crown length -----------------------------------------------------------

calculated_crown_length_m <-
  calculated_tree_height_m -
  reference_crown_base_height_m

# Crown diameter method 1 ------------------------------------------------

# Method 1 is the mean of the crown extent in the X and Y directions.

crown_x_extent_m <- diff(
  range(
    crown_points$X,
    na.rm = TRUE
  )
)

crown_y_extent_m <- diff(
  range(
    crown_points$Y,
    na.rm = TRUE
  )
)

calculated_crown_diameter_1_m <- mean(
  c(
    crown_x_extent_m,
    crown_y_extent_m
  )
)

# Create the two-dimensional convex hull --------------------------------

hull_indices <- chull(
  crown_points$X,
  crown_points$Y
)

crown_hull <- crown_points[
  hull_indices,
  c("X", "Y"),
  drop = FALSE
]

# Close the polygon by repeating its first point.

closed_crown_hull <- rbind(
  crown_hull,
  crown_hull[1, , drop = FALSE]
)

# Crown diameter method 2 ------------------------------------------------

# For every convex-hull point, find its furthest hull point.
# The mean of those maximum distances is crown diameter method 2.

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

calculated_crown_diameter_2_m <- mean(
  furthest_distances_m
)

# Projected crown area ---------------------------------------------------

# Calculate the area inside the projected 2D convex hull using the
# shoelace formula.

x_coordinates <- closed_crown_hull$X
y_coordinates <- closed_crown_hull$Y

calculated_crown_area_m2 <- 0.5 * abs(
  sum(
    x_coordinates[-length(x_coordinates)] *
      y_coordinates[-1] -
      x_coordinates[-1] *
      y_coordinates[-length(y_coordinates)]
  )
)

# Additional crown ratios ------------------------------------------------

crown_length_to_tree_height_ratio <-
  calculated_crown_length_m /
  calculated_tree_height_m

crown_length_to_width_ratio <-
  calculated_crown_length_m /
  calculated_crown_diameter_1_m

# Compare calculated and reference values -------------------------------

crown_metrics <- data.frame(
  tree_id = "WL12",
  species = wl12_reference$species,
  method = "reference-assisted crown extraction",

  calculated_tree_height_m =
    calculated_tree_height_m,

  reference_tree_height_m =
    reference_tree_height_m,

  crown_base_height_used_m =
    reference_crown_base_height_m,

  calculated_crown_length_m =
    calculated_crown_length_m,

  reference_crown_length_m =
    reference_crown_length_m,

  crown_length_error_m =
    calculated_crown_length_m -
    reference_crown_length_m,

  calculated_crown_diameter_1_m =
    calculated_crown_diameter_1_m,

  reference_crown_diameter_1_m =
    reference_crown_diameter_1_m,

  crown_diameter_1_error_m =
    calculated_crown_diameter_1_m -
    reference_crown_diameter_1_m,

  calculated_crown_diameter_2_m =
    calculated_crown_diameter_2_m,

  reference_crown_diameter_2_m =
    reference_crown_diameter_2_m,

  crown_diameter_2_error_m =
    calculated_crown_diameter_2_m -
    reference_crown_diameter_2_m,

  calculated_crown_area_m2 =
    calculated_crown_area_m2,

  reference_crown_area_m2 =
    reference_crown_area_m2,

  crown_area_error_m2 =
    calculated_crown_area_m2 -
    reference_crown_area_m2,

  crown_x_extent_m =
    crown_x_extent_m,

  crown_y_extent_m =
    crown_y_extent_m,

  crown_length_to_tree_height_ratio =
    crown_length_to_tree_height_ratio,

  crown_length_to_width_ratio =
    crown_length_to_width_ratio,

  crown_point_count =
    nrow(crown_points),

  convex_hull_point_count =
    nrow(crown_hull),

  stringsAsFactors = FALSE
)

# Save table -------------------------------------------------------------

dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

table_output_file <- paste0(
  "outputs/tls/tables/",
  "WL12_crown_metrics.csv"
)

write.csv(
  crown_metrics,
  table_output_file,
  row.names = FALSE
)

# Prepare points for plotting -------------------------------------------

set.seed(42)

maximum_plot_points <- 100000

if (nrow(crown_points) > maximum_plot_points) {
  plot_indices <- sample(
    seq_len(nrow(crown_points)),
    maximum_plot_points
  )
} else {
  plot_indices <- seq_len(nrow(crown_points))
}

plot_crown_points <- crown_points[
  plot_indices,
  ,
  drop = FALSE
]

# Create figure ----------------------------------------------------------

dir.create(
  "outputs/tls/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

figure_output_file <- paste0(
  "outputs/tls/figures/",
  "WL12_crown_metrics.png"
)

png(
  figure_output_file,
  width = 2200,
  height = 1200,
  res = 220
)

par(
  mfrow = c(1, 2),
  mar = c(4.5, 4.5, 3.5, 1)
)

# Crown projection and convex hull

plot(
  plot_crown_points$X,
  plot_crown_points$Y,
  asp = 1,
  pch = 16,
  cex = 0.15,
  col = rgb(0.10, 0.35, 0.20, 0.15),
  xlab = "X coordinate (m)",
  ylab = "Y coordinate (m)",
  main = "Projected crown and 2D convex hull"
)

polygon(
  closed_crown_hull$X,
  closed_crown_hull$Y,
  border = "#D55E00",
  lwd = 2.5
)

# Calculated versus reference measurements

comparison_values <- rbind(
  calculated = c(
    calculated_crown_length_m,
    calculated_crown_diameter_1_m,
    calculated_crown_diameter_2_m
  ),
  reference = c(
    reference_crown_length_m,
    reference_crown_diameter_1_m,
    reference_crown_diameter_2_m
  )
)

barplot(
  comparison_values,
  beside = TRUE,
  col = c("#355C7D", "#D55E00"),
  names.arg = c(
    "Crown\nlength",
    "Crown\ndiameter 1",
    "Crown\ndiameter 2"
  ),
  ylab = "Measurement (m)",
  main = "Calculated and reference crown metrics"
)

legend(
  "topright",
  legend = c(
    "Calculated",
    "Reference"
  ),
  fill = c(
    "#355C7D",
    "#D55E00"
  ),
  bty = "n"
)

dev.off()

# Print results ----------------------------------------------------------

cat("\nWL12 crown metrics:\n")

print(
  crown_metrics[
    ,
    c(
      "tree_id",
      "calculated_crown_length_m",
      "reference_crown_length_m",
      "calculated_crown_diameter_1_m",
      "reference_crown_diameter_1_m",
      "calculated_crown_diameter_2_m",
      "reference_crown_diameter_2_m",
      "calculated_crown_area_m2",
      "reference_crown_area_m2"
    )
  ],
  row.names = FALSE
)

cat(
  "\nCrown points:",
  nrow(crown_points),
  "\n"
)

cat(
  "Convex-hull points:",
  nrow(crown_hull),
  "\n"
)

cat(
  "Crown table saved to:",
  table_output_file,
  "\n"
)

cat(
  "Crown figure saved to:",
  figure_output_file,
  "\n"
)