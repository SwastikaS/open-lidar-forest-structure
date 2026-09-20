# Investigate the WL20 crown-diameter disagreement

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

tree_id <- "WL20"

# Locate the point cloud --------------------------------------------------

tls_files <- list.files(
  "data/raw/tls",
  pattern = "[.]laz$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

wl20_file <- tls_files[
  grepl(
    "^WL20_",
    basename(tls_files),
    ignore.case = TRUE
  )
]

if (length(wl20_file) == 0) {
  stop("The WL20 point cloud was not found.")
}

if (length(wl20_file) > 1) {
  stop(
    "More than one WL20 point cloud was found:\n",
    paste(wl20_file, collapse = "\n")
  )
}

# Read reference measurements --------------------------------------------

reference_file <-
  "data/reference/tls/Tree Parameters TLS AD WL.csv"

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

standardise_tree_id <- function(x) {
  x <- toupper(trimws(x))

  sub(
    "^([A-Z]+)0+([0-9]+)$",
    "\\1\\2",
    x
  )
}

reference_row <- match(
  standardise_tree_id(tree_id),
  standardise_tree_id(reference$ID)
)

if (is.na(reference_row)) {
  stop("WL20 was not found in the reference table.")
}

reference_species <-
  reference$species[reference_row]

reference_crown_base_m <-
  as.numeric(
    reference$crown_base_height[reference_row]
  )

reference_diameter_1_m <-
  as.numeric(
    reference$crown_dia1[reference_row]
  )

reference_diameter_2_m <-
  as.numeric(
    reference$crown_dia2[reference_row]
  )

reference_area_m2 <-
  as.numeric(
    reference$crown_area[reference_row]
  )

# Read WL20 ---------------------------------------------------------------

wl20_las <- readLAS(
  wl20_file,
  select = "xyz"
)

if (is.empty(wl20_las)) {
  stop("The WL20 point cloud is empty.")
}

points <- as.data.frame(
  wl20_las@data
)[
  ,
  c("X", "Y", "Z")
]

tree_base_z <- min(
  points$Z,
  na.rm = TRUE
)

points$height_above_base_m <-
  points$Z - tree_base_z

calculated_tree_height_m <-
  max(
    points$height_above_base_m,
    na.rm = TRUE
  )

# Select crown points using the same provisional rule as the validation:
# points at or above the published crown-base height.

crown_points <- points[
  is.finite(points$height_above_base_m) &
    points$height_above_base_m >=
    reference_crown_base_m,
  c("X", "Y", "Z", "height_above_base_m")
]

if (nrow(crown_points) < 3) {
  stop(
    "Fewer than three crown points remained ",
    "after crown selection."
  )
}

# Original X-Y crown diameter --------------------------------------------

x_extent_m <- diff(
  range(crown_points$X, na.rm = TRUE)
)

y_extent_m <- diff(
  range(crown_points$Y, na.rm = TRUE)
)

calculated_raw_diameter_1_m <-
  mean(
    c(
      x_extent_m,
      y_extent_m
    )
  )

# PCA-rotated crown diameter ---------------------------------------------

pca_fit <- prcomp(
  crown_points[, c("X", "Y")],
  center = TRUE,
  scale. = FALSE
)

pca_coordinates <- as.data.frame(
  pca_fit$x[, 1:2, drop = FALSE]
)

pca_extent_1_m <- diff(
  range(
    pca_coordinates$PC1,
    na.rm = TRUE
  )
)

pca_extent_2_m <- diff(
  range(
    pca_coordinates$PC2,
    na.rm = TRUE
  )
)

calculated_pca_diameter_1_m <-
  mean(
    c(
      pca_extent_1_m,
      pca_extent_2_m
    )
  )

# Calculate the two-dimensional convex hull -------------------------------

hull_indices <- chull(
  crown_points$X,
  crown_points$Y
)

hull_points <- crown_points[
  hull_indices,
  c("X", "Y")
]

if (nrow(hull_points) < 3) {
  stop(
    "The projected crown did not produce ",
    "a valid convex hull."
  )
}

# Close the polygon for plotting

closed_hull <- rbind(
  hull_points,
  hull_points[1, ]
)

# Projected convex-hull area

polygon_area <- function(x, y) {
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

calculated_convex_hull_area_m2 <-
  polygon_area(
    hull_points$X,
    hull_points$Y
  )

# Crown diameter 2:
# For each hull vertex, find its most distant hull vertex.
# Diameter 2 is the mean of those maximum distances.

hull_distance_matrix <- as.matrix(
  dist(
    hull_points[, c("X", "Y")]
  )
)

farthest_vertex_index <- apply(
  hull_distance_matrix,
  1,
  which.max
)

farthest_distance_m <- apply(
  hull_distance_matrix,
  1,
  max
)

calculated_diameter_2_m <-
  mean(
    farthest_distance_m,
    na.rm = TRUE
  )

# Find the single longest hull-to-hull distance for visualisation

maximum_pair <- which(
  hull_distance_matrix ==
    max(hull_distance_matrix),
  arr.ind = TRUE
)[1, ]

maximum_hull_distance_m <-
  hull_distance_matrix[
    maximum_pair[1],
    maximum_pair[2]
  ]

# Calculate directional crown widths -------------------------------------

direction_angles_degrees <- 0:179
direction_angles_radians <-
  direction_angles_degrees * pi / 180

directional_width_m <- vapply(
  direction_angles_radians,
  function(angle) {
    projected_coordinate <-
      crown_points$X * cos(angle) +
      crown_points$Y * sin(angle)

    diff(
      range(
        projected_coordinate,
        na.rm = TRUE
      )
    )
  },
  numeric(1)
)

directional_width_table <- data.frame(
  angle_degrees =
    direction_angles_degrees,
  directional_width_m =
    directional_width_m
)

# Compare with the existing corrected validation result ------------------

corrected_file <- paste0(
  "outputs/tls/tables/",
  "tls_crown_validation_corrected.csv"
)

batch_calculated_diameter_2_m <- NA_real_

if (file.exists(corrected_file)) {
  corrected_results <- read.csv(
    corrected_file,
    stringsAsFactors = FALSE
  )

  corrected_row <- corrected_results[
    corrected_results$tree_id == tree_id,
  ]

  if (
    nrow(corrected_row) == 1 &&
    "calculated_crown_diameter_2_hull_m" %in%
    names(corrected_row)
  ) {
    batch_calculated_diameter_2_m <-
      corrected_row$
      calculated_crown_diameter_2_hull_m
  }
}

reproduction_difference_m <-
  calculated_diameter_2_m -
  batch_calculated_diameter_2_m

# Prepare the investigation summary --------------------------------------

investigation_summary <- data.frame(
  tree_id = tree_id,
  species = reference_species,
  file_name = basename(wl20_file),
  total_points = nrow(points),
  crown_points = nrow(crown_points),
  convex_hull_vertices = nrow(hull_points),
  calculated_tree_height_m =
    calculated_tree_height_m,
  reference_crown_base_height_m =
    reference_crown_base_m,
  x_extent_m = x_extent_m,
  y_extent_m = y_extent_m,
  calculated_raw_crown_diameter_1_m =
    calculated_raw_diameter_1_m,
  calculated_pca_crown_diameter_1_m =
    calculated_pca_diameter_1_m,
  reference_crown_diameter_1_m =
    reference_diameter_1_m,
  calculated_crown_diameter_2_m =
    calculated_diameter_2_m,
  batch_calculated_crown_diameter_2_m =
    batch_calculated_diameter_2_m,
  reproduction_difference_m =
    reproduction_difference_m,
  reference_crown_diameter_2_m =
    reference_diameter_2_m,
  crown_diameter_2_error_m =
    calculated_diameter_2_m -
    reference_diameter_2_m,
  maximum_hull_distance_m =
    maximum_hull_distance_m,
  minimum_directional_width_m =
    min(directional_width_m),
  mean_directional_width_m =
    mean(directional_width_m),
  maximum_directional_width_m =
    max(directional_width_m),
  calculated_convex_hull_area_m2 =
    calculated_convex_hull_area_m2,
  reference_crown_area_m2 =
    reference_area_m2,
  stringsAsFactors = FALSE
)

# Save tables -------------------------------------------------------------

table_directory <- "outputs/tls/tables"

dir.create(
  table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  investigation_summary,
  file.path(
    table_directory,
    "WL20_crown_diameter_investigation.csv"
  ),
  row.names = FALSE
)

write.csv(
  directional_width_table,
  file.path(
    table_directory,
    "WL20_crown_directional_widths.csv"
  ),
  row.names = FALSE
)

hull_output <- data.frame(
  hull_vertex = seq_len(nrow(hull_points)),
  X = hull_points$X,
  Y = hull_points$Y,
  farthest_vertex = farthest_vertex_index,
  farthest_distance_m = farthest_distance_m
)

write.csv(
  hull_output,
  file.path(
    table_directory,
    "WL20_crown_hull_vertices.csv"
  ),
  row.names = FALSE
)

# Create visual investigation figure -------------------------------------

figure_directory <- "outputs/tls/figures"

dir.create(
  figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_file <- file.path(
  figure_directory,
  "WL20_crown_diameter_investigation.png"
)

set.seed(20)

display_count <- min(
  50000,
  nrow(crown_points)
)

display_rows <- sample(
  seq_len(nrow(crown_points)),
  size = display_count
)

display_points <- crown_points[
  display_rows,
]

png(
  figure_file,
  width = 2200,
  height = 1800,
  res = 220
)

par(
  mfrow = c(2, 2),
  mar = c(4.7, 4.7, 3.5, 1.2)
)

# Panel 1: projected crown and hull

plot(
  display_points$X,
  display_points$Y,
  asp = 1,
  pch = 16,
  cex = 0.22,
  col = rgb(
    0.12,
    0.34,
    0.18,
    0.18
  ),
  xlab = "X coordinate (m)",
  ylab = "Y coordinate (m)",
  main = "WL20 projected crown and convex hull"
)

polygon(
  closed_hull$X,
  closed_hull$Y,
  border = "#D95F02",
  lwd = 2
)

segments(
  hull_points$X[maximum_pair[1]],
  hull_points$Y[maximum_pair[1]],
  hull_points$X[maximum_pair[2]],
  hull_points$Y[maximum_pair[2]],
  col = "#A51C30",
  lwd = 2.5
)

points(
  hull_points$X,
  hull_points$Y,
  pch = 21,
  bg = "#F4A261",
  col = "#A64B00",
  cex = 0.8
)

legend(
  "topright",
  legend = c(
    "Crown points",
    "Convex hull",
    "Maximum hull distance"
  ),
  col = c(
    rgb(0.12, 0.34, 0.18, 0.55),
    "#D95F02",
    "#A51C30"
  ),
  pch = c(16, NA, NA),
  lty = c(NA, 1, 1),
  lwd = c(NA, 2, 2.5),
  bty = "n",
  cex = 0.8
)

# Panel 2: selected farthest-vertex connections

plot(
  hull_points$X,
  hull_points$Y,
  asp = 1,
  type = "n",
  xlab = "X coordinate (m)",
  ylab = "Y coordinate (m)",
  main = "Hull-based diameter measurements"
)

polygon(
  closed_hull$X,
  closed_hull$Y,
  border = "#4C78A8",
  lwd = 2
)

selected_vertices <- unique(
  round(
    seq(
      1,
      nrow(hull_points),
      length.out = min(
        14,
        nrow(hull_points)
      )
    )
  )
)

for (vertex in selected_vertices) {
  farthest_vertex <-
    farthest_vertex_index[vertex]

  segments(
    hull_points$X[vertex],
    hull_points$Y[vertex],
    hull_points$X[farthest_vertex],
    hull_points$Y[farthest_vertex],
    col = rgb(
      0.65,
      0.11,
      0.18,
      0.45
    ),
    lwd = 1.2
  )
}

points(
  hull_points$X,
  hull_points$Y,
  pch = 21,
  bg = "#F4A261",
  col = "#A64B00",
  cex = 0.85
)

# Panel 3: distribution of hull distances

hist(
  farthest_distance_m,
  breaks = "FD",
  col = "#79A879",
  border = "white",
  xlab = "Farthest distance from each hull vertex (m)",
  main = "Distribution used for diameter 2"
)

abline(
  v = calculated_diameter_2_m,
  col = "#1B5E20",
  lwd = 2.5
)

abline(
  v = reference_diameter_2_m,
  col = "#C62828",
  lwd = 2.5,
  lty = 2
)

legend(
  "topright",
  legend = c(
    paste0(
      "Calculated mean: ",
      round(calculated_diameter_2_m, 2),
      " m"
    ),
    paste0(
      "Reference: ",
      round(reference_diameter_2_m, 2),
      " m"
    )
  ),
  col = c(
    "#1B5E20",
    "#C62828"
  ),
  lty = c(1, 2),
  lwd = 2.5,
  bty = "n",
  cex = 0.8
)

# Panel 4: method comparison

method_values <- c(
  calculated_raw_diameter_1_m,
  calculated_pca_diameter_1_m,
  calculated_diameter_2_m,
  reference_diameter_1_m,
  reference_diameter_2_m
)

method_names <- c(
  "Raw XY\nmean",
  "PCA\nmean",
  "Hull\nmean",
  "Reference\ndiameter 1",
  "Reference\ndiameter 2"
)

bar_positions <- barplot(
  method_values,
  names.arg = method_names,
  col = c(
    "#4C78A8",
    "#72A0C1",
    "#347847",
    "#F4A261",
    "#E76F51"
  ),
  border = NA,
  ylim = c(
    0,
    max(method_values) * 1.18
  ),
  ylab = "Crown diameter (m)",
  main = "WL20 crown-width comparison",
  cex.names = 0.75
)

text(
  bar_positions,
  method_values,
  labels = sprintf(
    "%.2f",
    method_values
  ),
  pos = 3,
  cex = 0.75
)

dev.off()

# Print results -----------------------------------------------------------

cat("\nWL20 crown-diameter investigation:\n")

print(
  investigation_summary,
  row.names = FALSE
)

cat(
  "\nCalculated diameter 2:",
  round(calculated_diameter_2_m, 3),
  "m\n"
)

cat(
  "Published diameter 2:",
  round(reference_diameter_2_m, 3),
  "m\n"
)

cat(
  "Difference:",
  round(
    calculated_diameter_2_m -
      reference_diameter_2_m,
    3
  ),
  "m\n"
)

cat(
  "Maximum distance between hull vertices:",
  round(maximum_hull_distance_m, 3),
  "m\n"
)

cat(
  "Directional crown-width range:",
  round(min(directional_width_m), 3),
  "to",
  round(max(directional_width_m), 3),
  "m\n"
)

if (is.finite(reproduction_difference_m)) {
  cat(
    "Difference from the saved batch result:",
    format(
      reproduction_difference_m,
      scientific = TRUE,
      digits = 4
    ),
    "m\n"
  )
}

cat(
  "\nInvestigation figure saved to:",
  figure_file,
  "\n"
)

cat(
  "Investigation tables saved under:",
  table_directory,
  "\n"
)