library(lidR)

# macOS Tahoe graphics compatibility
Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

# Input TLS point cloud
tls_file <- paste0(
  "data/raw/tls/site1_WL_group2/",
  "WL12_FagSyl_2020-12-04.laz"
)

if (!file.exists(tls_file)) {
  stop("The WL12 TLS point cloud could not be found.")
}

# Read the individual-tree point cloud
tls_tree <- readLAS(tls_file)

if (is.empty(tls_tree)) {
  stop("The TLS file was loaded but contains no points.")
}

print(tls_tree)
summary(tls_tree)

# Extract coordinates
tree_points <- as.data.frame(tls_tree@data)[, c("X", "Y", "Z")]

# Calculate point-cloud dimensions
tree_dimensions <- data.frame(
  point_count = nrow(tree_points),
  x_span_m = diff(range(tree_points$X, na.rm = TRUE)),
  y_span_m = diff(range(tree_points$Y, na.rm = TRUE)),
  z_span_m = diff(range(tree_points$Z, na.rm = TRUE)),
  minimum_z_m = min(tree_points$Z, na.rm = TRUE),
  maximum_z_m = max(tree_points$Z, na.rm = TRUE)
)

print(tree_dimensions)

# Subsample only for faster visualisation
set.seed(42)

display_points <- tree_points[
  sample(
    seq_len(nrow(tree_points)),
    min(150000, nrow(tree_points))
  ),
]

point_colour <- adjustcolor("darkgreen", alpha.f = 0.25)

plot_tls_projections <- function(points) {
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

  plot(
    points$X,
    points$Y,
    asp = 1,
    pch = 16,
    cex = 0.15,
    col = point_colour,
    xlab = "X (m)",
    ylab = "Y (m)",
    main = "Top view (XY)"
  )

  plot(
    points$X,
    points$Z,
    asp = 1,
    pch = 16,
    cex = 0.15,
    col = point_colour,
    xlab = "X (m)",
    ylab = "Z (m)",
    main = "Side view (XZ)"
  )

  plot(
    points$Y,
    points$Z,
    asp = 1,
    pch = 16,
    cex = 0.15,
    col = point_colour,
    xlab = "Y (m)",
    ylab = "Z (m)",
    main = "Side view (YZ)"
  )

  par(mfrow = c(1, 1))
}

# Save a GitHub-ready figure
png(
  "outputs/tls/figures/WL12_TLS_projections.png",
  width = 2400,
  height = 800,
  res = 150
)

plot_tls_projections(display_points)
dev.off()

# Display in RStudio
plot_tls_projections(display_points)