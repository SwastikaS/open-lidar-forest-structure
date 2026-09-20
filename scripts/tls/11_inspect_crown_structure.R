# Inspect the vertical crown structure of WL12

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

# Read the reference measurements ---------------------------------------

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

# Read the TLS point cloud -----------------------------------------------

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

# Divide the tree into 20 cm vertical sections ---------------------------

bin_width_m <- 0.20

points$height_bin <- floor(
  points$height_m / bin_width_m
)

bin_indices <- split(
  seq_len(nrow(points)),
  points$height_bin
)

vertical_profile <- do.call(
  rbind,
  lapply(
    bin_indices,
    function(index) {

      section <- points[
        index,
        ,
        drop = FALSE
      ]

      x_limits <- quantile(
        section$X,
        probabilities = c(0.01, 0.99),
        na.rm = TRUE,
        names = FALSE
      )

      y_limits <- quantile(
        section$Y,
        probabilities = c(0.01, 0.99),
        na.rm = TRUE,
        names = FALSE
      )

      data.frame(
        height_lower_m =
          min(section$height_bin) *
          bin_width_m,
        height_upper_m =
          (
            min(section$height_bin) + 1
          ) *
          bin_width_m,
        height_midpoint_m =
          (
            min(section$height_bin) + 0.5
          ) *
          bin_width_m,
        point_count = nrow(section),
        robust_x_width_m =
          x_limits[2] - x_limits[1],
        robust_y_width_m =
          y_limits[2] - y_limits[1],
        mean_robust_width_m =
          mean(
            c(
              x_limits[2] - x_limits[1],
              y_limits[2] - y_limits[1]
            )
          )
      )
    }
  )
)

row.names(vertical_profile) <- NULL

vertical_profile <- vertical_profile[
  order(vertical_profile$height_midpoint_m),
]

# Create output folders --------------------------------------------------

dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/tls/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save the vertical profile ----------------------------------------------

profile_output_file <- paste0(
  "outputs/tls/tables/",
  "WL12_crown_vertical_profile.csv"
)

write.csv(
  vertical_profile,
  profile_output_file,
  row.names = FALSE
)

# Select points for plotting --------------------------------------------

set.seed(42)

maximum_plot_points <- 100000

if (nrow(points) > maximum_plot_points) {
  plot_indices <- sample(
    seq_len(nrow(points)),
    maximum_plot_points
  )
} else {
  plot_indices <- seq_len(nrow(points))
}

plot_points <- points[
  plot_indices,
  ,
  drop = FALSE
]

# Create the crown-profile figure ---------------------------------------

figure_output_file <- paste0(
  "outputs/tls/figures/",
  "WL12_crown_vertical_profile.png"
)

png(
  figure_output_file,
  width = 2400,
  height = 1400,
  res = 220
)

par(
  mfrow = c(1, 3),
  mar = c(4.5, 4.5, 3, 1)
)

# Front projection

plot(
  plot_points$X,
  plot_points$height_m,
  pch = 16,
  cex = 0.15,
  col = rgb(0.10, 0.25, 0.15, 0.18),
  xlab = "X coordinate (m)",
  ylab = "Height above tree base (m)",
  main = "WL12 front projection"
)

abline(
  h = reference_crown_base_height_m,
  col = "#D55E00",
  lwd = 2,
  lty = 2
)

legend(
  "topright",
  legend = "Reference crown base",
  col = "#D55E00",
  lty = 2,
  lwd = 2,
  bty = "n",
  cex = 0.8
)

# Horizontal width profile

plot(
  vertical_profile$mean_robust_width_m,
  vertical_profile$height_midpoint_m,
  type = "l",
  lwd = 2,
  col = "#355C7D",
  xlab = "Mean robust horizontal width (m)",
  ylab = "Height above tree base (m)",
  main = "Width by height"
)

lines(
  vertical_profile$robust_x_width_m,
  vertical_profile$height_midpoint_m,
  col = "#2A9D8F",
  lwd = 1.5
)

lines(
  vertical_profile$robust_y_width_m,
  vertical_profile$height_midpoint_m,
  col = "#8E5EA2",
  lwd = 1.5
)

abline(
  h = reference_crown_base_height_m,
  col = "#D55E00",
  lwd = 2,
  lty = 2
)

legend(
  "bottomright",
  legend = c(
    "Mean width",
    "X width",
    "Y width",
    "Reference crown base"
  ),
  col = c(
    "#355C7D",
    "#2A9D8F",
    "#8E5EA2",
    "#D55E00"
  ),
  lty = c(1, 1, 1, 2),
  lwd = c(2, 1.5, 1.5, 2),
  bty = "n",
  cex = 0.75
)

# Point count profile

plot(
  vertical_profile$point_count,
  vertical_profile$height_midpoint_m,
  type = "l",
  lwd = 2,
  log = "x",
  col = "#347847",
  xlab = "Points per 20 cm section (log scale)",
  ylab = "Height above tree base (m)",
  main = "Point density by height"
)

abline(
  h = reference_crown_base_height_m,
  col = "#D55E00",
  lwd = 2,
  lty = 2
)

dev.off()

# Print a concise summary ------------------------------------------------

crown_reference_summary <- data.frame(
  tree_id = "WL12",
  species = wl12_reference$species,
  calculated_tree_height_m =
    calculated_tree_height_m,
  reference_tree_height_m =
    reference_tree_height_m,
  reference_crown_base_height_m =
    reference_crown_base_height_m,
  reference_crown_length_m =
    reference_crown_length_m,
  reference_crown_diameter_1_m =
    reference_crown_diameter_1_m,
  reference_crown_diameter_2_m =
    reference_crown_diameter_2_m,
  reference_crown_area_m2 =
    reference_crown_area_m2
)

cat("\nWL12 crown reference summary:\n")
print(
  crown_reference_summary,
  row.names = FALSE
)

cat(
  "\nVertical sections:",
  nrow(vertical_profile),
  "\n"
)

cat(
  "Profile table saved to:",
  profile_output_file,
  "\n"
)

cat(
  "Profile figure saved to:",
  figure_output_file,
  "\n"
)