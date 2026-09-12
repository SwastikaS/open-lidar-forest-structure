library(lidR)
library(terra)

# macOS Tahoe compatibility
Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

# Read the height-normalised point cloud
las <- readLAS("data/raw/airborne/Megaplot.laz")

# Whole-site canopy metrics
site_metrics <- cloud_metrics(
  las,
  ~list(
    point_count = length(Z),
    mean_height_m = mean(Z),
    sd_height_m = sd(Z),
    median_height_m = median(Z),
    p75_height_m = unname(quantile(Z, 0.75)),
    p95_height_m = unname(quantile(Z, 0.95)),
    p98_height_m = unname(quantile(Z, 0.98)),
    maximum_height_m = max(Z),
    returns_above_2m_percent = mean(Z > 2) * 100
  )
)

print(site_metrics)

# Calculate metrics within 20 × 20 m grid cells
canopy_metrics_20m <- pixel_metrics(
  las,
  ~list(
    mean_height = mean(Z),
    p95_height = unname(quantile(Z, 0.95)),
    maximum_height = max(Z),
    returns_above_2m = mean(Z > 2) * 100
  ),
  res = 20
)

# Display the four raster layers
plot(canopy_metrics_20m)

# Save outputs
writeRaster(
  canopy_metrics_20m,
  "outputs/airborne/rasters/canopy_metrics_20m.tif",
  overwrite = TRUE
)

write.csv(
  as.data.frame(site_metrics),
  "outputs/airborne/tables/site_canopy_metrics.csv",
  row.names = FALSE
)


# Generate a 1 m canopy height model using point-to-raster interpolation
chm_1m <- rasterize_canopy(
  las,
  res = 1,
  algorithm = p2r(subcircle = 0.2)
)

# Prevent any small negative values
chm_1m[chm_1m < 0] <- 0

# Fill isolated empty pixels using neighbouring values
chm_1m_filled <- terra::focal(
  chm_1m,
  w = 3,
  fun = "mean",
  na.policy = "only",
  na.rm = TRUE
)

# Visualise original and gap-filled CHMs
par(mfrow = c(1, 2))

plot(
  chm_1m,
  main = "Original 1 m canopy height model",
  col = hcl.colors(30, "YlGn")
)

plot(
  chm_1m_filled,
  main = "Gap-filled 1 m canopy height model",
  col = hcl.colors(30, "YlGn")
)

par(mfrow = c(1, 1))

writeRaster(
  chm_1m_filled,
  "outputs/airborne/rasters/chm_1m.tif",
  overwrite = TRUE
)