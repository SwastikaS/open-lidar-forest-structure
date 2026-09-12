library(lidR)
library(terra)

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

las <- readLAS("data/raw/airborne/Megaplot.laz")

if (is.empty(las)) {
  stop("The point cloud could not be loaded.")
}

# Calculate structural predictors in 20 × 20 m cells
biomass_predictors_20m <- pixel_metrics(
  las,
  ~list(
    point_count = length(Z),
    mean_height_m = mean(Z),
    sd_height_m = sd(Z),
    p25_height_m = unname(quantile(Z, 0.25)),
    p50_height_m = unname(quantile(Z, 0.50)),
    p75_height_m = unname(quantile(Z, 0.75)),
    p90_height_m = unname(quantile(Z, 0.90)),
    p95_height_m = unname(quantile(Z, 0.95)),
    p98_height_m = unname(quantile(Z, 0.98)),
    maximum_height_m = max(Z),
    returns_above_2m_percent = mean(Z > 2) * 100,
    returns_above_mean_percent = mean(Z > mean(Z)) * 100,
    canopy_relief_ratio =
      (mean(Z) - min(Z)) / (max(Z) - min(Z))
  ),
  res = 20
)

print(biomass_predictors_20m)

# Layers 2, 8, 11 and 13:
# mean height, P95 height, returns above 2 m and canopy relief ratio
selected_predictors <- biomass_predictors_20m[[c(2, 8, 11, 13)]]

names(selected_predictors) <- c(
  "Mean height",
  "P95 height",
  "Returns above 2 m",
  "Canopy relief ratio"
)

plot(
  selected_predictors,
  col = hcl.colors(30, "YlGn")
)
     
# Save the multiband predictor raster
writeRaster(
  biomass_predictors_20m,
  "outputs/airborne/rasters/biomass_predictors_20m.tif",
  overwrite = TRUE
)

# Export each valid grid cell as a table row
predictor_table <- as.data.frame(
  biomass_predictors_20m,
  xy = TRUE,
  na.rm = TRUE
)

write.csv(
  predictor_table,
  "outputs/airborne/tables/biomass_predictors_20m.csv",
  row.names = FALSE
)

cat("Valid 20 m cells:", nrow(predictor_table), "\n")