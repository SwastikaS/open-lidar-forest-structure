library(lidR)
library(terra)
library(sf)

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

# Load data
las <- readLAS("data/raw/Megaplot.laz")
chm <- rast("outputs/rasters/chm_1m.tif")

# Smooth the CHM
chm_smooth <- focal(
  chm,
  w = matrix(1, 3, 3),
  fun = "mean",
  na.rm = TRUE
)

# Retain the selected 5 m treetop-detection window
treetops <- locate_trees(
  chm_smooth,
  lmf(ws = 5, hmin = 5)
)

# Assign point-cloud returns to individual crowns
segmented_las <- segment_trees(
  las,
  dalponte2016(
    chm = chm_smooth,
    treetops = treetops,
    th_tree = 5,
    th_seed = 0.45,
    th_cr = 0.55,
    max_cr = 10
  )
)

# Construct crown polygons and calculate tree-level metrics
crowns <- crown_metrics(
  segmented_las,
  ~list(
    tree_height_m = max(Z),
    mean_return_height_m = mean(Z),
    point_count = length(Z)
  ),
  geom = "convex"
)

crowns$crown_area_m2 <- as.numeric(st_area(crowns))

print(crowns)
cat("Delineated crowns:", nrow(crowns), "\n")

# Display crown boundaries
plot(
  chm_smooth,
  main = paste("Individual tree crowns:", nrow(crowns)),
  col = hcl.colors(30, "YlGn")
)

plot(
  st_geometry(crowns),
  add = TRUE,
  border = "red",
  lwd = 0.6
)

plot(
  st_geometry(treetops),
  add = TRUE,
  pch = 3,
  col = "blue",
  cex = 0.35
)

# Save spatial and tabular outputs
dir.create("outputs/vectors", showWarnings = FALSE)

st_write(
  crowns,
  "outputs/vectors/tree_crowns.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

crown_table <- st_drop_geometry(crowns)

write.csv(
  crown_table,
  "outputs/tables/tree_crown_metrics.csv",
  row.names = FALSE
)







# Derive structural indicators from crown geometry and height

# Diameter of a circle having the same area as each crown
crowns$equivalent_crown_diameter_m <-
  2 * sqrt(crowns$crown_area_m2 / pi)

# Three-dimensional crown-size proxy
# This is not woody volume or biomass
crowns$crown_volume_proxy_m3 <-
  crowns$crown_area_m2 * crowns$tree_height_m

# Summarise individual-crown structure
crown_summary <- data.frame(
  delineated_crowns = nrow(crowns),
  mean_tree_height_m = mean(crowns$tree_height_m, na.rm = TRUE),
  median_tree_height_m = median(crowns$tree_height_m, na.rm = TRUE),
  mean_crown_area_m2 = mean(crowns$crown_area_m2, na.rm = TRUE),
  median_crown_area_m2 = median(crowns$crown_area_m2, na.rm = TRUE),
  mean_crown_diameter_m =
    mean(crowns$equivalent_crown_diameter_m, na.rm = TRUE),
  mean_crown_volume_proxy_m3 =
    mean(crowns$crown_volume_proxy_m3, na.rm = TRUE)
)

print(crown_summary)

# Rewrite outputs with the additional fields
st_write(
  crowns,
  "outputs/vectors/tree_crowns.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

write.csv(
  st_drop_geometry(crowns),
  "outputs/tables/tree_crown_metrics.csv",
  row.names = FALSE
)

write.csv(
  crown_summary,
  "outputs/tables/crown_structure_summary.csv",
  row.names = FALSE
)