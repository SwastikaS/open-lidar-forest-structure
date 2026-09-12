# Export publication-ready figures used by the project README.
# Run scripts 01-05 first so that the required rasters and vectors exist.

library(lidR)
library(terra)
library(sf)
library(ggplot2)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

theme_project <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    legend.position = "right"
  )

# 1. Point-cloud overview ------------------------------------------------------
las <- readLAS("data/raw/Megaplot.laz")
point_data <- as.data.frame(las@data)[, c("X", "Y", "Z")]

set.seed(42)
if (nrow(point_data) > 60000) {
  point_data <- point_data[sample(nrow(point_data), 60000), ]
}

p_cloud <- ggplot(point_data, aes(X, Y, colour = Z)) +
  geom_point(size = 0.18, alpha = 0.75) +
  scale_colour_viridis_c(option = "C", name = "Height (m)") +
  coord_equal() +
  labs(
    title = "Airborne LiDAR point cloud",
    subtitle = "Megaplot sample supplied with the lidR package",
    x = "Easting (m)", y = "Northing (m)"
  ) +
  theme_project

ggsave(
  "outputs/figures/01_point_cloud_overview.png",
  p_cloud, width = 12, height = 7, dpi = 180
)

# 2. Canopy metrics ------------------------------------------------------------
metric_table <- read.csv("outputs/tables/biomass_predictors_20m.csv")

metric_long <- rbind(
  transform(metric_table[, c("x", "y")], metric = "Mean height", value = metric_table$mean_height_m),
  transform(metric_table[, c("x", "y")], metric = "P95 height", value = metric_table$p95_height_m)
)

p_metrics <- ggplot(metric_long, aes(x, y, fill = value)) +
  geom_tile(width = 20, height = 20) +
  facet_wrap(~metric) +
  scale_fill_viridis_c(option = "C", name = "Metres") +
  coord_equal() +
  labs(
    title = "Canopy-height metrics at 20 m resolution",
    x = "Easting (m)", y = "Northing (m)"
  ) +
  theme_project

ggsave(
  "outputs/figures/02_canopy_metrics.png",
  p_metrics, width = 12, height = 7, dpi = 180
)

# 3. Treetops and segmented crowns --------------------------------------------
crowns <- st_read("outputs/vectors/tree_crowns.gpkg", quiet = TRUE)
treetops <- st_read("outputs/vectors/detected_treetops.gpkg", quiet = TRUE)

p_crowns <- ggplot() +
  geom_sf(data = crowns, aes(fill = tree_height_m), colour = "white", linewidth = 0.12) +
  geom_sf(data = treetops, shape = 3, colour = "#d73027", size = 0.55, linewidth = 0.35) +
  scale_fill_viridis_c(option = "C", name = "Tree height (m)") +
  labs(
    title = "Detected treetops and delineated crowns",
    subtitle = "Local-maximum filtering and Dalponte–Coomes crown segmentation"
  ) +
  theme_project +
  theme(axis.title = element_blank())

ggsave(
  "outputs/figures/03_treetops_and_crowns.png",
  p_crowns, width = 12, height = 7, dpi = 180
)

# 4. Biomass-related structural predictors ------------------------------------
predictor_long <- rbind(
  transform(metric_table[, c("x", "y")], metric = "Mean height (m)", value = metric_table$mean_height_m),
  transform(metric_table[, c("x", "y")], metric = "P95 height (m)", value = metric_table$p95_height_m),
  transform(metric_table[, c("x", "y")], metric = "Returns above 2 m (%)", value = metric_table$returns_above_2m_percent),
  transform(metric_table[, c("x", "y")], metric = "Canopy relief ratio", value = metric_table$canopy_relief_ratio)
)

p_predictors <- ggplot(predictor_long, aes(x, y, fill = value)) +
  geom_tile(width = 20, height = 20) +
  facet_wrap(~metric, scales = "free") +
  scale_fill_viridis_c(option = "C", name = "Value") +
  coord_equal() +
  labs(
    title = "Biomass-related structural predictors",
    subtitle = "LiDAR-derived indicators at 20 m resolution",
    x = "Easting (m)", y = "Northing (m)"
  ) +
  theme_project

ggsave(
  "outputs/figures/04_structural_predictors.png",
  p_predictors, width = 12, height = 8, dpi = 180
)

# 5. Social/link-preview image -------------------------------------------------
p_social <- ggplot() +
  geom_sf(
    data = crowns,
    aes(fill = tree_height_m),
    colour = scales::alpha("white", 0.25),
    linewidth = 0.1
  ) +
  geom_sf(
    data = treetops,
    shape = 3,
    colour = "#ff4c3b",
    size = 0.35,
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  labs(
    title = "OPEN LiDAR FOREST STRUCTURE",
    subtitle = "Canopy metrics  •  treetop detection  •  crown segmentation"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#0d171d", colour = NA),
    panel.background = element_rect(fill = "#0d171d", colour = NA),
    plot.title = element_text(colour = "white", face = "bold", size = 22),
    plot.subtitle = element_text(colour = "#b9c5cb", size = 12),
    plot.margin = margin(24, 28, 24, 28)
  )

ggsave(
  "outputs/figures/social_preview.png",
  p_social, width = 12, height = 6.3, dpi = 100,
  bg = "#0d171d"
)
