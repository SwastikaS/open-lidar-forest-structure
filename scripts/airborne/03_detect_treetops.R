library(lidR)
library(terra)
library(sf)

# Load the canopy height model
chm <- rast("outputs/airborne/rasters/chm_1m.tif")

# Smooth local irregularities before detecting canopy peaks
chm_smooth <- focal(
  chm,
  w = matrix(1, 3, 3),
  fun = "mean",
  na.rm = TRUE
)

# Local maximum filter:
# ws = 5 m search window
# hmin = ignore vegetation shorter than 5 m
treetops <- locate_trees(
  chm_smooth,
  algorithm = lmf(ws = 5, hmin = 5)
)

print(treetops)
cat("Detected treetops:", nrow(treetops), "\n")

# Display detected treetops over the CHM
plot(
  chm_smooth,
  main = paste("Detected treetops:", nrow(treetops)),
  col = hcl.colors(30, "YlGn")
)

plot(
  terra::vect(treetops),
  add = TRUE,
  pch = 3,
  col = "red",
  cex = 0.6
)

# Save the results
dir.create("outputs/vectors", showWarnings = FALSE)

st_write(
  treetops,
  "outputs/airborne/vectors/detected_treetops.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

write.csv(
  st_drop_geometry(treetops),
  "outputs/airborne/tables/detected_treetops.csv",
  row.names = FALSE
)



# Compare alternative local-maximum window sizes
window_sizes <- c(3, 5, 7)

treetop_tests <- lapply(
  window_sizes,
  function(window_size) {
    locate_trees(
      chm_smooth,
      algorithm = lmf(ws = window_size, hmin = 5)
    )
  }
)

detection_summary <- data.frame(
  window_size_m = window_sizes,
  detected_treetops = sapply(treetop_tests, nrow),
  detected_treetops_per_ha = sapply(treetop_tests, nrow) / 5.1572
)

print(detection_summary)

write.csv(
  detection_summary,
  "outputs/airborne/tables/treetop_sensitivity.csv",
  row.names = FALSE
)

# Compare the detections visually
par(mfrow = c(1, 3))

for (i in seq_along(window_sizes)) {
  plot(
    chm_smooth,
    main = paste0(window_sizes[i], " m window"),
    col = hcl.colors(30, "YlGn")
  )
  
  plot(
    terra::vect(treetop_tests[[i]]),
    add = TRUE,
    pch = 3,
    col = "red",
    cex = 0.35
  )
}

par(mfrow = c(1, 1))