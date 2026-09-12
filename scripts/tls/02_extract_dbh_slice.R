library(lidR)

tls_file <- paste0(
  "data/raw/tls/site1_WL_group2/",
  "WL12_FagSyl_2020-12-04.laz"
)

tls_tree <- readLAS(tls_file)

if (is.empty(tls_tree)) {
  stop("The TLS point cloud is empty.")
}

tree_points <- as.data.frame(tls_tree@data)[, c("X", "Y", "Z")]

# Convert local Z to height above the lowest tree point
tree_base_z <- min(tree_points$Z, na.rm = TRUE)
tree_points$height_above_base_m <- tree_points$Z - tree_base_z

cat("Tree base Z:", tree_base_z, "m\n")
cat(
  "Calculated tree height:",
  max(tree_points$height_above_base_m),
  "m\n"
)

# Extract a 10 cm-thick slice centred at breast height
dbh_slice <- tree_points[
  tree_points$height_above_base_m >= 1.25 &
    tree_points$height_above_base_m <= 1.35,
]

cat("Points in DBH slice:", nrow(dbh_slice), "\n")

# Inspect the horizontal stem cross-section
plot(
  dbh_slice$X,
  dbh_slice$Y,
  asp = 1,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("steelblue4", alpha.f = 0.4),
  xlab = "X (m)",
  ylab = "Y (m)",
  main = "WL12 stem cross-section at 1.3 m"
)

# Save the DBH cross-section
png(
  "outputs/figures/tls/WL12_DBH_slice_raw.png",
  width = 1200,
  height = 1200,
  res = 180
)

plot(
  dbh_slice$X,
  dbh_slice$Y,
  asp = 1,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("steelblue4", alpha.f = 0.4),
  xlab = "X (m)",
  ylab = "Y (m)",
  main = "WL12 stem cross-section at 1.3 m"
)

dev.off()

write.csv(
  dbh_slice,
  "data/processed/tls/WL12_DBH_slice_raw.csv",
  row.names = FALSE
)