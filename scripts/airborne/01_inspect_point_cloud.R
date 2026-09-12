# Use browser-based WebGL because macOS Tahoe does not support rgl OpenGL
Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

# Locate the example forest point cloud supplied with lidR
source_file <- system.file(
  "extdata",
  "Megaplot.laz",
  package = "lidR"
)

if (source_file == "") {
  stop("The example point cloud could not be located.")
}

# Copy it into the local raw-data folder
destination <- "data/raw/airborne/Megaplot.laz"

if (!file.exists(destination)) {
  file.copy(source_file, destination)
}

# Read the point cloud
las <- readLAS(destination)

if (is.empty(las)) {
  stop("The point cloud was loaded but contains no points.")
}

# Inspect its contents
print(las)
summary(las)

# Open an interactive 3D view
#plot(las, color = "Z")
#rgl::rglwidget()

# Inspect the available attributes
print(names(las))
print(head(las@data))

# Count points belonging to each classification
print(table(las$Classification, useNA = "ifany"))

# Examine the recorded laser returns
print(table(las$ReturnNumber))
print(table(las$NumberOfReturns))

# Run lidR's integrity checks
#las_check(las)

table(las$Classification, useNA = "ifany")
table(las$ReturnNumber)
table(las$NumberOfReturns)
