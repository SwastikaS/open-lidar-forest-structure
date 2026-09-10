# Use browser-based WebGL because macOS Tahoe does not support rgl OpenGL
Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

# Core packages for the airborne LiDAR workflow

required_packages <- c(
  "lidR",
  "terra",
  "sf",
  "data.table",
  "ggplot2"
)

new_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

message("LiDAR working environment is ready.")