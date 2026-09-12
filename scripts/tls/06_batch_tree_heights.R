# Batch calculation of tree height from individual TLS point clouds

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

# Locate all TLS point clouds
tls_files <- list.files(
  "data/raw/tls",
  pattern = "[.]laz$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(tls_files) == 0) {
  stop("No TLS .laz files were found under data/raw/tls.")
}

# Create the point-cloud inventory
tree_inventory <- data.frame(
  tree_id = toupper(sub("_.*$", "", basename(tls_files))),
  file_name = basename(tls_files),
  file_path = tls_files,
  file_size_mb = round(file.info(tls_files)$size / 1024^2, 2),
  stringsAsFactors = FALSE
)

# Read the reference measurements
reference <- read.csv(
  "data/reference/tls/Tree Parameters TLS AD WL.csv",
  sep = ";",
  check.names = FALSE
)

# Standardise IDs for matching:
# WL01 becomes WL1 and AD01 becomes AD1
standardise_tree_id <- function(x) {
  x <- toupper(trimws(x))

  sub(
    "^([A-Z]+)0+([0-9]+)$",
    "\\1\\2",
    x
  )
}

tree_inventory$match_id <-
  standardise_tree_id(tree_inventory$tree_id)

reference$match_id <-
  standardise_tree_id(reference$ID)

reference_row <- match(
  tree_inventory$match_id,
  reference$match_id
)

tree_inventory$reference_id <-
  reference$ID[reference_row]

tree_inventory$species <-
  reference$species[reference_row]

tree_inventory$reference_height_m <-
  reference$tree_height[reference_row]

tree_inventory$reference_dbh_cm <-
  reference[["d1.3"]][reference_row] * 100

if (any(is.na(reference_row))) {
  warning(
    sum(is.na(reference_row)),
    " TLS files could not be matched to the reference table."
  )
}

# Prepare columns for calculated measurements
tree_inventory$point_count <- NA_real_
tree_inventory$minimum_z_m <- NA_real_
tree_inventory$maximum_z_m <- NA_real_
tree_inventory$calculated_height_m <- NA_real_
tree_inventory$processing_status <- "not processed"
tree_inventory$error_message <- NA_character_

# Process each tree separately
for (i in seq_len(nrow(tree_inventory))) {

  cat(
    "Processing",
    i,
    "of",
    nrow(tree_inventory),
    ":",
    tree_inventory$tree_id[i],
    "\n"
  )

  result <- tryCatch(
    {
      tls_tree <- readLAS(
        tree_inventory$file_path[i],
        select = "xyz"
      )

      if (is.empty(tls_tree)) {
        stop("Point cloud is empty.")
      }

      minimum_z <- min(tls_tree$Z, na.rm = TRUE)
      maximum_z <- max(tls_tree$Z, na.rm = TRUE)

      list(
        point_count = length(tls_tree$Z),
        minimum_z_m = minimum_z,
        maximum_z_m = maximum_z,
        calculated_height_m = maximum_z - minimum_z,
        status = "success",
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        point_count = NA_real_,
        minimum_z_m = NA_real_,
        maximum_z_m = NA_real_,
        calculated_height_m = NA_real_,
        status = "failed",
        error = conditionMessage(e)
      )
    }
  )

  tree_inventory$point_count[i] <- result$point_count
  tree_inventory$minimum_z_m[i] <- result$minimum_z_m
  tree_inventory$maximum_z_m[i] <- result$maximum_z_m
  tree_inventory$calculated_height_m[i] <-
    result$calculated_height_m
  tree_inventory$processing_status[i] <- result$status
  tree_inventory$error_message[i] <- result$error

  rm(result)

  if (exists("tls_tree")) {
    rm(tls_tree)
  }

  invisible(gc())
}

# Compare calculated and reference heights
tree_inventory$height_error_m <-
  tree_inventory$calculated_height_m -
  tree_inventory$reference_height_m

tree_inventory$absolute_height_error_m <-
  abs(tree_inventory$height_error_m)

valid_height_rows <-
  tree_inventory$processing_status == "success" &
  is.finite(tree_inventory$calculated_height_m) &
  is.finite(tree_inventory$reference_height_m)

height_summary <- data.frame(
  total_files = nrow(tree_inventory),
  successfully_processed =
    sum(tree_inventory$processing_status == "success"),
  failed =
    sum(tree_inventory$processing_status == "failed"),
  trees_with_reference_height = sum(valid_height_rows),
  mean_absolute_error_m =
    mean(
      tree_inventory$absolute_height_error_m[valid_height_rows],
      na.rm = TRUE
    ),
  root_mean_square_error_m =
    sqrt(
      mean(
        tree_inventory$height_error_m[valid_height_rows]^2,
        na.rm = TRUE
      )
    ),
  mean_error_m =
    mean(
      tree_inventory$height_error_m[valid_height_rows],
      na.rm = TRUE
    )
)

# Save the batch results
dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  tree_inventory,
  "outputs/tls/tables/tls_tree_height_batch.csv",
  row.names = FALSE
)

write.csv(
  height_summary,
  "outputs/tls/tables/tls_tree_height_batch_summary.csv",
  row.names = FALSE
)

print(height_summary)

cat(
  "\nBatch results saved under outputs/tls/tables/.\n"
)
