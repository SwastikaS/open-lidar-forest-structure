# Inspect withheld points in the TLS dataset

Sys.setenv(RGL_USE_NULL = "TRUE")
options(rgl.useNULL = TRUE)

library(lidR)

# Output locations -------------------------------------------------------

output_table_directory <-
  "outputs/tls/tables"

output_figure_directory <-
  "outputs/tls/figures"

dir.create(
  output_table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

# Helper functions -------------------------------------------------------

calculate_convex_hull_area <- function(
    x,
    y
) {

  valid <- is.finite(x) &
    is.finite(y)

  xy <- unique(
    data.frame(
      X = x[valid],
      Y = y[valid]
    )
  )

  if (nrow(xy) < 3) {
    return(NA_real_)
  }

  hull_indices <- chull(
    xy$X,
    xy$Y
  )

  hull <- xy[
    hull_indices,
    ,
    drop = FALSE
  ]

  closed_x <- c(
    hull$X,
    hull$X[1]
  )

  closed_y <- c(
    hull$Y,
    hull$Y[1]
  )

  0.5 * abs(
    sum(
      closed_x[-length(closed_x)] *
        closed_y[-1] -
        closed_x[-1] *
        closed_y[-length(closed_y)]
    )
  )
}

calculate_extent <- function(
    values
) {

  values <- values[
    is.finite(values)
  ]

  if (length(values) == 0) {
    return(NA_real_)
  }

  diff(
    range(values)
  )
}

# Locate TLS files -------------------------------------------------------

tls_files <- list.files(
  "data/raw/tls",
  pattern = "[.]laz$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(tls_files) == 0) {
  stop(
    "No TLS .laz files were found under data/raw/tls."
  )
}

tree_inventory <- data.frame(
  tree_id = toupper(
    sub(
      "_.*$",
      "",
      basename(tls_files)
    )
  ),
  file_name = basename(tls_files),
  file_path = tls_files,
  stringsAsFactors = FALSE
)

# Inspect each file ------------------------------------------------------

audit_results <- vector(
  "list",
  nrow(tree_inventory)
)

for (i in seq_len(nrow(tree_inventory))) {

  cat(
    "Inspecting",
    i,
    "of",
    nrow(tree_inventory),
    ":",
    tree_inventory$tree_id[i],
    "\n"
  )

  result <- tryCatch(
    {

      # The letter w requests the LAS withheld flag.
      # XYZ coordinates are also loaded for the comparison.

      tls_tree <- suppressWarnings(
        readLAS(
          tree_inventory$file_path[i],
          select = "xyzw"
        )
      )

      if (is.empty(tls_tree)) {
        stop(
          "Point cloud is empty."
        )
      }

      point_data <- as.data.frame(
        tls_tree@data
      )

      required_columns <- c(
        "X",
        "Y",
        "Z"
      )

      missing_columns <- setdiff(
        required_columns,
        names(point_data)
      )

      if (length(missing_columns) > 0) {
        stop(
          "Missing coordinate columns: ",
          paste(
            missing_columns,
            collapse = ", "
          )
        )
      }

      if (
        "Withheld_flag" %in%
        names(point_data)
      ) {

        withheld_flag <- as.logical(
          point_data$Withheld_flag
        )

        withheld_flag[
          is.na(withheld_flag)
        ] <- FALSE

      } else {

        withheld_flag <- rep(
          FALSE,
          nrow(point_data)
        )
      }

      total_points <- nrow(
        point_data
      )

      withheld_points <- sum(
        withheld_flag
      )

      retained_points <-
        total_points -
        withheld_points

      withheld_percent <- if (
        total_points > 0
      ) {
        withheld_points /
          total_points *
          100
      } else {
        NA_real_
      }

      retained_data <- point_data[
        !withheld_flag,
        c(
          "X",
          "Y",
          "Z"
        ),
        drop = FALSE
      ]

      all_data <- point_data[
        ,
        c(
          "X",
          "Y",
          "Z"
        ),
        drop = FALSE
      ]

      all_height_m <-
        calculate_extent(
          all_data$Z
        )

      retained_height_m <-
        calculate_extent(
          retained_data$Z
        )

      all_x_extent_m <-
        calculate_extent(
          all_data$X
        )

      retained_x_extent_m <-
        calculate_extent(
          retained_data$X
        )

      all_y_extent_m <-
        calculate_extent(
          all_data$Y
        )

      retained_y_extent_m <-
        calculate_extent(
          retained_data$Y
        )

      # Convex-hull area is calculated only when withheld
      # points exist, because this is the comparison of interest.

      if (
        withheld_points > 0 &&
        nrow(retained_data) >= 3
      ) {

        all_convex_hull_area_m2 <-
          calculate_convex_hull_area(
            all_data$X,
            all_data$Y
          )

        retained_convex_hull_area_m2 <-
          calculate_convex_hull_area(
            retained_data$X,
            retained_data$Y
          )

      } else {

        all_convex_hull_area_m2 <-
          NA_real_

        retained_convex_hull_area_m2 <-
          NA_real_
      }

      data.frame(
        tree_id =
          tree_inventory$tree_id[i],

        file_name =
          tree_inventory$file_name[i],

        total_points =
          total_points,

        withheld_points =
          withheld_points,

        retained_points =
          retained_points,

        withheld_percent =
          withheld_percent,

        all_height_m =
          all_height_m,

        retained_height_m =
          retained_height_m,

        height_change_m =
          retained_height_m -
          all_height_m,

        all_x_extent_m =
          all_x_extent_m,

        retained_x_extent_m =
          retained_x_extent_m,

        x_extent_change_m =
          retained_x_extent_m -
          all_x_extent_m,

        all_y_extent_m =
          all_y_extent_m,

        retained_y_extent_m =
          retained_y_extent_m,

        y_extent_change_m =
          retained_y_extent_m -
          all_y_extent_m,

        all_convex_hull_area_m2 =
          all_convex_hull_area_m2,

        retained_convex_hull_area_m2 =
          retained_convex_hull_area_m2,

        convex_hull_area_change_m2 =
          retained_convex_hull_area_m2 -
          all_convex_hull_area_m2,

        processing_status =
          "success",

        error_message =
          NA_character_,

        stringsAsFactors = FALSE
      )
    },
    error = function(e) {

      data.frame(
        tree_id =
          tree_inventory$tree_id[i],

        file_name =
          tree_inventory$file_name[i],

        total_points =
          NA_integer_,

        withheld_points =
          NA_integer_,

        retained_points =
          NA_integer_,

        withheld_percent =
          NA_real_,

        all_height_m =
          NA_real_,

        retained_height_m =
          NA_real_,

        height_change_m =
          NA_real_,

        all_x_extent_m =
          NA_real_,

        retained_x_extent_m =
          NA_real_,

        x_extent_change_m =
          NA_real_,

        all_y_extent_m =
          NA_real_,

        retained_y_extent_m =
          NA_real_,

        y_extent_change_m =
          NA_real_,

        all_convex_hull_area_m2 =
          NA_real_,

        retained_convex_hull_area_m2 =
          NA_real_,

        convex_hull_area_change_m2 =
          NA_real_,

        processing_status =
          "failed",

        error_message =
          conditionMessage(e),

        stringsAsFactors = FALSE
      )
    }
  )

  audit_results[[i]] <- result

  rm(result)

  if (exists("tls_tree")) {
    rm(tls_tree)
  }

  if (exists("point_data")) {
    rm(point_data)
  }

  if (exists("all_data")) {
    rm(all_data)
  }

  if (exists("retained_data")) {
    rm(retained_data)
  }

  invisible(gc())
}

audit_results <- do.call(
  rbind,
  audit_results
)

row.names(audit_results) <- NULL

# Select files containing withheld points --------------------------------

affected_files <- audit_results[
  audit_results$processing_status ==
    "success" &
    is.finite(
      audit_results$withheld_points
    ) &
    audit_results$withheld_points > 0,
  ,
  drop = FALSE
]

affected_files <- affected_files[
  order(
    affected_files$withheld_percent,
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]

row.names(affected_files) <- NULL

# Create summary ---------------------------------------------------------

audit_summary <- data.frame(
  total_files =
    nrow(audit_results),

  successfully_inspected =
    sum(
      audit_results$processing_status ==
        "success"
    ),

  failed_files =
    sum(
      audit_results$processing_status ==
        "failed"
    ),

  files_with_withheld_points =
    nrow(affected_files),

  total_points =
    sum(
      audit_results$total_points,
      na.rm = TRUE
    ),

  total_withheld_points =
    sum(
      audit_results$withheld_points,
      na.rm = TRUE
    ),

  overall_withheld_percent =
    sum(
      audit_results$withheld_points,
      na.rm = TRUE
    ) /
    sum(
      audit_results$total_points,
      na.rm = TRUE
    ) *
    100,

  stringsAsFactors = FALSE
)

# Save tables ------------------------------------------------------------

audit_output_file <- file.path(
  output_table_directory,
  "tls_withheld_point_audit.csv"
)

summary_output_file <- file.path(
  output_table_directory,
  "tls_withheld_point_audit_summary.csv"
)

write.csv(
  audit_results,
  audit_output_file,
  row.names = FALSE
)

write.csv(
  audit_summary,
  summary_output_file,
  row.names = FALSE
)

# Create figure ----------------------------------------------------------

figure_output_file <- file.path(
  output_figure_directory,
  "tls_withheld_point_audit.png"
)

png(
  figure_output_file,
  width = 2200,
  height = 1300,
  res = 220
)

if (nrow(affected_files) == 0) {

  plot.new()

  text(
    0.5,
    0.5,
    "No withheld points were found",
    cex = 1.5
  )

} else {

  par(
    mfrow = c(1, 2),
    mar = c(8, 5, 3.5, 1)
  )

  barplot(
    affected_files$withheld_percent,
    names.arg =
      affected_files$tree_id,
    las = 2,
    col = "#D55E00",
    ylab = "Withheld points (%)",
    main = "Withheld points by tree"
  )

  area_change <- affected_files[
    is.finite(
      affected_files$convex_hull_area_change_m2
    ),
    ,
    drop = FALSE
  ]

  if (nrow(area_change) > 0) {

    barplot(
      area_change$convex_hull_area_change_m2,
      names.arg =
        area_change$tree_id,
      las = 2,
      col = "#355C7D",
      ylab = "Change after removal (m²)",
      main = "Change in convex-hull area"
    )

    abline(
      h = 0,
      col = "grey35",
      lty = 2
    )

  } else {

    plot.new()

    text(
      0.5,
      0.5,
      "No convex-hull comparisons available",
      cex = 1.2
    )
  }
}

dev.off()

# Print results ----------------------------------------------------------

cat("\nWithheld-point audit summary:\n")

print(
  audit_summary,
  row.names = FALSE
)

if (nrow(affected_files) > 0) {

  cat(
    "\nFiles containing withheld points:\n"
  )

  print(
    affected_files[
      ,
      c(
        "tree_id",
        "total_points",
        "withheld_points",
        "retained_points",
        "withheld_percent",
        "height_change_m",
        "x_extent_change_m",
        "y_extent_change_m",
        "convex_hull_area_change_m2"
      )
    ],
    row.names = FALSE
  )

} else {

  cat(
    "\nNo files contained withheld points.\n"
  )
}

failed_files <- audit_results[
  audit_results$processing_status ==
    "failed",
  c(
    "tree_id",
    "file_name",
    "error_message"
  ),
  drop = FALSE
]

if (nrow(failed_files) > 0) {

  cat(
    "\nFiles that could not be inspected:\n"
  )

  print(
    failed_files,
    row.names = FALSE
  )
}

cat(
  "\nAudit table saved to:",
  audit_output_file,
  "\n"
)

cat(
  "Audit summary saved to:",
  summary_output_file,
  "\n"
)

cat(
  "Audit figure saved to:",
  figure_output_file,
  "\n"
)