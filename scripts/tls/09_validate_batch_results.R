# Validate batch TLS DBH estimates

# Read batch results
batch_results <- read.csv(
  "outputs/tls/tables/tls_dbh_batch_results.csv",
  stringsAsFactors = FALSE
)

# Keep successful estimates
successful_results <- batch_results[
  batch_results$processing_status == "success" &
    is.finite(batch_results$estimated_dbh_cm) &
    is.finite(batch_results$reference_dbh_cm),
]

# Calculate accuracy for each quality category
quality_groups <- split(
  successful_results,
  successful_results$quality_flag
)

accuracy_by_quality <- do.call(
  rbind,
  lapply(
    names(quality_groups),
    function(group_name) {

      group <- quality_groups[[group_name]]

      data.frame(
        quality_flag = group_name,
        trees = nrow(group),
        mean_error_cm = mean(group$error_cm),
        mean_absolute_error_cm =
          mean(group$absolute_error_cm),
        root_mean_square_error_cm =
          sqrt(mean(group$error_cm^2)),
        within_1_cm_percent =
          mean(group$absolute_error_cm <= 1) * 100,
        within_2_cm_percent =
          mean(group$absolute_error_cm <= 2) * 100,
        within_5_cm_percent =
          mean(group$absolute_error_cm <= 5) * 100
      )
    }
  )
)

row.names(accuracy_by_quality) <- NULL

# Create the inspection queue
inspection_queue <- batch_results[
  batch_results$quality_flag != "acceptable",
  c(
    "tree_id",
    "species",
    "quality_flag",
    "method_used",
    "reference_dbh_cm",
    "estimated_dbh_cm",
    "error_cm",
    "absolute_error_cm",
    "circle_rmse_mm",
    "circumference_completeness_percent",
    "error_message"
  )
]

inspection_queue <- inspection_queue[
  order(
    inspection_queue$absolute_error_cm,
    decreasing = TRUE,
    na.last = TRUE
  ),
]

# Save validation tables
dir.create(
  "outputs/tls/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  accuracy_by_quality,
  "outputs/tls/tables/tls_dbh_accuracy_by_quality.csv",
  row.names = FALSE
)

write.csv(
  inspection_queue,
  "outputs/tls/tables/tls_dbh_inspection_queue.csv",
  row.names = FALSE
)

# Create validation figure
dir.create(
  "outputs/tls/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "outputs/tls/figures/tls_dbh_batch_validation.png",
  width = 1800,
  height = 850,
  res = 160
)

par(
  mfrow = c(1, 2),
  mar = c(5, 5, 4, 2)
)

# Panel 1: all successful estimates
point_colours <- ifelse(
  successful_results$quality_flag == "acceptable",
  "#2E7D32",
  "#E67E22"
)

plot(
  successful_results$reference_dbh_cm,
  successful_results$estimated_dbh_cm,
  pch = 16,
  cex = 1.2,
  col = point_colours,
  xlab = "Reference DBH (cm)",
  ylab = "Estimated DBH (cm)",
  main = "All successful DBH estimates",
  asp = 1
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  lwd = 2,
  col = "grey35"
)

# Label estimates with errors greater than 5 cm
outliers <- successful_results[
  successful_results$absolute_error_cm > 5,
]

text(
  outliers$reference_dbh_cm,
  outliers$estimated_dbh_cm,
  labels = outliers$tree_id,
  pos = 4,
  cex = 0.8
)

legend(
  "topleft",
  legend = c("Acceptable", "Inspect", "Perfect agreement"),
  col = c("#2E7D32", "#E67E22", "grey35"),
  pch = c(16, 16, NA),
  lty = c(NA, NA, 2),
  bty = "n"
)

# Panel 2: acceptable estimates only
acceptable_results <- successful_results[
  successful_results$quality_flag == "acceptable",
]

plot(
  acceptable_results$reference_dbh_cm,
  acceptable_results$estimated_dbh_cm,
  pch = 16,
  cex = 1.2,
  col = "#2E7D32",
  xlab = "Reference DBH (cm)",
  ylab = "Estimated DBH (cm)",
  main = paste(
    "Quality-approved estimates:",
    nrow(acceptable_results),
    "trees"
  ),
  asp = 1
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  lwd = 2,
  col = "grey35"
)

accepted_mae <- mean(
  acceptable_results$absolute_error_cm
)

accepted_rmse <- sqrt(
  mean(acceptable_results$error_cm^2)
)

legend(
  "topleft",
  legend = c(
    paste0("MAE = ", round(accepted_mae, 2), " cm"),
    paste0("RMSE = ", round(accepted_rmse, 2), " cm")
  ),
  bty = "n"
)

dev.off()

cat("\nAccuracy by quality category:\n")
print(accuracy_by_quality)

cat("\nTrees requiring inspection or processing improvement:\n")
print(inspection_queue)

cat(
  "\nValidation figure saved to:",
  "outputs/tls/figures/tls_dbh_batch_validation.png\n"
)