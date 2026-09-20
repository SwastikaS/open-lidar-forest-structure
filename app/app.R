# TLS Tree Structure Analyser

library(shiny)

options(shiny.maxRequestSize = 500 * 1024^2)

engine_path <- if (file.exists("../R/tls_processing.R")) {
  "../R/tls_processing.R"
} else {
  "R/tls_processing.R"
}
source(engine_path)

format_value <- function(value, digits, suffix = "") {
  if (length(value) == 0 || is.na(value) || !is.finite(value)) {
    return("Unavailable")
  }
  paste0(round(value, digits), suffix)
}

result_cards <- function(prefix) {
  fluidRow(
    column(4, div(class = "result-card",
                  div(class = "result-value", textOutput(paste0(prefix, "height"))),
                  div(class = "result-label", "Tree height")
    )),
    column(4, div(class = "result-card",
                  div(class = "result-value", textOutput(paste0(prefix, "dbh"))),
                  div(class = "result-label", "Estimated DBH")
    )),
    column(4, div(class = "result-card",
                  div(class = "result-value", textOutput(paste0(prefix, "basal_area"))),
                  div(class = "result-label", "Basal area")
    ))
  )
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    html, body { overflow-x: hidden; background-color: #f4f7f4; }
    .container-fluid { width: 100%; max-width: 1500px; margin: auto; }
    .title-panel { background-color: #234d34; color: white; padding: 22px;
      margin: 15px 0 24px 0; border-radius: 6px; }
    .title-panel h2 { margin-top: 0; }
    .title-panel p { margin-bottom: 0; font-size: 16px; }
    .result-card { min-height: 125px; background-color: white; padding: 18px;
      margin-bottom: 15px; border-radius: 6px; border-left: 5px solid #3b7d4f;
      box-shadow: 0 1px 4px rgba(0, 0, 0, 0.10); }
    .result-value { font-size: 27px; font-weight: bold; color: #234d34;
      overflow-wrap: anywhere; }
    .result-label { margin-top: 5px; color: #555555; }
    .section-panel { background-color: white; padding: 20px; margin: 15px 0 20px 0;
      border-radius: 6px; box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08); }
    .section-panel h3 { margin-top: 0; color: #234d34; }
    .btn-success { background-color: #347847; border-color: #347847; }
    .btn-success:hover, .btn-success:focus { background-color: #285f38;
      border-color: #285f38; }
    .plot-note { margin-top: 10px; color: #666; font-size: 13px; }
    .tab-content { padding-top: 18px; }
    .table-scroll { overflow-x: auto; }
  "))),

  div(class = "title-panel",
      h2("TLS Tree Structure Analyser"),
      p(
        "Estimate tree height, DBH, basal area, lower-stem structure ",
        "and crown dimensions from isolated terrestrial LiDAR tree point clouds."
      )
  ),

  tabsetPanel(id = "analysis_mode",
              tabPanel(
                "Single tree",

                sidebarLayout(
                  sidebarPanel(
                    h4("Point-cloud input"),

                    fileInput(
                      "single_file",
                      "Upload one isolated tree",
                      accept = c(".las", ".laz")
                    ),

                    actionButton(
                      "analyse_single",
                      "Analyse tree",
                      class = "btn-success",
                      width = "100%"
                    ),

                    br(),
                    br(),

                    helpText(
                      "The file must contain one isolated tree, include the tree base ",
                      "and use metres for XYZ coordinates."
                    ),

                    tags$hr(),

                    strong("Prototype notice"),

                    p(
                      "Measurements that do not pass the quality checks are marked ",
                      "for inspection and should not be treated as validated results."
                    )
                  ),

                  mainPanel(
                    uiOutput("single_message"),

                    conditionalPanel(
                      condition = "output.single_complete",

                      result_cards("single_"),

                      conditionalPanel(
                        condition = "output.single_visual_available",

                        div(
                          class = "section-panel",
                          h3("Tree point-cloud views"),

                          plotOutput(
                            "single_projections",
                            height = "520px"
                          ),

                          div(
                            class = "plot-note",
                            "A reproducible sample of up to 50,000 points is displayed."
                          )
                        ),

                        div(
                          class = "section-panel",
                          h3("DBH cross-section at 1.3 m"),

                          plotOutput(
                            "single_cross_section",
                            height = "520px"
                          )
                        ),

                        div(
                          class = "section-panel",

                          h3("Lower-stem taper"),

                          tableOutput(
                            "single_taper_table"
                          ),

                          plotOutput(
                            "single_taper_plot",
                            height = "420px"
                          ),

                          tableOutput(
                            "single_taper_summary"
                          ),

                          div(
                            class = "plot-note",
                            "Diameters are estimated at 1.3, 2, 4 and 6 m. ",
                            "Measurements marked inspect require visual review."
                          )
                        ),
                        div(
                          class = "section-panel",
                          h3("Crown structure"),

                          tableOutput(
                            "single_crown_summary"
                          ),

                          plotOutput(
                            "single_crown_projection",
                            height = "560px"
                          ),

                          div(
                            class = "plot-note",
                            "Crown diameter uses the complete-tree XY convex hull. ",
                            "Projected convex-hull area is experimental."
                          )
                        )
                      ),

                      div(
                        class = "section-panel",
                        h3("Complete result"),
                        tableOutput("single_result")
                      ),

                      downloadButton(
                        "download_single",
                        "Download result as CSV"
                      )
                    )
                  )
                )
              ),
              tabPanel("Batch processing",
                       sidebarLayout(
                         sidebarPanel(
                           h4("Multiple point clouds"),
                           fileInput("batch_files", "Upload isolated-tree LAS or LAZ files",
                                     multiple = TRUE, accept = c(".las", ".laz")),
                           actionButton("analyse_batch", "Analyse all trees",
                                        class = "btn-success", width = "100%"),
                           br(), br(),
                           helpText("Each file must contain one isolated tree. Files are processed ",
                                    "sequentially, so one failed tree does not stop the batch."),
                           tags$hr(),
                           uiOutput("batch_tree_selector")
                         ),
                         mainPanel(
                           uiOutput("batch_message"),
                           conditionalPanel(condition = "output.batch_complete",
                                            fluidRow(
                                              column(4, div(class = "result-card",
                                                            div(class = "result-value", textOutput("batch_total")),
                                                            div(class = "result-label", "Files processed")
                                              )),
                                              column(4, div(class = "result-card",
                                                            div(class = "result-value", textOutput("batch_acceptable")),
                                                            div(class = "result-label", "Acceptable results")
                                              )),
                                              column(4, div(class = "result-card",
                                                            div(class = "result-value", textOutput("batch_attention")),
                                                            div(class = "result-label", "Inspect or failed")
                                              ))
                                            ),
                                            div(class = "section-panel table-scroll",
                                                h3("Batch results"),
                                                tableOutput("batch_table")
                                            ),
                                            downloadButton("download_batch", "Download batch CSV"),
                                            conditionalPanel(
                                              condition = "output.batch_visual_available",
                                              div(
                                                class = "section-panel",

                                                h3(textOutput("selected_tree_heading")),

                                                result_cards("selected_"),

                                                h3("Tree point-cloud views"),
                                                plotOutput(
                                                  "selected_projections",
                                                  height = "520px"
                                                ),

                                                h3("DBH cross-section at 1.3 m"),
                                                plotOutput(
                                                  "selected_cross_section",
                                                  height = "520px"
                                                ),

                                                h3("Lower-stem taper"),

                                                tableOutput(
                                                  "selected_taper_table"
                                                ),

                                                plotOutput(
                                                  "selected_taper_plot",
                                                  height = "420px"
                                                ),

                                                tableOutput(
                                                  "selected_taper_summary"
                                                ),

                                                div(
                                                  class = "plot-note",
                                                  "Diameters are estimated at 1.3, 2, 4 and 6 m. ",
                                                  "Measurements marked inspect require visual review."
                                                ),

                                                h3("Crown structure"),
                                                tableOutput("selected_crown_summary"),

                                                plotOutput(
                                                  "selected_crown_projection",
                                                  height = "560px"
                                                ),

                                                div(
                                                  class = "plot-note",
                                                  "Crown diameter uses the complete-tree XY convex hull. ",
                                                  "Projected convex-hull area is experimental."
                                                )
                                              )
                                            )
                         )
                       )
              )
  )
)
)
server <- function(input, output, session) {
  single_analysis <- reactiveVal(NULL)
  batch_results <- reactiveVal(NULL)
  batch_paths <- reactiveVal(NULL)
  selected_visual <- reactiveVal(NULL)
  upload_directory <- tempfile("tls_batch_")
  dir.create(upload_directory)

  session$onSessionEnded(function() {
    unlink(upload_directory, recursive = TRUE, force = TRUE)
  })

  prepare_upload <- function(datapath, original_name) {
    extension <- tolower(tools::file_ext(original_name))
    if (!extension %in% c("las", "laz")) {
      stop("Only LAS and LAZ files are supported.")
    }
    destination <- tempfile(pattern = "tls_", tmpdir = upload_directory,
                            fileext = paste0(".", extension))
    if (!file.copy(datapath, destination, overwrite = TRUE)) {
      stop("The uploaded file could not be prepared for processing.")
    }
    destination
  }

  observeEvent(input$analyse_single, {

    req(input$single_file)

    result <- tryCatch(
      {
        path <- prepare_upload(
          input$single_file$datapath,
          input$single_file$name
        )

        withProgress(
          message = "Processing the TLS point cloud",
          value = 0,
          {
            incProgress(
              0.25,
              detail = "Reading coordinates"
            )

            analysed <- process_tls_tree_visual(
              path
            )

            incProgress(
              0.25,
              detail = "Calculating crown structure"
            )

            crown_result <- tryCatch(
              estimate_crown_structure(path),
              error = function(e) e
            )

            if (inherits(crown_result, "error")) {

              analysed$summary$
                estimated_crown_diameter_m <-
                NA_real_

              analysed$summary$
                crown_diameter_1_xy_m <-
                NA_real_

              analysed$summary$
                crown_diameter_1_pca_m <-
                NA_real_

              analysed$summary$
                crown_diameter_2_m <-
                NA_real_

              analysed$summary$
                maximum_crown_span_m <-
                NA_real_

              analysed$summary$
                projected_convex_hull_area_m2 <-
                NA_real_

              analysed$summary$
                crown_hull_vertex_count <-
                NA_integer_

              analysed$summary$crown_method <-
                "complete-tree XY convex hull"

              analysed$summary$crown_area_status <-
                "experimental"

              analysed$summary$
                crown_processing_status <-
                "failed"

              analysed$summary$crown_error_message <-
                conditionMessage(crown_result)

              analysed$crown_hull <- NULL

            } else {

              crown_summary <-
                crown_result$summary

              crown_columns <- c(
                "estimated_crown_diameter_m",
                "crown_diameter_1_xy_m",
                "crown_diameter_1_pca_m",
                "crown_diameter_2_m",
                "maximum_crown_span_m",
                "projected_convex_hull_area_m2",
                "crown_hull_vertex_count",
                "crown_method",
                "crown_area_status",
                "crown_processing_status",
                "crown_error_message"
              )

              for (column_name in crown_columns) {
                analysed$summary[[column_name]] <-
                  crown_summary[[column_name]][1]
              }

              analysed$crown_hull <-
                crown_result$closed_hull
            }

            analysed$summary$file_name <-
              input$single_file$name

            incProgress(
              0.25,
              detail = "Estimating lower-stem taper"
            )

            if (isTRUE(analysed$success)) {
              analysed$taper <-
                estimate_stem_taper(path)

              taper_table <-
                analysed$taper$taper_table

              taper_summary <-
                analysed$taper$summary

              diameter_at <- function(height) {
                matching_row <- which(
                  abs(
                    taper_table$height_m -
                      height
                  ) < 0.001
                )

                if (length(matching_row) == 0) {
                  return(NA_real_)
                }

                taper_table$estimated_diameter_cm[
                  matching_row[1]
                ]
              }

              analysed$summary$diameter_2m_cm <-
                diameter_at(2)

              analysed$summary$diameter_4m_cm <-
                diameter_at(4)

              analysed$summary$diameter_6m_cm <-
                diameter_at(6)

              analysed$summary$mean_taper_cm_per_m <-
                taper_summary$mean_taper_cm_per_m

              analysed$summary$linear_taper_cm_per_m <-
                taper_summary$linear_taper_cm_per_m

              analysed$summary$lower_stem_displacement_m <-
                taper_summary$lower_stem_displacement_m

              analysed$summary$measured_vertical_span_m <-
                taper_summary$measured_vertical_span_m

              analysed$summary$lower_stem_lean_degrees <-
                taper_summary$lower_stem_lean_degrees

              analysed$summary$taper_successful_measurements <-
                taper_summary$successful_measurements

              analysed$summary$taper_requested_measurements <-
                taper_summary$requested_measurements

            } else {
              analysed$taper <- NULL

              analysed$summary$diameter_2m_cm <-
                NA_real_

              analysed$summary$diameter_4m_cm <-
                NA_real_

              analysed$summary$diameter_6m_cm <-
                NA_real_

              analysed$summary$mean_taper_cm_per_m <-
                NA_real_

              analysed$summary$linear_taper_cm_per_m <-
                NA_real_

              analysed$summary$lower_stem_displacement_m <-
                NA_real_

              analysed$summary$measured_vertical_span_m <-
                NA_real_

              analysed$summary$lower_stem_lean_degrees <-
                NA_real_

              analysed$summary$taper_successful_measurements <-
                NA_integer_

              analysed$summary$taper_requested_measurements <-
                NA_integer_
            }

            incProgress(
              0.25,
              detail = "Preparing results"
            )

            analysed
          }
        )
      },

      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error"
        )

        NULL
      }
    )

    single_analysis(result)
  })

  observeEvent(input$analyse_batch, {

    req(input$batch_files)

    invalid <-
      !tolower(
        tools::file_ext(
          input$batch_files$name
        )
      ) %in% c("las", "laz")

    if (any(invalid)) {
      showNotification(
        "Every uploaded file must be LAS or LAZ.",
        type = "error"
      )

      return()
    }

    selected_visual(NULL)

    paths <- tryCatch(
      vapply(
        seq_len(nrow(input$batch_files)),
        function(i) {
          prepare_upload(
            input$batch_files$datapath[i],
            input$batch_files$name[i]
          )
        },
        character(1)
      ),

      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error"
        )

        NULL
      }
    )

    req(paths)

    names(paths) <-
      input$batch_files$name

    batch_paths(paths)

    results <- withProgress(
      message = "Processing TLS files",
      value = 0,
      {
        results <- process_tls_batch(
          unname(paths),
          names(paths),

          progress_callback = function(
    i,
    total,
    name
          ) {
            incProgress(
              0.75 / total,
              detail = paste(
                "Tree",
                i,
                "of",
                total,
                ":",
                name
              )
            )
          }
        )

        crown_rows <- vector(
          "list",
          length(paths)
        )

        for (i in seq_along(paths)) {

          crown_result <- tryCatch(
            estimate_crown_structure(
              paths[i]
            ),
            error = function(e) e
          )

          if (inherits(crown_result, "error")) {

            crown_rows[[i]] <- data.frame(
              crown_point_count =
                NA_integer_,
              crown_hull_vertex_count =
                NA_integer_,
              crown_diameter_1_xy_m =
                NA_real_,
              crown_diameter_1_pca_m =
                NA_real_,
              crown_diameter_2_m =
                NA_real_,
              estimated_crown_diameter_m =
                NA_real_,
              maximum_crown_span_m =
                NA_real_,
              projected_convex_hull_area_m2 =
                NA_real_,
              crown_method =
                "complete-tree XY convex hull",
              crown_area_status =
                "experimental",
              crown_processing_status =
                "failed",
              crown_error_message =
                conditionMessage(crown_result),
              stringsAsFactors = FALSE
            )

          } else {

            crown_summary <-
              crown_result$summary

            crown_rows[[i]] <-
              crown_summary[
                ,
                c(
                  "crown_point_count",
                  "crown_hull_vertex_count",
                  "crown_diameter_1_xy_m",
                  "crown_diameter_1_pca_m",
                  "crown_diameter_2_m",
                  "estimated_crown_diameter_m",
                  "maximum_crown_span_m",
                  "projected_convex_hull_area_m2",
                  "crown_method",
                  "crown_area_status",
                  "crown_processing_status",
                  "crown_error_message"
                ),
                drop = FALSE
              ]
          }

          incProgress(
            0.25 / length(paths),
            detail = paste(
              "Crown",
              i,
              "of",
              length(paths),
              ":",
              names(paths)[i]
            )
          )
        }

        crown_table <- do.call(
          rbind,
          crown_rows
        )

        row.names(crown_table) <- NULL

        for (
          column_name in names(crown_table)
        ) {
          results[[column_name]] <-
            crown_table[[column_name]]
        }

        results
      }
    )

    batch_results(results)
  })

  output$single_complete <- reactive(!is.null(single_analysis()))
  output$single_visual_available <- reactive({
    result <- single_analysis()
    !is.null(result) && isTRUE(result$success)
  })
  output$batch_complete <- reactive(!is.null(batch_results()))
  output$batch_visual_available <- reactive({
    result <- selected_visual()
    !is.null(result) && isTRUE(result$success)
  })

  for (id in c("single_complete", "single_visual_available",
               "batch_complete", "batch_visual_available")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  output$single_message <- renderUI({
    result <- single_analysis()
    if (is.null(result)) {
      return(div(class = "alert alert-info",
                 "Upload one tree and select Analyse tree."))
    }
    summary <- result$summary
    if (summary$processing_status == "failed") {
      div(class = "alert alert-danger", strong("Processing failed: "),
          summary$error_message)
    } else if (summary$quality_flag == "acceptable") {
      div(class = "alert alert-success", strong("Quality check: acceptable."))
    } else {
      div(class = "alert alert-warning",
          strong("Quality check: inspect the fitted circle."))
    }
  })

  output$batch_message <- renderUI({
    results <- batch_results()
    if (is.null(results)) {
      return(div(class = "alert alert-info",
                 "Upload multiple trees and select Analyse all trees."))
    }
    failed <- sum(results$processing_status == "failed")
    div(class = if (failed == 0) "alert alert-success" else "alert alert-warning",
        strong("Batch complete. "),
        paste(nrow(results), "files processed;", failed, "failed."))
  })

  single_summary <- reactive({
    req(single_analysis())
    single_analysis()$summary
  })

  output$single_result <- renderTable(
    {
      summary <- single_summary()

      safe_text <- function(value) {
        if (
          length(value) == 0 ||
          is.na(value[1]) ||
          !nzchar(as.character(value[1]))
        ) {
          return("Unavailable")
        }

        as.character(value[1])
      }

      safe_count <- function(value) {
        if (
          length(value) == 0 ||
          is.na(value[1]) ||
          !is.finite(as.numeric(value[1]))
        ) {
          return("Unavailable")
        }

        format(
          as.numeric(value[1]),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      }

      taper_measurement_count <- if (
        length(
          summary$taper_successful_measurements
        ) == 0 ||
        length(
          summary$taper_requested_measurements
        ) == 0 ||
        is.na(
          summary$taper_successful_measurements[1]
        ) ||
        is.na(
          summary$taper_requested_measurements[1]
        )
      ) {
        "Unavailable"
      } else {
        paste0(
          summary$taper_successful_measurements[1],
          " of ",
          summary$taper_requested_measurements[1]
        )
      }

      result_values <- c(
        safe_text(
          summary$file_name
        ),

        safe_count(
          summary$point_count
        ),

        format_value(
          summary$calculated_height_m,
          2,
          " m"
        ),

        format_value(
          summary$estimated_dbh_cm,
          2,
          " cm"
        ),

        format_value(
          summary$basal_area_m2,
          3,
          " m²"
        ),

        safe_text(
          summary$method_used
        ),

        format_value(
          summary$circle_rmse_mm,
          2,
          " mm"
        ),

        format_value(
          summary$circumference_completeness_percent,
          1,
          "%"
        ),

        safe_count(
          summary$retained_stem_points
        ),

        safe_text(
          summary$quality_flag
        ),

        format_value(
          summary$diameter_2m_cm,
          2,
          " cm"
        ),

        format_value(
          summary$diameter_4m_cm,
          2,
          " cm"
        ),

        format_value(
          summary$diameter_6m_cm,
          2,
          " cm"
        ),

        format_value(
          summary$mean_taper_cm_per_m,
          2,
          " cm/m"
        ),

        format_value(
          summary$linear_taper_cm_per_m,
          2,
          " cm/m"
        ),

        format_value(
          summary$lower_stem_displacement_m,
          3,
          " m"
        ),

        format_value(
          summary$measured_vertical_span_m,
          2,
          " m"
        ),

        format_value(
          summary$lower_stem_lean_degrees,
          2,
          "°"
        ),

        taper_measurement_count,

        format_value(
          summary$estimated_crown_diameter_m,
          2,
          " m"
        ),

        format_value(
          summary$crown_diameter_1_xy_m,
          2,
          " m"
        ),

        format_value(
          summary$crown_diameter_1_pca_m,
          2,
          " m"
        ),

        format_value(
          summary$maximum_crown_span_m,
          2,
          " m"
        ),

        safe_count(
          summary$crown_hull_vertex_count
        ),

        paste0(
          format_value(
            summary$projected_convex_hull_area_m2,
            2,
            " m²"
          ),
          " — experimental"
        ),

        safe_text(
          summary$crown_method
        )
      )

      data.frame(
        Category = c(
          rep("Input", 2),
          "Tree",
          rep("Stem", 7),
          rep("Taper", 9),
          rep("Crown", 7)
        ),

        Measurement = c(
          "File name",
          "Point count",
          "Tree height",
          "Estimated DBH",
          "Basal area",
          "DBH method",
          "Circle-fit RMSE",
          "Circumference completeness",
          "Retained stem points",
          "DBH quality",
          "Diameter at 2 m",
          "Diameter at 4 m",
          "Diameter at 6 m",
          "Mean taper",
          "Linear taper",
          "Lower-stem displacement",
          "Measured vertical span",
          "Lower-stem lean",
          "Successful measurement heights",
          "Preferred crown diameter",
          "Original XY mean diameter",
          "PCA-rotated mean diameter",
          "Maximum crown span",
          "Convex-hull vertices",
          "Projected convex-hull area",
          "Crown method"
        ),

        Result = result_values,

        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "m",
    rownames = FALSE,
    na = "Unavailable"
  )

  output$batch_total <- renderText(nrow(batch_results()))
  output$batch_acceptable <- renderText(
    sum(batch_results()$quality_flag == "acceptable"))
  output$batch_attention <- renderText(
    sum(batch_results()$quality_flag != "acceptable"))
  output$batch_table <- renderTable(
    {
      results <- batch_results()

      data.frame(
        File =
          results$file_name,

        `Height (m)` =
          round(
            results$calculated_height_m,
            2
          ),

        `DBH (cm)` =
          round(
            results$estimated_dbh_cm,
            2
          ),

        `Diameter at 6 m (cm)` =
          round(
            results$diameter_6m_cm,
            2
          ),

        `Mean taper (cm/m)` =
          round(
            results$mean_taper_cm_per_m,
            2
          ),

        `Lean (degrees)` =
          round(
            results$lower_stem_lean_degrees,
            2
          ),

        `Crown diameter (m)` =
          round(
            results$estimated_crown_diameter_m,
            2
          ),

        `DBH quality` =
          results$quality_flag,

        `Taper quality` =
          results$taper_quality_flag,

        Status =
          results$processing_status,

        check.names = FALSE
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    rownames = FALSE,
    na = "—"
  )

  output$batch_tree_selector <- renderUI({
    results <- batch_results()
    req(results)
    successful <- results$file_name[results$processing_status == "success"]
    if (length(successful) == 0) {
      return(helpText("No successful tree is available for visual inspection."))
    }
    selectInput("selected_batch_tree", "Inspect one processed tree",
                choices = successful)
  })

  observeEvent(
    input$selected_batch_tree,
    {

      req(
        nzchar(input$selected_batch_tree),
        batch_paths()
      )

      path <- unname(
        batch_paths()[
          input$selected_batch_tree
        ]
      )

      req(
        length(path) == 1,
        file.exists(path)
      )

      result <- withProgress(
        message = paste(
          "Preparing",
          input$selected_batch_tree
        ),
        value = 0,
        {

          incProgress(
            0.25,
            detail = "Preparing tree measurements"
          )

          analysed <- process_tls_tree_visual(
            path
          )

          analysed$summary$file_name <-
            input$selected_batch_tree

          incProgress(
            0.25,
            detail = "Calculating crown structure"
          )

          crown_result <- tryCatch(
            estimate_crown_structure(path),
            error = function(e) e
          )

          if (inherits(crown_result, "error")) {

            analysed$summary$
              estimated_crown_diameter_m <-
              NA_real_

            analysed$summary$
              crown_diameter_1_xy_m <-
              NA_real_

            analysed$summary$
              crown_diameter_1_pca_m <-
              NA_real_

            analysed$summary$
              crown_diameter_2_m <-
              NA_real_

            analysed$summary$
              maximum_crown_span_m <-
              NA_real_

            analysed$summary$
              projected_convex_hull_area_m2 <-
              NA_real_

            analysed$summary$
              crown_hull_vertex_count <-
              NA_integer_

            analysed$summary$crown_method <-
              "complete-tree XY convex hull"

            analysed$summary$crown_area_status <-
              "experimental"

            analysed$summary$
              crown_processing_status <-
              "failed"

            analysed$summary$crown_error_message <-
              conditionMessage(crown_result)

            analysed$crown_hull <- NULL

          } else {

            crown_summary <-
              crown_result$summary

            crown_columns <- c(
              "estimated_crown_diameter_m",
              "crown_diameter_1_xy_m",
              "crown_diameter_1_pca_m",
              "crown_diameter_2_m",
              "maximum_crown_span_m",
              "projected_convex_hull_area_m2",
              "crown_hull_vertex_count",
              "crown_method",
              "crown_area_status",
              "crown_processing_status",
              "crown_error_message"
            )

            for (column_name in crown_columns) {
              analysed$summary[[column_name]] <-
                crown_summary[[column_name]][1]
            }

            analysed$crown_hull <-
              crown_result$closed_hull
          }

          incProgress(
            0.25,
            detail = "Estimating lower-stem taper"
          )

          if (isTRUE(analysed$success)) {
            analysed$taper <-
              estimate_stem_taper(path)
          } else {
            analysed$taper <- NULL
          }

          incProgress(
            0.25,
            detail = "Preparing visualisations"
          )

          analysed
        }
      )

      selected_visual(result)
    },

    ignoreInit = TRUE
  )
  selected_summary <- reactive({
    req(selected_visual())
    selected_visual()$summary
  })

  output$selected_tree_heading <- renderText(
    paste("Inspecting", selected_summary()$file_name))
  output$selected_height <- renderText(
    format_value(selected_summary()$calculated_height_m, 2, " m"))
  output$selected_dbh <- renderText(
    format_value(selected_summary()$estimated_dbh_cm, 2, " cm"))
  output$selected_basal_area <- renderText(
    format_value(selected_summary()$basal_area_m2, 3, " m²"))
  output$single_crown_summary <- renderTable(
    {
      result <- single_analysis()
      req(result)

      summary <- result$summary

      safe_text <- function(value) {
        if (
          length(value) == 0 ||
          is.na(value[1]) ||
          !nzchar(as.character(value[1]))
        ) {
          return("Unavailable")
        }

        as.character(value[1])
      }

      safe_count <- function(value) {
        if (
          length(value) == 0 ||
          is.na(value[1]) ||
          !is.finite(as.numeric(value[1]))
        ) {
          return("Unavailable")
        }

        format(
          as.numeric(value[1]),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      }

      data.frame(
        Measurement = c(
          "Preferred crown diameter",
          "Original XY mean diameter",
          "PCA-rotated mean diameter",
          "Maximum crown span",
          "Convex-hull vertices",
          "Projected convex-hull area",
          "Crown method",
          "Crown processing status"
        ),

        Result = c(
          format_value(
            summary$estimated_crown_diameter_m,
            2,
            " m"
          ),

          format_value(
            summary$crown_diameter_1_xy_m,
            2,
            " m"
          ),

          format_value(
            summary$crown_diameter_1_pca_m,
            2,
            " m"
          ),

          format_value(
            summary$maximum_crown_span_m,
            2,
            " m"
          ),

          safe_count(
            summary$crown_hull_vertex_count
          ),

          paste0(
            format_value(
              summary$projected_convex_hull_area_m2,
              2,
              " m²"
            ),
            " — experimental"
          ),

          safe_text(
            summary$crown_method
          ),

          safe_text(
            summary$crown_processing_status
          )
        ),

        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "m",
    rownames = FALSE,
    na = "Unavailable"
  )

  output$single_crown_projection <- renderPlot(
    {
      result <- single_analysis()

      req(
        result,
        isTRUE(result$success)
      )

      points <- result$display_points
      hull <- result$crown_hull
      summary <- result$summary

      validate(
        need(
          !is.null(points) &&
            nrow(points) > 0,
          "Point-cloud display data are unavailable."
        ),

        need(
          !is.null(hull) &&
            nrow(hull) >= 3,
          "Crown hull is unavailable."
        )
      )

      plot(
        points$X,
        points$Y,
        asp = 1,
        pch = 16,
        cex = 0.22,
        col = rgb(
          0.12,
          0.34,
          0.18,
          0.20
        ),
        xlab = "X coordinate (m)",
        ylab = "Y coordinate (m)",
        main = paste0(
          "Estimated crown diameter: ",
          format_value(
            summary$estimated_crown_diameter_m,
            2,
            " m"
          )
        )
      )

      polygon(
        hull$X,
        hull$Y,
        border = "#D95F02",
        lwd = 3
      )

      points(
        hull$X,
        hull$Y,
        pch = 21,
        bg = "#F4A261",
        col = "#A64B00",
        cex = 0.85
      )

      legend(
        "topright",
        legend = c(
          "Tree points",
          "Complete XY convex hull"
        ),
        col = c(
          rgb(
            0.12,
            0.34,
            0.18,
            0.70
          ),
          "#D95F02"
        ),
        pch = c(16, NA),
        lty = c(NA, 1),
        lwd = c(NA, 3),
        bty = "n"
      )
    }
  )
  output$selected_crown_summary <- renderTable(
    {
      summary <- selected_summary()

      safe_text <- function(value) {
        if (
          length(value) == 0 ||
          is.na(value[1]) ||
          !nzchar(as.character(value[1]))
        ) {
          return("Unavailable")
        }

        as.character(value[1])
      }

      safe_count <- function(value) {
        if (
          length(value) == 0 ||
          is.na(value[1]) ||
          !is.finite(as.numeric(value[1]))
        ) {
          return("Unavailable")
        }

        format(
          as.numeric(value[1]),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      }

      data.frame(
        Measurement = c(
          "Preferred crown diameter",
          "Original XY mean diameter",
          "PCA-rotated mean diameter",
          "Maximum crown span",
          "Convex-hull vertices",
          "Projected convex-hull area",
          "Crown method",
          "Crown processing status"
        ),

        Result = c(
          format_value(
            summary$estimated_crown_diameter_m,
            2,
            " m"
          ),

          format_value(
            summary$crown_diameter_1_xy_m,
            2,
            " m"
          ),

          format_value(
            summary$crown_diameter_1_pca_m,
            2,
            " m"
          ),

          format_value(
            summary$maximum_crown_span_m,
            2,
            " m"
          ),

          safe_count(
            summary$crown_hull_vertex_count
          ),

          paste0(
            format_value(
              summary$projected_convex_hull_area_m2,
              2,
              " m²"
            ),
            " — experimental"
          ),

          safe_text(
            summary$crown_method
          ),

          safe_text(
            summary$crown_processing_status
          )
        ),

        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "m",
    rownames = FALSE,
    na = "Unavailable"
  )

  output$selected_crown_projection <- renderPlot(
    {
      result <- selected_visual()

      req(
        result,
        result$success
      )

      points <- result$display_points
      hull <- result$crown_hull
      summary <- result$summary

      validate(
        need(
          !is.null(points) &&
            nrow(points) > 0,
          "Point-cloud display data are unavailable."
        ),

        need(
          !is.null(hull) &&
            nrow(hull) >= 3,
          "Crown hull is unavailable."
        )
      )

      plot(
        points$X,
        points$Y,
        asp = 1,
        pch = 16,
        cex = 0.22,
        col = rgb(
          0.12,
          0.34,
          0.18,
          0.20
        ),
        xlab = "X coordinate (m)",
        ylab = "Y coordinate (m)",
        main = paste0(
          "Estimated crown diameter: ",
          round(
            summary$estimated_crown_diameter_m,
            2
          ),
          " m"
        )
      )

      polygon(
        hull$X,
        hull$Y,
        border = "#D95F02",
        lwd = 3
      )

      points(
        hull$X,
        hull$Y,
        pch = 21,
        bg = "#F4A261",
        col = "#A64B00",
        cex = 0.85
      )

      legend(
        "topright",
        legend = c(
          "Tree points",
          "Complete XY convex hull"
        ),
        col = c(
          rgb(
            0.12,
            0.34,
            0.18,
            0.70
          ),
          "#D95F02"
        ),
        pch = c(16, NA),
        lty = c(NA, 1),
        lwd = c(NA, 3),
        bty = "n"
      )
    }
  )

  draw_projections <- function(result) {
    points <- result$display_points
    height_range <- range(points$height_above_base_m, na.rm = TRUE)
    if (diff(height_range) == 0) {
      colours <- rep("#2E7D32", nrow(points))
    } else {
      index <- floor((points$height_above_base_m - height_range[1]) /
                       diff(height_range) * 99) + 1
      index <- pmax(1, pmin(100, index))
      colours <- hcl.colors(100, "Viridis")[index]
    }
    par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
    plot(points$X, points$height_above_base_m,
         pch = 16, cex = 0.18, col = colours,
         xlab = "X coordinate (m)", ylab = "Height above tree base (m)",
         main = "Front view")
    abline(h = 1.3, col = "#C0392B", lty = 2, lwd = 2)
    plot(points$Y, points$height_above_base_m,
         pch = 16, cex = 0.18, col = colours,
         xlab = "Y coordinate (m)", ylab = "Height above tree base (m)",
         main = "Side view")
    abline(h = 1.3, col = "#C0392B", lty = 2, lwd = 2)
  }

  draw_cross_section <- function(result) {
    points <- result$dbh_display_points
    circle <- result$fitted_circle
    centre <- result$circle_centre
    summary <- result$summary
    circle_colour <- ifelse(summary$quality_flag == "acceptable",
                            "#2E7D32", "#E67E22")
    plot(points$X, points$Y,
         asp = 1, pch = 16, cex = 0.45,
         col = rgb(0.15, 0.15, 0.15, 0.35),
         xlab = "X coordinate (m)", ylab = "Y coordinate (m)",
         main = paste0("Fitted DBH: ",
                       round(summary$estimated_dbh_cm, 2), " cm"))
    lines(circle$X, circle$Y, col = circle_colour, lwd = 3)
    points(centre$X, centre$Y, pch = 3, cex = 1.4,
           lwd = 2, col = "#2457A7")
  }

  draw_taper <- function(taper_result) {
    taper <- taper_result$taper_table

    valid <- is.finite(
      taper$estimated_diameter_cm
    )

    validate(
      need(
        any(valid),
        "No valid stem-diameter measurements are available."
      )
    )

    plot(
      taper$estimated_diameter_cm[valid],
      taper$height_m[valid],
      type = "b",
      pch = 16,
      lwd = 2,
      col = "#347847",
      xlab = "Estimated stem diameter (cm)",
      ylab = "Height above tree base (m)",
      main = "Lower-stem taper",
      ylim = range(
        taper$height_m,
        na.rm = TRUE
      )
    )

    inspect <- valid &
      taper$quality_flag == "inspect"

    if (any(inspect)) {
      points(
        taper$estimated_diameter_cm[inspect],
        taper$height_m[inspect],
        pch = 17,
        cex = 1.3,
        col = "#E67E22"
      )
    }

    legend(
      "topright",
      legend = c(
        "Estimated diameter",
        "Inspect fit"
      ),
      col = c(
        "#347847",
        "#E67E22"
      ),
      pch = c(16, 17),
      lty = c(1, NA),
      bty = "n"
    )
  }

  taper_table_for_display <- function(taper_result) {
    taper <- taper_result$taper_table

    data.frame(
      `Height (m)` =
        taper$height_m,

      `Diameter (cm)` =
        round(
          taper$estimated_diameter_cm,
          2
        ),

      `Circle RMSE (mm)` =
        round(
          taper$circle_rmse_mm,
          2
        ),

      `Circumference (%)` =
        round(
          taper$circumference_completeness_percent,
          1
        ),

      `Points used` =
        taper$points_used,

      Quality =
        taper$quality_flag,

      check.names = FALSE
    )
  }

  taper_summary_for_display <- function(taper_result) {
    summary <- taper_result$summary

    data.frame(
      Measurement = c(
        "Mean taper",
        "Linear taper",
        "Lower-stem displacement",
        "Measured vertical span",
        "Lower-stem lean",
        "Successful measurement heights"
      ),

      Result = c(
        format_value(
          summary$mean_taper_cm_per_m,
          2,
          " cm/m"
        ),

        format_value(
          summary$linear_taper_cm_per_m,
          2,
          " cm/m"
        ),

        format_value(
          summary$lower_stem_displacement_m,
          3,
          " m"
        ),

        format_value(
          summary$measured_vertical_span_m,
          2,
          " m"
        ),

        format_value(
          summary$lower_stem_lean_degrees,
          2,
          "°"
        ),

        paste0(
          summary$successful_measurements,
          " of ",
          summary$requested_measurements
        )
      ),

      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  output$single_taper_table <- renderTable(
    {
      result <- single_analysis()

      req(
        result,
        result$taper
      )

      taper_table_for_display(
        result$taper
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    rownames = FALSE,
    na = "—"
  )

  output$single_taper_plot <- renderPlot(
    {
      result <- single_analysis()

      req(
        result,
        result$taper
      )

      draw_taper(
        result$taper
      )
    }
  )

  output$single_taper_summary <- renderTable(
    {
      result <- single_analysis()

      req(
        result,
        result$taper
      )

      taper_summary_for_display(
        result$taper
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    rownames = FALSE,
    na = "—"
  )

  output$single_projections <- renderPlot({
    req(single_analysis()$success)
    draw_projections(single_analysis())
  })
  output$single_cross_section <- renderPlot({
    req(single_analysis()$success)
    draw_cross_section(single_analysis())
  })

  output$selected_taper_table <- renderTable(
    {
      result <- selected_visual()

      req(
        result,
        result$taper
      )

      taper_table_for_display(
        result$taper
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    rownames = FALSE,
    na = "—"
  )

  output$selected_taper_plot <- renderPlot(
    {
      result <- selected_visual()

      req(
        result,
        result$taper
      )

      draw_taper(
        result$taper
      )
    }
  )

  output$selected_taper_summary <- renderTable(
    {
      result <- selected_visual()

      req(
        result,
        result$taper
      )

      taper_summary_for_display(
        result$taper
      )
    },

    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    rownames = FALSE,
    na = "—"
  )

  output$selected_projections <- renderPlot({
    req(selected_visual()$success)
    draw_projections(selected_visual())
  })
  output$selected_cross_section <- renderPlot({
    req(selected_visual()$success)
    draw_cross_section(selected_visual())
  })

  output$download_single <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(single_summary()$file_name),
             "_TLS_measurements.csv")
    },
    content = function(file) {
      write.csv(single_summary(), file, row.names = FALSE)
    }
  )

  output$download_batch <- downloadHandler(
    filename = function() paste0("TLS_batch_results_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(batch_results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
