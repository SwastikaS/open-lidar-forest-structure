# TLS Forest Structure prototype

library(shiny)

options(
  shiny.maxRequestSize = 500 * 1024^2
)

source("../R/tls_processing.R")

ui <- fluidPage(

  tags$head(
    tags$style(
      HTML("
        html,
        body {
          overflow-x: hidden;
          background-color: #f4f7f4;
        }

        .container-fluid {
          width: 100%;
          max-width: 1500px;
          margin: auto;
        }

        .title-panel {
          width: 100%;
          background-color: #234d34;
          color: white;
          padding: 22px;
          margin-top: 15px;
          margin-bottom: 24px;
          border-radius: 6px;
        }

        .title-panel h2 {
          margin-top: 0;
        }

        .title-panel p {
          margin-bottom: 0;
          font-size: 16px;
        }

        .result-card {
          min-height: 125px;
          background-color: white;
          padding: 18px;
          margin-bottom: 15px;
          border-radius: 6px;
          border-left: 5px solid #3b7d4f;
          box-shadow: 0 1px 4px rgba(0, 0, 0, 0.10);
        }

        .result-value {
          font-size: 27px;
          font-weight: bold;
          color: #234d34;
          overflow-wrap: anywhere;
        }

        .result-label {
          margin-top: 5px;
          color: #555555;
        }

        .section-panel {
          width: 100%;
          background-color: white;
          padding: 20px;
          margin-top: 15px;
          margin-bottom: 20px;
          border-radius: 6px;
          box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
        }

        .section-panel h3 {
          margin-top: 0;
          color: #234d34;
        }

        .shiny-table {
          width: 100%;
          max-width: 100%;
        }

        .shiny-table th,
        .shiny-table td {
          white-space: normal;
          overflow-wrap: anywhere;
          vertical-align: top;
        }

        .btn-success {
          background-color: #347847;
          border-color: #347847;
        }

        .btn-success:hover,
        .btn-success:focus {
          background-color: #285f38;
          border-color: #285f38;
        }

        .download-button {
          margin-top: 5px;
          margin-bottom: 25px;
        }

        .plot-note {
          margin-top: 10px;
          color: #666666;
          font-size: 13px;
        }
      ")
    )
  ),

  div(
    class = "title-panel",

    h2("TLS Forest Structure"),

    p(
      "Estimate tree height, diameter at breast height and basal area ",
      "from an isolated terrestrial LiDAR tree point cloud."
    )
  ),

  sidebarLayout(

    sidebarPanel(

      h4("Point-cloud input"),

      fileInput(
        "tls_file",
        "Upload an isolated tree point cloud",
        accept = c(".las", ".laz")
      ),

      actionButton(
        "analyse",
        "Analyse tree",
        class = "btn-success",
        width = "100%"
      ),

      br(),
      br(),

      helpText(
        "The input file must contain one isolated tree, include the ",
        "tree base and use metres for its XYZ coordinates."
      ),

      tags$hr(),

      strong("Prototype notice"),

      p(
        "Measurements that do not pass the quality checks are marked ",
        "for inspection. They should not be treated as validated results."
      )
    ),

    mainPanel(

      uiOutput("processing_message"),

      conditionalPanel(
        condition = "output.analysis_complete",

        fluidRow(

          column(
            width = 4,

            div(
              class = "result-card",

              div(
                class = "result-value",
                textOutput("tree_height")
              ),

              div(
                class = "result-label",
                "Tree height"
              )
            )
          ),

          column(
            width = 4,

            div(
              class = "result-card",

              div(
                class = "result-value",
                textOutput("tree_dbh")
              ),

              div(
                class = "result-label",
                "Estimated DBH"
              )
            )
          ),

          column(
            width = 4,

            div(
              class = "result-card",

              div(
                class = "result-value",
                textOutput("basal_area")
              ),

              div(
                class = "result-label",
                "Basal area"
              )
            )
          )
        ),

        conditionalPanel(
          condition = "output.visualisation_available",

          div(
            class = "section-panel",

            h3("Tree point-cloud views"),

            plotOutput(
              "tree_projections",
              height = "520px"
            ),

            div(
              class = "plot-note",

              paste(
                "The plots use a reproducible sample of up to 50,000",
                "points so that large point clouds remain responsive."
              )
            )
          ),

          div(
            class = "section-panel",

            h3("DBH cross-section at 1.3 m"),

            plotOutput(
              "dbh_cross_section",
              height = "520px"
            ),

            div(
              class = "plot-note",

              paste(
                "Points show the stem slice between 1.25 and 1.35 m.",
                "The line represents the fitted DBH circle."
              )
            )
          )
        ),

        div(
          class = "section-panel",

          h3("Measurement quality"),

          tableOutput("quality_table")
        ),

        div(
          class = "section-panel",

          h3("Complete result"),

          tableOutput("complete_result")
        ),

        div(
          class = "download-button",

          downloadButton(
            "download_result",
            "Download result as CSV"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  visual_analysis <- reactiveVal(NULL)

  observeEvent(
    input$analyse,
    {

      req(input$tls_file)

      uploaded_extension <- tolower(
        tools::file_ext(
          input$tls_file$name
        )
      )

      if (
        !uploaded_extension %in%
        c("las", "laz")
      ) {

        showNotification(
          "Please upload a LAS or LAZ file.",
          type = "error"
        )

        return()
      }

      temporary_file <- tempfile(
        fileext = paste0(
          ".",
          uploaded_extension
        )
      )

      copied_successfully <- file.copy(
        input$tls_file$datapath,
        temporary_file,
        overwrite = TRUE
      )

      if (!copied_successfully) {

        showNotification(
          "The uploaded file could not be prepared for processing.",
          type = "error"
        )

        return()
      }

      on.exit(
        unlink(temporary_file),
        add = TRUE
      )

      result <- withProgress(
        message = "Processing the TLS point cloud",
        value = 0,
        {

          incProgress(
            0.2,
            detail = "Reading point-cloud coordinates"
          )

          incProgress(
            0.4,
            detail = "Estimating tree height and DBH"
          )

          processed_result <- process_tls_tree_visual(
            temporary_file
          )

          processed_result$summary$file_name <-
            input$tls_file$name

          incProgress(
            0.4,
            detail = "Preparing quality-control plots"
          )

          processed_result
        }
      )

      visual_analysis(result)
    }
  )

  summary_result <- reactive({

    result <- visual_analysis()

    req(result)

    result$summary
  })

  output$analysis_complete <- reactive({
    !is.null(visual_analysis())
  })

  output$visualisation_available <- reactive({

    result <- visual_analysis()

    !is.null(result) &&
      isTRUE(result$success)
  })

  outputOptions(
    output,
    "analysis_complete",
    suspendWhenHidden = FALSE
  )

  outputOptions(
    output,
    "visualisation_available",
    suspendWhenHidden = FALSE
  )

  output$processing_message <- renderUI({

    if (is.null(visual_analysis())) {

      return(
        div(
          class = "alert alert-info",

          paste(
            "Upload an isolated LAS or LAZ tree file",
            "and select Analyse tree."
          )
        )
      )
    }

    result <- summary_result()

    if (result$processing_status == "failed") {

      return(
        div(
          class = "alert alert-danger",

          strong("Processing failed: "),

          result$error_message
        )
      )
    }

    if (result$quality_flag == "acceptable") {

      return(
        div(
          class = "alert alert-success",

          strong("Quality check: acceptable. "),

          paste(
            "The measurement passed the current",
            "prototype thresholds."
          )
        )
      )
    }

    div(
      class = "alert alert-warning",

      strong("Quality check: inspect. "),

      paste(
        "A result was calculated, but the fitted stem circle",
        "should be reviewed before using the measurement."
      )
    )
  })

  output$tree_height <- renderText({

    result <- summary_result()

    if (!is.finite(result$calculated_height_m)) {
      return("Unavailable")
    }

    paste0(
      round(
        result$calculated_height_m,
        2
      ),
      " m"
    )
  })

  output$tree_dbh <- renderText({

    result <- summary_result()

    if (!is.finite(result$estimated_dbh_cm)) {
      return("Unavailable")
    }

    paste0(
      round(
        result$estimated_dbh_cm,
        2
      ),
      " cm"
    )
  })

  output$basal_area <- renderText({

    result <- summary_result()

    if (!is.finite(result$basal_area_m2)) {
      return("Unavailable")
    }

    paste0(
      round(
        result$basal_area_m2,
        3
      ),
      " m²"
    )
  })

  output$tree_projections <- renderPlot({

    result <- visual_analysis()

    req(
      result,
      result$success,
      result$display_points
    )

    points <- result$display_points

    height_range <- range(
      points$height_above_base_m,
      na.rm = TRUE
    )

    if (diff(height_range) == 0) {
      point_colours <- rep(
        "#2E7D32",
        nrow(points)
      )
    } else {

      colour_index <- floor(
        (
          points$height_above_base_m -
            height_range[1]
        ) /
          diff(height_range) *
          99
      ) + 1

      colour_index <- pmax(
        1,
        pmin(
          100,
          colour_index
        )
      )

      point_colours <- hcl.colors(
        100,
        "Viridis"
      )[colour_index]
    }

    par(
      mfrow = c(1, 2),
      mar = c(4.5, 4.5, 3, 1)
    )

    plot(
      points$X,
      points$height_above_base_m,
      pch = 16,
      cex = 0.18,
      col = point_colours,
      xlab = "X coordinate (m)",
      ylab = "Height above tree base (m)",
      main = "Front view"
    )

    abline(
      h = 1.3,
      col = "#C0392B",
      lty = 2,
      lwd = 2
    )

    plot(
      points$Y,
      points$height_above_base_m,
      pch = 16,
      cex = 0.18,
      col = point_colours,
      xlab = "Y coordinate (m)",
      ylab = "Height above tree base (m)",
      main = "Side view"
    )

    abline(
      h = 1.3,
      col = "#C0392B",
      lty = 2,
      lwd = 2
    )
  })

  output$dbh_cross_section <- renderPlot({

    result <- visual_analysis()

    req(
      result,
      result$success,
      result$dbh_display_points,
      result$fitted_circle,
      result$circle_centre
    )

    slice_points <-
      result$dbh_display_points

    fitted_circle <-
      result$fitted_circle

    circle_centre <-
      result$circle_centre

    summary <- result$summary

    circle_colour <- ifelse(
      summary$quality_flag == "acceptable",
      "#2E7D32",
      "#E67E22"
    )

    plot(
      slice_points$X,
      slice_points$Y,
      asp = 1,
      pch = 16,
      cex = 0.45,
      col = rgb(
        0.15,
        0.15,
        0.15,
        0.35
      ),
      xlab = "X coordinate (m)",
      ylab = "Y coordinate (m)",
      main = paste0(
        "Fitted DBH: ",
        round(
          summary$estimated_dbh_cm,
          2
        ),
        " cm"
      )
    )

    lines(
      fitted_circle$X,
      fitted_circle$Y,
      col = circle_colour,
      lwd = 3
    )

    points(
      circle_centre$X,
      circle_centre$Y,
      pch = 3,
      cex = 1.4,
      lwd = 2,
      col = "#2457A7"
    )

    legend(
      "topright",
      legend = c(
        "TLS slice points",
        "Fitted stem circle",
        "Estimated centre"
      ),
      col = c(
        "grey35",
        circle_colour,
        "#2457A7"
      ),
      pch = c(
        16,
        NA,
        3
      ),
      lty = c(
        NA,
        1,
        NA
      ),
      lwd = c(
        NA,
        3,
        2
      ),
      bty = "n"
    )
  })

  output$quality_table <- renderTable({

    result <- summary_result()

    data.frame(
      Measure = c(
        "Processing status",
        "Quality flag",
        "Method",
        "Circle RMSE",
        "Circumference completeness",
        "Stem points retained"
      ),

      Result = c(
        result$processing_status,
        result$quality_flag,

        ifelse(
          is.na(result$method_used),
          "Unavailable",
          result$method_used
        ),

        ifelse(
          is.finite(result$circle_rmse_mm),
          paste0(
            round(
              result$circle_rmse_mm,
              2
            ),
            " mm"
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(
            result$circumference_completeness_percent
          ),
          paste0(
            round(
              result$circumference_completeness_percent,
              1
            ),
            "%"
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(result$retained_stem_points),
          format(
            result$retained_stem_points,
            big.mark = ",",
            scientific = FALSE
          ),
          "Unavailable"
        )
      ),

      stringsAsFactors = FALSE
    )
  })

  output$complete_result <- renderTable({

    result <- summary_result()

    data.frame(
      Measurement = c(
        "File",
        "Point count",
        "Tree height",
        "Estimated DBH",
        "Basal area",
        "Method",
        "Circle RMSE",
        "Circumference completeness",
        "Retained stem points",
        "Quality flag",
        "Processing status",
        "Error message"
      ),

      Value = c(
        result$file_name,

        ifelse(
          is.finite(result$point_count),
          format(
            result$point_count,
            big.mark = ",",
            scientific = FALSE
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(result$calculated_height_m),
          paste0(
            round(
              result$calculated_height_m,
              3
            ),
            " m"
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(result$estimated_dbh_cm),
          paste0(
            round(
              result$estimated_dbh_cm,
              2
            ),
            " cm"
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(result$basal_area_m2),
          paste0(
            round(
              result$basal_area_m2,
              3
            ),
            " m²"
          ),
          "Unavailable"
        ),

        ifelse(
          is.na(result$method_used),
          "Unavailable",
          result$method_used
        ),

        ifelse(
          is.finite(result$circle_rmse_mm),
          paste0(
            round(
              result$circle_rmse_mm,
              2
            ),
            " mm"
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(
            result$circumference_completeness_percent
          ),
          paste0(
            round(
              result$circumference_completeness_percent,
              1
            ),
            "%"
          ),
          "Unavailable"
        ),

        ifelse(
          is.finite(result$retained_stem_points),
          format(
            result$retained_stem_points,
            big.mark = ",",
            scientific = FALSE
          ),
          "Unavailable"
        ),

        result$quality_flag,
        result$processing_status,

        ifelse(
          is.na(result$error_message),
          "None",
          result$error_message
        )
      ),

      stringsAsFactors = FALSE
    )
  })

  output$download_result <- downloadHandler(

    filename = function() {

      req(input$tls_file)

      paste0(
        tools::file_path_sans_ext(
          input$tls_file$name
        ),
        "_TLS_measurements.csv"
      )
    },

    content = function(file) {

      write.csv(
        summary_result(),
        file,
        row.names = FALSE
      )
    }
  )
}

shinyApp(
  ui = ui,
  server = server
)