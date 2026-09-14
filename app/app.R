# TLS Forest Structure prototype

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
    .batch-layout {
      display: flex;
      align-items: stretch;
    }
    .batch-layout > .col-sm-4,
    .batch-layout > .col-sm-8 {
      float: none;
    }
    .sticky-sidebar {
      max-height: calc(100vh - 30px);
      overflow-y: auto;
      z-index: 10;
    }
    .sticky-sidebar.sidebar-fixed {
      position: fixed;
      top: 15px;
    }
    @media (max-width: 767px) {
      .batch-layout {
        display: block;
      }
      .batch-layout > .col-sm-4,
      .batch-layout > .col-sm-8 {
        float: left;
      }
      .sticky-sidebar {
        position: static;
        max-height: none;
        overflow-y: visible;
      }
    }
  ")),
  tags$script(HTML("
    $(function() {
      var startTop = null;

      function updateBatchSidebar() {
        var sidebar = $('.sticky-sidebar');
        var column = sidebar.parent();

        if (!sidebar.length || !sidebar.is(':visible') || window.innerWidth < 768) {
          sidebar.removeClass('sidebar-fixed').css({left: '', width: ''});
          startTop = null;
          return;
        }

        if (!sidebar.hasClass('sidebar-fixed')) {
          startTop = sidebar.offset().top;
        }

        if (window.pageYOffset > startTop - 15) {
          var columnBox = column[0].getBoundingClientRect();
          sidebar.addClass('sidebar-fixed').css({
            left: (columnBox.left + 15) + 'px',
            width: column.width() + 'px'
          });
        } else {
          sidebar.removeClass('sidebar-fixed').css({left: '', width: ''});
        }
      }

      $(window).off('.tlsSidebar');
      $(window).on('scroll.tlsSidebar resize.tlsSidebar', updateBatchSidebar);
      $('a[data-toggle=tab]').on('shown.bs.tab', function() {
        startTop = null;
        updateBatchSidebar();
      });
      updateBatchSidebar();
    });
  "))),

  div(class = "title-panel",
    h2("TLS Forest Structure"),
    p("Estimate tree height, diameter at breast height and basal area ",
      "from isolated terrestrial LiDAR tree point clouds.")
  ),

  tabsetPanel(id = "analysis_mode",
    tabPanel("Single tree",
      sidebarLayout(
        sidebarPanel(
          h4("Point-cloud input"),
          fileInput("single_file", "Upload one isolated tree",
            accept = c(".las", ".laz")),
          actionButton("analyse_single", "Analyse tree",
            class = "btn-success", width = "100%"),
          br(), br(),
          helpText("The file must contain one isolated tree, include the tree base ",
            "and use metres for XYZ coordinates."),
          tags$hr(),
          strong("Prototype notice"),
          p("Measurements that do not pass the quality checks are marked ",
            "for inspection and should not be treated as validated results.")
        ),
        mainPanel(
          uiOutput("single_message"),
          conditionalPanel(condition = "output.single_complete",
            result_cards("single_"),
            conditionalPanel(condition = "output.single_visual_available",
              div(class = "section-panel",
                h3("Tree point-cloud views"),
                plotOutput("single_projections", height = "520px"),
                div(class = "plot-note",
                  "A reproducible sample of up to 50,000 points is displayed.")
              ),
              div(class = "section-panel",
                h3("DBH cross-section at 1.3 m"),
                plotOutput("single_cross_section", height = "520px")
              ),
              div(class = "section-panel",
                h3("Lower-stem taper"),
                tableOutput("single_taper_table"),
                plotOutput("single_taper_plot", height = "420px"),
                tableOutput("single_taper_summary")
              )
            ),
            div(class = "section-panel",
              h3("Complete result"),
              tableOutput("single_result")
            ),
            downloadButton("download_single", "Download result as CSV")
          )
        )
      )
    ),

    tabPanel("Batch processing",
      div(class = "row batch-layout",
        column(4,
          div(class = "well sticky-sidebar",
            h4("Multiple point clouds"),
            fileInput("batch_files", "Upload isolated-tree LAS or LAZ files",
              multiple = TRUE, accept = c(".las", ".laz")),
            actionButton("analyse_batch", "Analyse all trees",
              class = "btn-success", width = "100%"),
            br(), br(),
            helpText("Each file must contain one isolated tree. Files are processed ",
              "sequentially, so one failed tree does not stop the batch."),
            tags$hr(),
            strong("Inspect a processed tree"),
            uiOutput("batch_tree_selector"),
            helpText("Choose a successful tree after the batch has finished. ",
              "Its measurements and plots will appear below the table.")
          )
        ),
        column(8,
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
            conditionalPanel(condition = "output.batch_visual_available",
              div(class = "section-panel",
                h3(textOutput("selected_tree_heading")),
                result_cards("selected_"),
                plotOutput("selected_projections", height = "520px"),
                plotOutput("selected_cross_section", height = "520px"),
                h3("Lower-stem taper"),
                tableOutput("selected_taper_table"),
                plotOutput("selected_taper_plot", height = "420px"),
                tableOutput("selected_taper_summary")
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
    result <- tryCatch({
      path <- prepare_upload(input$single_file$datapath, input$single_file$name)
      withProgress(message = "Processing the TLS point cloud", value = 0, {
        incProgress(0.4, detail = "Reading coordinates")
        analysed <- process_tls_tree_visual(path)
        analysed$summary$file_name <- input$single_file$name
        incProgress(0.3, detail = "Estimating lower-stem taper")
        if (isTRUE(analysed$success)) {
          analysed$taper <- estimate_stem_taper(path)
        } else {
          analysed$taper <- NULL
        }
        incProgress(0.3, detail = "Preparing results")
        analysed
      })
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      NULL
    })
    single_analysis(result)
  })

  observeEvent(input$analyse_batch, {
    req(input$batch_files)
    invalid <- !tolower(tools::file_ext(input$batch_files$name)) %in% c("las", "laz")
    if (any(invalid)) {
      showNotification("Every uploaded file must be LAS or LAZ.", type = "error")
      return()
    }
    selected_visual(NULL)
    paths <- tryCatch(
      vapply(seq_len(nrow(input$batch_files)), function(i) {
        prepare_upload(input$batch_files$datapath[i], input$batch_files$name[i])
      }, character(1)),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    req(paths)
    names(paths) <- input$batch_files$name
    batch_paths(paths)
    results <- withProgress(message = "Processing TLS files", value = 0, {
      process_tls_batch(unname(paths), names(paths),
        progress_callback = function(i, total, name) {
          incProgress(1 / total,
            detail = paste("File", i, "of", total, ":", name))
        })
    })
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

  output$single_height <- renderText(
    format_value(single_summary()$calculated_height_m, 2, " m"))
  output$single_dbh <- renderText(
    format_value(single_summary()$estimated_dbh_cm, 2, " cm"))
  output$single_basal_area <- renderText(
    format_value(single_summary()$basal_area_m2, 3, " m²"))
  output$single_result <- renderTable(single_summary())

  output$batch_total <- renderText(nrow(batch_results()))
  output$batch_acceptable <- renderText(
    sum(batch_results()$quality_flag == "acceptable"))
  output$batch_attention <- renderText(
    sum(batch_results()$quality_flag != "acceptable"))
  output$batch_table <- renderTable({
    results <- batch_results()
    data.frame(
      File = results$file_name,
      `Height (m)` = round(results$calculated_height_m, 2),
      `DBH (cm)` = round(results$estimated_dbh_cm, 2),
      `Diameter at 6 m (cm)` = round(results$diameter_6m_cm, 2),
      `Taper (cm/m)` = round(results$mean_taper_cm_per_m, 2),
      `Lean (degrees)` = round(results$lower_stem_lean_degrees, 2),
      `DBH quality` = results$quality_flag,
      `Taper quality` = results$taper_quality_flag,
      Status = results$processing_status,
      check.names = FALSE
    )
  }, na = "—")

  output$batch_tree_selector <- renderUI({
    results <- batch_results()
    req(results)
    successful <- results$file_name[results$processing_status == "success"]
    if (length(successful) == 0) {
      return(helpText("No successful tree is available for visual inspection."))
    }
    selectInput(
      "selected_batch_tree",
      "Choose a successfully processed tree",
      choices = c("Select a tree..." = "", successful),
      selected = ""
    )
  })

  observeEvent(input$selected_batch_tree, {
    req(nzchar(input$selected_batch_tree), batch_paths())
    path <- unname(batch_paths()[input$selected_batch_tree])
    req(length(path) == 1, file.exists(path))
    result <- withProgress(
      message = paste("Preparing", input$selected_batch_tree), value = 0, {
        analysed <- process_tls_tree_visual(path)
        analysed$summary$file_name <- input$selected_batch_tree
        if (isTRUE(analysed$success)) {
          analysed$taper <- estimate_stem_taper(path)
        } else {
          analysed$taper <- NULL
        }
        incProgress(1)
        analysed
      })
    selected_visual(result)
  }, ignoreInit = TRUE)

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
    valid <- is.finite(taper$estimated_diameter_cm)

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
      ylim = range(taper$height_m)
    )

    inspect <- valid & taper$quality_flag == "inspect"
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
      legend = c("Estimated diameter", "Inspect fit"),
      col = c("#347847", "#E67E22"),
      pch = c(16, 17),
      lty = c(1, NA),
      bty = "n"
    )
  }

  taper_table_for_display <- function(taper_result) {
    taper <- taper_result$taper_table
    data.frame(
      `Height (m)` = taper$height_m,
      `Diameter (cm)` = round(taper$estimated_diameter_cm, 2),
      `Circle RMSE (mm)` = round(taper$circle_rmse_mm, 2),
      `Circumference (%)` = round(
        taper$circumference_completeness_percent,
        1
      ),
      `Points used` = taper$points_used,
      Quality = taper$quality_flag,
      check.names = FALSE
    )
  }

  taper_summary_for_display <- function(taper_result) {
    summary <- taper_result$summary
    data.frame(
      Measure = c(
        "Mean taper",
        "Linear taper",
        "Lower-stem displacement",
        "Lower-stem lean",
        "Successful measurement heights"
      ),
      Result = c(
        format_value(summary$mean_taper_cm_per_m, 2, " cm/m"),
        format_value(summary$linear_taper_cm_per_m, 2, " cm/m"),
        format_value(summary$lower_stem_displacement_m, 3, " m"),
        format_value(summary$lower_stem_lean_degrees, 2, "°"),
        paste0(
          summary$successful_measurements,
          " of ",
          summary$requested_measurements
        )
      ),
      stringsAsFactors = FALSE
    )
  }

  output$single_projections <- renderPlot({
    req(single_analysis()$success)
    draw_projections(single_analysis())
  })
  output$single_cross_section <- renderPlot({
    req(single_analysis()$success)
    draw_cross_section(single_analysis())
  })
  output$single_taper_table <- renderTable({
    req(single_analysis()$taper)
    taper_table_for_display(single_analysis()$taper)
  }, na = "—")
  output$single_taper_plot <- renderPlot({
    req(single_analysis()$taper)
    draw_taper(single_analysis()$taper)
  })
  output$single_taper_summary <- renderTable({
    req(single_analysis()$taper)
    taper_summary_for_display(single_analysis()$taper)
  })
  output$selected_projections <- renderPlot({
    req(selected_visual()$success)
    draw_projections(selected_visual())
  })
  output$selected_cross_section <- renderPlot({
    req(selected_visual()$success)
    draw_cross_section(selected_visual())
  })
  output$selected_taper_table <- renderTable({
    req(selected_visual()$taper)
    taper_table_for_display(selected_visual()$taper)
  }, na = "—")
  output$selected_taper_plot <- renderPlot({
    req(selected_visual()$taper)
    draw_taper(selected_visual()$taper)
  })
  output$selected_taper_summary <- renderTable({
    req(selected_visual()$taper)
    taper_summary_for_display(selected_visual()$taper)
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
