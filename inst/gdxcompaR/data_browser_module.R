# Data Browser Shiny Module
#
# Browses two categories of GDX data:
#   - Historical/validation data: witchplot's bundled data_historical_values_*.gdx
#   - Model input data: data_{reg_id}/*.gdx found relative to results_dir
#
# Removal: delete this file and remove the three tagged lines:
#   global.R : source("../data_browser_module.R")       # DATA_BROWSER
#   ui.R     : tabPanel("Data Browser", ...)             # DATA_BROWSER
#   server.R : dataBrowserServer("data_browser")         # DATA_BROWSER

# ---- Helpers ----------------------------------------------------------------

# Convert model time index t to calendar year using globals year0/tstep.
.db_t_to_year <- function(t_vals) {
  y0    <- if (exists("year0")) get("year0") else 2005
  tstep <- if (exists("tstep")) get("tstep") else 5
  (as.numeric(t_vals) - 1) * tstep + y0
}

# Detect the "time" column and its type: "year" (calendar), "t" (model index), or NULL.
.db_time_col <- function(d) {
  if ("year" %in% names(d)) return(list(col = "year", type = "year"))
  if ("t"    %in% names(d)) return(list(col = "t",    type = "t"))
  # Heuristic: any non-value column whose numeric values all fall in 1800-2200
  for (col in setdiff(names(d), "value")) {
    vals <- suppressWarnings(as.numeric(as.character(d[[col]])))
    if (!all(is.na(vals)) && min(vals, na.rm = TRUE) >= 1800 &&
        max(vals, na.rm = TRUE) <= 2200)
      return(list(col = col, type = "year"))
  }
  NULL
}

# Build display labels for GDX parameters.
# Historical: "q_emi_valid_primap" -> "q_emi  [primap]"
# Model:      "ssp_ykali"         -> "ssp_ykali"
.db_param_choices <- function(param_names) {
  has_valid <- grepl("_valid_", param_names)
  labels <- ifelse(
    has_valid,
    paste0(gsub("_valid_.*$", "", param_names), "  [",
           gsub("^.*_valid_",  "", param_names), "]"),
    param_names
  )
  setNames(param_names, labels)
}

# Discover GDX files and return a grouped list suitable for selectInput.
# Groups: "Historical data" (bundled) and "Model input (data_{reg_id}/)" (local).
.db_discover_files <- function() {
  result <- list(hist = character(0), model = character(0), model_label = "Model input")

  # 1. Bundled historical files
  pkg_dir <- system.file("data", package = "witchplot")
  if (nchar(pkg_dir) > 0) {
    files <- list.files(pkg_dir, pattern = "data_historical_values_.*\\.gdx$",
                        full.names = TRUE)
    nms   <- gsub("data_historical_values_(.*)\\.gdx$", "\\1", basename(files))
    result$hist <- setNames(files, nms)
  }

  # 2. Local data_{reg_id}/ directory (relative to results_dir or cwd)
  reg   <- if (exists("reg_id"))      get("reg_id")      else NULL
  rdirs <- if (exists("results_dir")) get("results_dir") else NULL
  if (!is.null(reg) && !is.null(rdirs)) {
    data_dirname <- paste0("data_", reg)
    candidates <- unique(c(
      file.path(rdirs[1], data_dirname),
      file.path(dirname(normalizePath(rdirs[1], mustWork = FALSE)), data_dirname),
      file.path(getwd(), data_dirname)
    ))
    local_dir <- candidates[dir.exists(candidates)][1]
    if (length(local_dir) > 0 && !is.na(local_dir)) {
      files <- list.files(local_dir, pattern = "\\.gdx$", full.names = TRUE)
      nms   <- gsub("\\.gdx$", "", basename(files))
      result$model       <- setNames(files, nms)
      result$model_label <- paste0("Model input (", data_dirname, "/)")
    }
  }
  result
}

# ---- UI (split into sidebar + main so they can slot into pageWithSidebar) ---

# Sidebar controls — drop into conditionalPanel inside sidebarPanel
dataBrowserSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    strong("Data source:"),
    selectInput(ns("gdx_file"), NULL,
                choices = character(0), selectize = FALSE, size = 6),
    hr(),
    strong("Parameter:"),
    uiOutput(ns("param_selector")),
    hr(),
    uiOutput(ns("region_filter")),
    uiOutput(ns("extra_filters")),
    uiOutput(ns("time_slider"))
  )
}

# Main panel content — drop into conditionalPanel inside mainPanel
dataBrowserMainUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Plot",  plotOutput(ns("data_plot"),  height = "80vh")),
    tabPanel("Table", br(), uiOutput(ns("table_info")), uiOutput(ns("data_table_ui")))
  )
}

# ---- Server -----------------------------------------------------------------

dataBrowserServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # File inventory (built once at startup)
    file_inventory <- .db_discover_files()

    # Column-name map for historical parameters
    set_deps <- local({
      dep_file <- system.file("data", "historical_data_set_dependencies.rds",
                              package = "witchplot")
      if (file.exists(dep_file)) readRDS(dep_file) else list()
    })

    # Populate file selector with grouped choices
    observe({
      inv <- file_inventory
      ch  <- list()
      if (length(inv$hist)  > 0) ch[["Historical data"]]  <- as.list(inv$hist)
      if (length(inv$model) > 0) ch[[inv$model_label]]    <- as.list(inv$model)
      if (length(ch) == 0) return()
      first <- if (length(inv$model) > 0) inv$model[[1]] else inv$hist[[1]]
      updateSelectInput(session, "gdx_file", choices = ch, selected = first)
    })

    # GDX object for selected file
    gdx_meta <- reactive({
      req(input$gdx_file)
      tryCatch(gdx(input$gdx_file), error = function(e) NULL)
    })

    # Parameter selector
    output$param_selector <- renderUI({
      g <- gdx_meta(); req(!is.null(g))
      ch <- .db_param_choices(g$parameters$name)
      req(length(ch) > 0)
      selectInput(ns("param_name"), NULL,
                  choices = ch, selected = ch[1], selectize = TRUE)
    })

    # Raw data for selected parameter (with named columns applied)
    raw_data <- reactive({
      req(input$param_name, input$gdx_file)
      g <- gdx_meta(); req(!is.null(g))
      d <- tryCatch(g[input$param_name], error = function(e) NULL)
      if (is.null(d) || nrow(d) == 0) return(NULL)

      # Apply set_dependencies names where available (historical files)
      dep_names <- set_deps[[input$param_name]]
      if (!is.null(dep_names) && length(dep_names) == ncol(d) - 1)
        colnames(d)[seq_along(dep_names)] <- dep_names

      d
    })

    # Summarise dimension structure
    dim_info <- reactive({
      d <- raw_data(); req(d)
      tc   <- .db_time_col(d)
      cols <- setdiff(names(d), "value")
      list(
        time  = tc,
        has_n = "n" %in% cols,
        extra = setdiff(cols, c(if (!is.null(tc)) tc$col, "n"))
      )
    })

    # ---- Dynamic filter UIs -------------------------------------------------

    output$region_filter <- renderUI({
      info <- dim_info()
      if (!info$has_n) return(NULL)
      d       <- raw_data()
      regions <- sort(unique(as.character(d$n)))
      tagList(
        strong("Region:"),
        selectInput(ns("n_filter"), NULL,
                    choices   = regions,
                    selected  = regions[1:min(5, length(regions))],
                    multiple  = TRUE,
                    size      = min(8, length(regions)),
                    selectize = FALSE)
      )
    })

    output$extra_filters <- renderUI({
      d    <- raw_data(); req(d)
      info <- dim_info()
      if (length(info$extra) == 0) return(NULL)
      tagList(lapply(info$extra, function(col) {
        vals <- sort(unique(as.character(d[[col]])))
        tagList(
          strong(paste0(col, ":")),
          selectInput(ns(paste0("filter_", col)), NULL,
                      choices   = c("(all)" = "__all__", setNames(vals, vals)),
                      selected  = "__all__",
                      multiple  = TRUE,
                      size      = min(6, length(vals) + 1),
                      selectize = FALSE)
        )
      }))
    })

    output$time_slider <- renderUI({
      info <- dim_info()
      if (is.null(info$time)) return(NULL)
      d      <- raw_data()
      tc     <- info$time
      raw_t  <- suppressWarnings(as.numeric(as.character(d[[tc$col]])))
      yr     <- if (tc$type == "t") .db_t_to_year(raw_t) else raw_t
      yr     <- yr[!is.na(yr)]
      yr_min <- min(yr); yr_max <- max(yr)
      step   <- if (tc$type == "t") .db_t_to_year(2) - .db_t_to_year(1) else 1
      tagList(
        strong("Year range:"),
        sliderInput(ns("year_range"), NULL,
                    min   = yr_min, max = yr_max,
                    value = c(max(yr_min, 1960), yr_max),
                    step  = step, sep = "")
      )
    })

    # ---- Filtered data ------------------------------------------------------

    filtered_data <- reactive({
      d    <- raw_data(); req(d)
      info <- dim_info()

      if (!is.null(info$time) && !is.null(input$year_range)) {
        tc  <- info$time
        raw <- suppressWarnings(as.numeric(as.character(d[[tc$col]])))
        yr  <- if (tc$type == "t") .db_t_to_year(raw) else raw
        d   <- d[!is.na(yr) & yr >= input$year_range[1] & yr <= input$year_range[2], ]
      }
      if (info$has_n && !is.null(input$n_filter) && length(input$n_filter) > 0)
        d <- d[d$n %in% input$n_filter, ]
      for (col in info$extra) {
        sel <- input[[paste0("filter_", col)]]
        if (!is.null(sel) && !("__all__" %in% sel))
          d <- d[d[[col]] %in% sel, ]
      }
      d
    })

    # ---- Plot ---------------------------------------------------------------

    output$data_plot <- renderPlot({
      d    <- filtered_data(); req(d, nrow(d) > 0)
      info <- dim_info()

      if (!is.null(info$time)) {
        tc     <- info$time
        raw_t  <- suppressWarnings(as.numeric(as.character(d[[tc$col]])))
        d$year <- if (tc$type == "t") .db_t_to_year(raw_t) else raw_t
        d      <- d[!is.na(d$year), ]

        active_extra <- Filter(function(col) length(unique(d[[col]])) > 1, info$extra)

        # Aggregate over extra dims if too many combinations
        if (length(active_extra) > 0) {
          n_combos <- prod(sapply(active_extra, function(col) length(unique(d[[col]]))))
          if (n_combos > 12) {
            grp <- c("year", if (info$has_n) "n")
            d   <- d %>%
              group_by(across(all_of(grp))) %>%
              summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
            active_extra <- character(0)
          }
        }

        color_col <- if (info$has_n && length(unique(d$n)) > 1) "n" else
                     if (length(active_extra) > 0) active_extra[1] else NULL

        p <- ggplot(d, aes(x = year, y = value))
        if (!is.null(color_col)) p <- p + aes(color = .data[[color_col]])
        p <- p + geom_line(linewidth = 1) +
          xlab("Year") + ylab("Value") +
          theme(text            = element_text(size = 14),
                legend.position = "bottom",
                legend.title    = element_blank()) +
          guides(color = guide_legend(nrow = 4))
        if (!is.null(color_col) && length(unique(d[[color_col]])) > 20)
          p <- p + theme(legend.position = "none")

      } else {
        x_col <- if (info$has_n) "n" else setdiff(names(d), "value")[1]
        p <- ggplot(d, aes(x = .data[[x_col]], y = value)) +
          geom_col(fill = "steelblue") +
          xlab(x_col) + ylab("Value") +
          theme(text = element_text(size = 14),
                axis.text.x = element_text(angle = 45, hjust = 1))
      }
      print(p)
    })

    # ---- Table --------------------------------------------------------------

    output$table_info <- renderUI({
      d <- filtered_data(); req(d)
      tags$p(style = "color: grey;",
             sprintf("%d rows × %d columns", nrow(d), ncol(d)))
    })

    output$data_table_ui <- renderUI({
      if (requireNamespace("DT", quietly = TRUE))
        DT::dataTableOutput(ns("data_table_dt"))
      else
        tagList(
          tags$p(style = "color: grey; font-size: 0.85em;",
                 "(Install the DT package for an interactive table)"),
          tableOutput(ns("data_table_plain"))
        )
    })

    if (requireNamespace("DT", quietly = TRUE)) {
      output$data_table_dt <- DT::renderDataTable({
        d <- filtered_data(); req(d)
        DT::datatable(d, options = list(pageLength = 20, scrollX = TRUE),
                      rownames = FALSE)
      })
    }

    output$data_table_plain <- renderTable({
      d <- filtered_data(); req(d)
      head(d, 500)
    })

  })
}
