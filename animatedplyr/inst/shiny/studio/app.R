# =============================================================================
# animatedplyr studio — interactive animation designer
#
# Launched by animate_studio(). Lets users pick a dataset and dplyr verb and
# tune every visual aspect of the animation with a live preview, then copy the
# reproducible animate_config() + animate_*() code.
# =============================================================================

library(shiny)
library(animatedplyr)
library(dplyr)
library(rlang)

# ---- available datasets -----------------------------------------------------

.builtin_datasets <- function() {
  ds <- list(
    "mtcars" = function() mtcars
  )
  for (nm in c("kb_df", "lb_df", "mj_df")) {
    if (exists(nm, where = asNamespace("animatedplyr"))) {
      local({
        nm_ <- nm
        ds[[nm_]] <<- function() as.data.frame(get(nm_, asNamespace("animatedplyr")))
      })
    }
  }
  if (requireNamespace("palmerpenguins", quietly = TRUE)) {
    ds[["penguins"]] <- function() as.data.frame(palmerpenguins::penguins)
  }
  ds
}

DATASETS <- .builtin_datasets()

# ---- sensible per-dataset defaults so the preview works immediately ---------

pick_defaults <- function(df) {
  cols     <- colnames(df)
  num_cols <- cols[vapply(df, is.numeric, logical(1))]
  first_num <- if (length(num_cols)) num_cols[1] else cols[1]

  thr <- if (length(num_cols)) {
    signif(stats::median(df[[first_num]], na.rm = TRUE), 3)
  } else NA

  list(
    select_cols = utils::head(cols, 3),
    filter_expr = if (length(num_cols)) paste(first_num, ">", thr) else
      paste0(cols[1], " == '", df[[cols[1]]][1], "'"),
    mutate_name = if (length(num_cols)) paste0(first_num, "_x2") else "new_col",
    mutate_expr = if (length(num_cols)) paste(first_num, "* 2") else "1"
  )
}

# ---- small UI helper: dependency-free HTML5 color input ---------------------

color_input <- function(id, label, value = "#000000") {
  div(class = "ci",
    tags$label(label, `for` = id),
    tags$input(id = id, type = "color", value = value,
               class = "shiny-color-input")
  )
}

# JS: register a Shiny input binding for <input type="color"> (polls until the
# Shiny client is ready, so head/body ordering doesn't matter).
COLOR_BINDING_JS <- HTML("
(function reg(){
  if (!window.Shiny || !Shiny.InputBinding) { return setTimeout(reg, 50); }
  var b = new Shiny.InputBinding();
  $.extend(b, {
    find: function(scope){ return $(scope).find('input.shiny-color-input'); },
    getValue: function(el){ return el.value; },
    setValue: function(el, v){ el.value = v; },
    subscribe: function(el, cb){
      $(el).on('input.ci change.ci', function(){ cb(true); });
    },
    unsubscribe: function(el){ $(el).off('.ci'); },
    getRatePolicy: function(){ return {policy:'debounce', delay:120}; }
  });
  Shiny.inputBindings.register(b, 'animatedplyr.colorInput');
})();
")

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$script(COLOR_BINDING_JS),
    tags$style(HTML("
      .ci { margin-bottom: 8px; }
      .ci label { display:block; font-size:12px; font-weight:600; margin-bottom:2px; }
      .ci input[type=color] { width:100%; height:30px; padding:0;
        border:1px solid #ccc; border-radius:4px; cursor:pointer; }
      .well { background:#fbfbfb; }
      h4.sec { margin-top:4px; border-bottom:2px solid #eee; padding-bottom:4px; }
      .shiny-input-container { margin-bottom: 8px; }
    "))
  ),
  titlePanel("animatedplyr studio"),
  sidebarLayout(
    sidebarPanel(width = 4,

      h4("Data & operation", class = "sec"),
      selectInput("dataset", "Dataset", choices = names(DATASETS)),
      fileInput("upload", "...or upload a CSV", accept = c(".csv")),
      radioButtons("verb", "Verb", inline = TRUE,
                   choices = c("select", "filter", "mutate"), selected = "filter"),
      uiOutput("verb_controls"),
      fluidRow(
        column(6, numericInput("seed", "Seed", value = 1, min = 0, step = 1)),
        column(6, sliderInput("n_rows", "Rows", min = 2, max = 8, value = 5))
      ),
      sliderInput("max_cols", "Columns", min = 2, max = 6, value = 4),

      tags$hr(),
      h4("Typography", class = "sec"),
      selectInput("font_family", "Font family",
                  choices = c("Courier New", "Menlo", "Consolas", "Monaco",
                              "Georgia", "Arial", "Helvetica", "Times New Roman",
                              "Verdana", "Trebuchet MS"),
                  selected = "Courier New"),
      fluidRow(
        column(6, sliderInput("font_size", "Font size", min = 8, max = 26, value = 13)),
        column(6, selectInput("font_weight", "Weight",
                              choices = c("300", "400", "600", "700", "900"),
                              selected = "600"))
      ),

      tags$hr(),
      h4("Timing", class = "sec"),
      fluidRow(
        column(6, sliderInput("duration", "Step (ms)", min = 300, max = 3000,
                              value = 1300, step = 100)),
        column(6, sliderInput("transition", "Transition (ms)", min = 0,
                              max = 1500, value = 600, step = 50))
      ),

      tags$hr(),
      h4("Colors", class = "sec"),
      fluidRow(
        column(6, color_input("canvas_bg",   "Canvas",      "#fafafa")),
        column(6, color_input("title_bg",    "Title bg",    "#111111"))
      ),
      fluidRow(
        column(6, color_input("title_color", "Title text",  "#ffffff")),
        column(6, color_input("header_color","Header text", "#ffffff"))
      ),
      fluidRow(
        column(6, color_input("cell_bg",     "Cell bg",     "#f0f0f0")),
        column(6, color_input("cell_color",  "Cell text",   "#222222"))
      ),
      fluidRow(
        column(6, color_input("border_color","Border",      "#d0d0d0")),
        column(6, color_input("new_color",   "New (mutate)","#2196f3"))
      ),
      fluidRow(
        column(6, color_input("keep_color",  "Keep (filter)","#43a047")),
        column(6, color_input("drop_color",  "Drop (filter)","#e63946"))
      ),

      tags$hr(),
      h4("Box appearance", class = "sec"),
      fluidRow(
        column(6, sliderInput("box_size", "Box width", min = 50, max = 200, value = 95)),
        column(6, sliderInput("border_radius", "Corner radius", min = 0, max = 30, value = 5))
      ),
      fluidRow(
        column(6, sliderInput("border_width", "Border width", min = 0, max = 8,
                              value = 1.5, step = 0.5)),
        column(6, sliderInput("cell_gap", "Spacing", min = 0, max = 30, value = 8))
      ),
      fluidRow(
        column(6, sliderInput("cell_opacity", "Opacity", min = 0.1, max = 1,
                              value = 1, step = 0.05)),
        column(6, sliderInput("canvas_padding", "Padding", min = 0, max = 60, value = 24))
      ),
      checkboxInput("show_disclosure", "Show disclosure pill", value = TRUE)
    ),

    mainPanel(width = 8,
      tabsetPanel(
        tabPanel("Preview",
          br(),
          uiOutput("preview_ui")
        ),
        tabPanel("R code",
          br(),
          helpText("Copy this into a script, vignette, or animate_save_gif() call."),
          verbatimTextOutput("repro_code")
        ),
        tabPanel("Data",
          br(),
          DT::DTOutput("data_table")
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # --- current data frame (built-in or uploaded) ---------------------------
  current_df <- reactive({
    if (!is.null(input$upload)) {
      df <- tryCatch(
        utils::read.csv(input$upload$datapath, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df)) return(df)
    }
    DATASETS[[input$dataset]]()
  })

  # When the dataset/upload changes, rebuild the verb-specific controls with
  # defaults that are valid for that data's columns.
  output$verb_controls <- renderUI({
    df <- current_df()
    d  <- pick_defaults(df)
    cols <- colnames(df)

    if (input$verb == "select") {
      selectizeInput("select_cols", "Columns to select", choices = cols,
                     selected = d$select_cols, multiple = TRUE,
                     width = "100%")
    } else if (input$verb == "filter") {
      textInput("filter_expr", "Condition", value = d$filter_expr,
                width = "100%", placeholder = "e.g. mpg > 20 & cyl == 6")
    } else {
      tagList(
        fluidRow(
          column(5, textInput("mutate_name", "New column", value = d$mutate_name)),
          column(7, textInput("mutate_expr", "Expression", value = d$mutate_expr))
        )
      )
    }
  })

  # --- assemble a config from the controls ---------------------------------
  current_config <- reactive({
    animate_config(
      box_size        = input$box_size,
      font_size       = input$font_size,
      font_family     = input$font_family,
      font_weight     = input$font_weight,
      duration        = input$duration,
      transition      = input$transition,
      show_disclosure = input$show_disclosure,
      max_cols        = input$max_cols,
      canvas_bg       = input$canvas_bg,
      canvas_padding  = input$canvas_padding,
      title_bg        = input$title_bg,
      title_color     = input$title_color,
      cell_bg         = input$cell_bg,
      cell_color      = input$cell_color,
      header_color    = input$header_color,
      border_color    = input$border_color,
      border_width    = input$border_width,
      border_radius   = input$border_radius,
      cell_gap        = input$cell_gap,
      cell_opacity    = input$cell_opacity,
      keep_color      = input$keep_color,
      drop_color      = input$drop_color,
      new_color       = input$new_color
    )
  })

  # --- build the animation (or capture an error message) -------------------
  current_anim <- reactive({
    df  <- current_df()
    cfg <- current_config()
    seed <- if (is.na(input$seed)) NULL else input$seed

    tryCatch({
      if (input$verb == "select") {
        cols <- input$select_cols
        if (is.null(cols) || !length(cols))
          stop("Pick at least one column to select.")
        rlang::inject(animate_select(
          df, !!!rlang::syms(cols),
          n_rows = input$n_rows, seed = seed, config = cfg))
      } else if (input$verb == "filter") {
        req(input$filter_expr)
        cond <- rlang::parse_expr(input$filter_expr)
        rlang::inject(animate_filter(
          df, !!cond,
          n_rows = input$n_rows, seed = seed, config = cfg))
      } else {
        req(input$mutate_name, input$mutate_expr)
        expr <- rlang::parse_expr(input$mutate_expr)
        nm   <- input$mutate_name
        rlang::inject(animate_mutate(
          df, !!nm := !!expr,
          n_rows = input$n_rows, seed = seed, config = cfg))
      }
    }, error = function(e) structure(conditionMessage(e), class = "studio_error"))
  })

  # --- live preview (rendered in an iframe so the embedded JS runs) ---------
  output$preview_ui <- renderUI({
    a <- current_anim()
    if (inherits(a, "studio_error")) {
      return(div(style = "color:#b71c1c; background:#ffebee; border:1px solid #ffcdd2;
                  padding:14px; border-radius:6px; font-family:monospace;",
                 strong("Could not render: "), as.character(a)))
    }
    doc <- paste0(
      "<!DOCTYPE html><html><head><meta charset='utf-8'></head>",
      "<body style='margin:0'>", as.character(a), "</body></html>"
    )
    tags$iframe(srcdoc = doc, style = "width:100%; height:560px; border:0;")
  })

  # --- reproducible code ----------------------------------------------------
  output$repro_code <- renderText({
    cfg <- current_config()
    def <- animate_config()
    # only show config args that differ from the defaults
    args <- character(0)
    for (nm in names(def)) {
      if (nm == "colors") next
      if (!isTRUE(all.equal(cfg[[nm]], def[[nm]]))) {
        val <- cfg[[nm]]
        rhs <- if (is.character(val)) paste0('"', val, '"') else as.character(val)
        args <- c(args, paste0("  ", nm, " = ", rhs))
      }
    }
    cfg_code <- if (length(args)) {
      paste0("cfg <- animate_config(\n", paste(args, collapse = ",\n"), "\n)")
    } else {
      "cfg <- animate_config()"
    }

    data_expr <- if (!is.null(input$upload)) "your_data" else input$dataset
    seed_txt  <- if (is.na(input$seed)) "" else paste0(", seed = ", input$seed)
    nrow_txt  <- paste0(", n_rows = ", input$n_rows)

    call_code <- if (input$verb == "select") {
      paste0("animate_select(", data_expr, ", ",
             paste(input$select_cols, collapse = ", "),
             nrow_txt, seed_txt, ", config = cfg)")
    } else if (input$verb == "filter") {
      paste0("animate_filter(", data_expr, ", ", input$filter_expr,
             nrow_txt, seed_txt, ", config = cfg)")
    } else {
      paste0("animate_mutate(", data_expr, ", ", input$mutate_name, " = ",
             input$mutate_expr, nrow_txt, seed_txt, ", config = cfg)")
    }

    paste0("library(animatedplyr)\n\n", cfg_code, "\n\n", call_code)
  })

  # --- data tab -------------------------------------------------------------
  output$data_table <- DT::renderDT({
    DT::datatable(current_df(), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
}

shinyApp(ui, server)
