home_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page home-page",
    htmltools::div(
      class = "hero-panel",
      htmltools::div(
        class = "hero-copy",
        htmltools::span(class = "eyebrow", "INTERACTIVE RESEARCH SOFTWARE"),
        htmltools::h1("LAD-SEIR Interactive Calibration and Forecasting Laboratory"),
        htmltools::p(
          "A professional interface for robust calibration, paired LAD-LSQ sensitivity analysis, simulation, and short-horizon forecasting with time-varying SEIR models."
        ),
        htmltools::div(
          class = "hero-actions",
          shiny::actionButton(ns("open_data"), "Begin with data", class = "btn-primary", icon = app_icon("database")),
          htmltools::a(class = "btn btn-outline-light", href = CORE_REPOSITORY, target = "_blank", app_icon("github"), " Core R package")
        )
      ),
      htmltools::div(
        class = "hero-visual",
        htmltools::img(src = "logo.svg", alt = "LAD-SEIR application mark", class = "hero-logo")
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(
        class = "feature-card",
        bslib::card_body(app_icon("activity", "1.8rem"), htmltools::h3("Mechanistic calibration"), htmltools::p("Fit cosine, exponential, and logistic-decline transmission models to reporting-interval incidence."))
      ),
      bslib::card(
        class = "feature-card",
        bslib::card_body(app_icon("columns-gap", "1.8rem"), htmltools::h3("Paired sensitivity analysis"), htmltools::p("Compare LAD and LSQ under identical multistart optimization settings."))
      ),
      bslib::card(
        class = "feature-card",
        bslib::card_body(app_icon("graph-up-arrow", "1.8rem"), htmltools::h3("Forecasting and simulation"), htmltools::p("Generate predictive intervals and examine realistic reporting anomalies in a controlled simulation laboratory."))
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Analysis workflow"),
        bslib::card_body(
          htmltools::tags$ol(
            class = "workflow-list",
            htmltools::tags$li(htmltools::tags$b("Select data."), " Use a bundled RAPIDD Ebola scenario or upload a reporting-interval series."),
            htmltools::tags$li(htmltools::tags$b("Configure the model."), " Choose a transmission family, calibration criterion, and computational settings."),
            htmltools::tags$li(htmltools::tags$b("Evaluate robustness."), " Compare LAD and LSQ using shared starting values."),
            htmltools::tags$li(htmltools::tags$b("Forecast and document."), " Produce predictive intervals and download reproducible outputs.")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Scientific scope"),
        bslib::card_body(
          htmltools::p("LAD is implemented as a robustness and sensitivity-analysis criterion rather than a universal replacement for LSQ."),
          htmltools::p("The fitted target is incidence accumulated over each reporting interval. The application preserves the statistical and computational definitions implemented in the ", htmltools::code("ladseir"), " package."),
          htmltools::div(class = "scope-links", htmltools::a(href = CORE_REPOSITORY, target = "_blank", app_icon("box-arrow-up-right"), " Package and reproducibility repository"))
        )
      )
    )
  )
}

home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$open_data, {
      bslib::nav_select("main_nav", "Data", session = session$rootScope())
    })
  })
}
