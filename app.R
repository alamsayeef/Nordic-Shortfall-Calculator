library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)

# set directory
# Get the directory of the currently running app
app_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the app directory
setwd(app_dir)

# Load nordic life tables
load_data <- function(file_path) {
  read.csv(paste0("./data/", file_path))
}

countries <- c("d", "f", "n", "s")

for (country in countries) {
  assign(paste0(country, "_ref_df"), load_data(paste0(country, "_ref_df_appended.csv")))
  assign(paste0(country, "_mvh_df"), load_data(paste0(country, "_mvh_df.csv")))
}

# sidebar
create_accordion_panel <- function(country, prefix, selected = FALSE) {
  accordion_panel(
    country,
    sliderInput(
      paste0(prefix, "_start_age"),
      "Age of the patient population",
      min = 0,
      max = 100,
      value = 0,
      step = 1
    ),
    sliderInput(
      paste0(prefix, "_sex_mix"),
      "% female in the patient population",
      min = 0,
      max = 100,
      value = 50,
      step = 1
    ),
    selectInput(
      paste0(prefix, "_utils"),
      "Select scenario",
      choices = list(
        "Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = paste0(prefix, "_dsu_2014"),
        "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = paste0(prefix, "_dsu"),
        "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = paste0(prefix, "_vanHout"),
        "Alternative C: MVH value set + health state profiles" = paste0(prefix, "_mvh"),
        "Alternative D: MVH value set + HSE 2012+14" = paste0(prefix, "_tto")
      ),
      selected = paste0(prefix, "_dsu_2014")
    ),
    sliderInput(
      paste0(prefix, "_remaining_qalys"),
      "Remaining QALYS",
      min = 0,
      max = 49,
      value = 10,
      step = 1
    ),
    sliderInput(
      paste0(prefix, "_disc_rate"),
      "Discount rate",
      min = 0,
      max = 10,
      value = 1.5,
      step = 0.5
    ),
    selected = selected  # Set selected argument
  )
}

sidebar_acc = accordion(open = F,
  create_accordion_panel("Denmark", "d"),
  create_accordion_panel("Finland", "f"),
  create_accordion_panel("Norway", "n"),
  create_accordion_panel("Sweden", "s")
)

ui <- page_navbar(
  theme = bs_theme(preset = "shiny",
                   "primary" = "#0675DD"),
  lang = "en",
  title = "Nordic Shortfall Calculator",
  sidebar = sidebar(HTML('<p style="font-weight:bold;">Input for</p>'), width = 300, sidebar_acc),
  nav_spacer(),
  nav_panel(
    "Denmark",
    h1("Remaining QALYs"),
    fluidRow(
      column(width = 2, card(id = "d_card1", tags$h5("without the disease:"), width = "150px", height = "150px")),
      column(width = 2, card(id = "d_card2", tags$h5("with the disease:"), width = "150px", height = "150px")),
      column(width = 2, card(id = "d_card3", tags$h5("absolute shortfall:"), width = "150px", height = "150px")),
      column(width = 2, card(id = "d_card4", tags$h5("proportional shortfall:"), width = "150px", height = "150px")),
      column(width = 2, card(id = "d_card5", tags$h5("QALY weight:"), width = "150px", height = "150px"))
    ),
    fluidRow(
      column(3, plotOutput("plot1")),
      column(3, plotOutput("plot2")),
      column(3, plotOutput("plot3"))
    )
  ),
  nav_panel(
    "Finland",
    h1("Remaining QALYs"),
    fluidRow(
      column(width = 2, card(id = "f_card1", tags$h5("without the disease:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "f_card2", tags$h5("with the disease:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "f_card3", tags$h5("absolute shortfall:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "f_card4", tags$h5("proportional shortfall:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "f_card5", tags$h5("QALY weight:"), weight = "150px", height = "150px"))
    ),
    fluidRow(
      column(4, plotOutput("plot1")),
      column(4, plotOutput("plot2")),
      column(4, plotOutput("plot3"))
    )
  ),
  nav_panel(
    "Norway",
    h1("Remaining QALYs"),
    fluidRow(
      column(width = 2, card(id = "n_card1", tags$h5("without the disease:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "n_card2", tags$h5("with the disease:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "n_card3", tags$h5("absolute shortfall:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "n_card4", tags$h5("proportional shortfall:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "n_card5", tags$h5("QALY weight:"), weight = "150px", height = "150px"))
    ),
    fluidRow(
      column(4, plotOutput("plot1")),
      column(4, plotOutput("plot2")),
      column(4, plotOutput("plot3"))
    )
  ),
  nav_panel(
    "Sweden",
    h1("Remaining QALYs"),
    fluidRow(
      column(width = 2, card(id = "s_card1", tags$h5("without the disease:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "s_card2", tags$h5("with the disease:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "s_card3", tags$h5("absolute shortfall:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "s_card4", tags$h5("proportional shortfall:"), weight = "150px", height = "150px")),
      column(width = 2, card(id = "s_card5", tags$h5("QALY weight:"), weight = "150px", height = "150px"))
    ),
    fluidRow(
      column(4, plotOutput("plot1")),
      column(4, plotOutput("plot2")),
      column(4, plotOutput("plot3"))
    )
  )
)

server <- function(input, output, session) {
  observeEvent(list(input$d_remaining_qalys, input$f_remaining_qalys, input$n_remaining_qalys, input$s_remaining_qalys), {
    card(session, id = "d_card2", title = tags$h5("with the disease:"), content = paste0("Remaining QALYs: ", input$d_remaining_qalys))
    updateCard(session, id = "f_card2", title = tags$h5("with the disease:"), content = paste0("Remaining QALYs: ", input$f_remaining_qalys))
    updateCard(session, id = "n_card2", title = tags$h5("with the disease:"), content = paste0("Remaining QALYs: ", input$n_remaining_qalys))
    updateCard(session, id = "s_card2", title = tags$h5("with the disease:"), content = paste0("Remaining QALYs: ", input$s_remaining_qalys))
  })
  
  # Add JavaScript to expand the Denmark accordion panel on tab click
  observe({
    if (!is.null(input$tabName) && input$tabName == "Denmark") {
      session$sendCustomMessage(type = "expandAccordionPanel", payload = "d_accordion_panel")
    }
  })
}

shinyApp(ui, server)
