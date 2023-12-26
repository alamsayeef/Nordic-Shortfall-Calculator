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
    h1("Remaining QALYs for Denmark"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:")),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("d_with_disease_card"))),
      card(fill = FALSE, card_header("absolute shortfall:")),
      card(fill = FALSE, card_header("proportional shortfall:")),
      card(fill = FALSE, card_header("QALY weight:"))
    ),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("Absolute shortfall")),
      card(fill = FALSE, card_header("Proportional shortfall")),
      card(fill = FALSE, card_header("Cummulative QALYs")),
      card(fill = FALSE, card_header("HRQoL by year")),
      card(fill = FALSE, card_header("Cummulative Survival"))
    )
  ),
  nav_panel(
    "Finland",
    h1("Remaining QALYs for Finland"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:")),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("f_with_disease_card"))),
      card(fill = FALSE, card_header("absolute shortfall:")),
      card(fill = FALSE, card_header("proportional shortfall:")),
      card(fill = FALSE, card_header("QALY weight:"))
    ),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("Absolute shortfall")),
      card(fill = FALSE, card_header("Proportional shortfall")),
      card(fill = FALSE, card_header("Cummulative QALYs")),
      card(fill = FALSE, card_header("HRQoL by year")),
      card(fill = FALSE, card_header("Cummulative Survival"))
    )
  ),
  nav_panel(
    "Norway",
    h1("Remaining QALYs for Norway"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:")),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("n_with_disease_card"))),
      card(fill = FALSE, card_header("absolute shortfall:")),
      card(fill = FALSE, card_header("proportional shortfall:")),
      card(fill = FALSE, card_header("QALY weight:"))
    ),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("Absolute shortfall")),
      card(fill = FALSE, card_header("Proportional shortfall")),
      card(fill = FALSE, card_header("Cummulative QALYs")),
      card(fill = FALSE, card_header("HRQoL by year")),
      card(fill = FALSE, card_header("Cummulative Survival"))
    )
  ),
  nav_panel(
    "Sweden",
    h1("Remaining QALYs for Sweden"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:")),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("s_with_disease_card"))),
      card(fill = FALSE, card_header("absolute shortfall:")),
      card(fill = FALSE, card_header("proportional shortfall:")),
      card(fill = FALSE, card_header("QALY weight:"))
    ),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("Absolute shortfall")),
      card(fill = FALSE, card_header("Proportional shortfall")),
      card(fill = FALSE, card_header("Cummulative QALYs")),
      card(fill = FALSE, card_header("HRQoL by year")),
      card(fill = FALSE, card_header("Cummulative Survival"))
    )
  )
)

server <- function(input, output, session) {
  output$d_with_disease_card = renderText({input$d_remaining_qalys})
  output$f_with_disease_card = renderText({input$f_remaining_qalys})
  output$n_with_disease_card = renderText({input$n_remaining_qalys})
  output$s_with_disease_card = renderText({input$s_remaining_qalys})
  
}

shinyApp(ui, server)
