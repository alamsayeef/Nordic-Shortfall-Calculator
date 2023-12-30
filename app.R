# libraries ----
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)

# set directory ----
# Get the directory of the currently running app
app_dir = dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the app directory
setwd(app_dir)

# Load nordic life tables ----
load_data = function(file_path) {
  read.csv(paste0("./data/", file_path))
}

countries = c("d", "f", "n", "s")

for (country in countries) {
  assign(paste0(country, "_ref_df"), load_data(paste0(country, "_ref_df_appended.csv")))
  assign(paste0(country, "_mvh_df"), load_data(paste0(country, "_mvh_df.csv")))
}

# load functions ----
# load function to compute life and quality-adjusted life expectancies
source("./scr/compQale.R")

# consistent digits
fRound = function(str, digits = 2, width = 2){
  formatC(str, digits = 2, width = 2, format = "f")
}

# sidebar ----
create_accordion_panel = function(country, prefix, selected = FALSE) {
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

# UI ----
ui <- page_navbar(
  theme = bs_theme(preset = "shiny",
                   "primary" = "#0675DD"),
  lang = "en",
  title = "Nordic Shortfall Calculator",
  sidebar = sidebar(HTML('<p style="font-weight:bold;">Input for</p>'), width = 300, sidebar_acc),
  nav_spacer(),
  # Denmark cards ----
  nav_panel(
    "Denmark",
    h1("Remaining QALYs for Denmark"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:"), card_body(shiny::uiOutput("d_qales_healthy_txt"))),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("d_qales_ill_txt"))),
      card(fill = FALSE, card_header("absolute shortfall:"), card_body(shiny::uiOutput("d_abs_short_txt"))),
      card(fill = FALSE, card_header("proportional shortfall:"), card_body(shiny::uiOutput("d_prop_short_txt"))),
      card(fill = FALSE, card_header("QALY weight:"), card_body(shiny::uiOutput("d_mltplr_txt")))
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
  # Finland cards ----
  nav_panel(
    "Finland",
    h1("Remaining QALYs for Finland"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:"), card_body(shiny::uiOutput("f_qales_healthy_txt"))),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("f_qales_ill_txt"))),
      card(fill = FALSE, card_header("absolute shortfall:"), card_body(shiny::uiOutput("f_abs_short_txt"))),
      card(fill = FALSE, card_header("proportional shortfall:"), card_body(shiny::uiOutput("f_prop_short_txt"))),
      card(fill = FALSE, card_header("QALY weight:"), card_body(shiny::uiOutput("f_mltplr_txt")))
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
  # Norway cards ----
  nav_panel(
    "Norway",
    h1("Remaining QALYs for Norway"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:"), card_body(shiny::uiOutput("n_qales_healthy_txt"))),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("n_qales_ill_txt"))),
      card(fill = FALSE, card_header("absolute shortfall:"), card_body(shiny::uiOutput("n_abs_short_txt"))),
      card(fill = FALSE, card_header("proportional shortfall:"), card_body(shiny::uiOutput("n_prop_short_txt"))),
      card(fill = FALSE, card_header("QALY weight:"), card_body(shiny::uiOutput("n_mltplr_txt")))
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
  # Sweden cards ----
  nav_panel(
    "Sweden",
    h1("Remaining QALYs for Sweden"),
    layout_column_wrap(
      width = 1/5,
      card(fill = FALSE, card_header("without the disease:"), card_body(shiny::uiOutput("s_qales_healthy_txt"))),
      card(fill = FALSE, card_header("with the disease:"), card_body(shiny::uiOutput("s_qales_ill_txt"))),
      card(fill = FALSE, card_header("absolute shortfall:"), card_body(shiny::uiOutput("s_abs_short_txt"))),
      card(fill = FALSE, card_header("proportional shortfall:"), card_body(shiny::uiOutput("s_prop_short_txt"))),
      card(fill = FALSE, card_header("QALY weight:"), card_body(shiny::uiOutput("s_mltplr_txt")))
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

# SERVER ----
server <- function(input, output, session) {
  
  # check and set remaining QALYs ----
  # observeEvent(input$d_start_age,{
  #   max_le_d = 100 - (input$d_start_age)
  #   if(input$d_remaining_qalys > max_le_d){
  #     updateNumericInput(session, "d_remaining_qalys", value = max_le_d, options = list(maximumValue = max_le_d))
  #   }
  #   else{
  #     updateNumericInput(session, "d_remaining_qalys", options = list(maximumValue = max_le_d - 1))
  #   }
  # })
  # 
  # observeEvent(input$f_start_age,{
  #   max_le_f = 100 - (input$f_start_age)
  #   if(input$f_remaining_qalys > max_le_f){
  #     updateNumericInput(session, "f_remaining_qalys", value = max_le_f, options = list(maximumValue = max_le_f))
  #   }
  #   else{
  #     updateNumericInput(session, "f_remaining_qalys", options = list(maximumValue = max_le_f - 1))
  #   }
  # })
  # 
  # observeEvent(input$n_start_age,{
  #   max_le_n = 100 - (input$n_start_age)
  #   if(input$n_remaining_qalys > max_le_n){
  #     updateNumericInput(session, "n_remaining_qalys", value = max_le_n, options = list(maximumValue = max_le_n))
  #   }
  #   else{
  #     updateNumericInput(session, "n_remaining_qalys", options = list(maximumValue = max_le_n - 1))
  #   }
  # })
  # 
  # observeEvent(input$s_start_age,{
  #   max_le_s = 100 - (input$s_start_age)
  #   if(input$s_remaining_qalys > max_le_s){
  #     updateNumericInput(session, "s_remaining_qalys", value = max_le_s, options = list(maximumValue = max_le_s))
  #   }
  #   else{
  #     updateNumericInput(session, "s_remaining_qalys", options = list(maximumValue = max_le_s - 1))
  #   }
  # })
  
  # reactive values for Denmark ----
  d_dat = reactiveValues()
  
  observe({
    
    if(input$d_utils == "d_mvh"){
      util_df = d_mvh_df
      utils = "tto"
    }
    
    if(input$d_utils == "d_vanHout" | input$d_utils== ""){
      util_df = d_ref_df
      utils = "cw"
    }
    
    if(input$d_utils == "d_tto" | input$d_utils== ""){
      util_df = d_ref_df
      utils = "tto"
    }
    
    if(input$d_utils == "d_dsu" ){
      util_df = d_ref_df
      utils = "co"
    }
    
    if(input$d_utils == "d_dsu_2014" ){
      util_df = d_ref_df
      utils = "dsu_2014"
    }
    
    d_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$d_sex_mix/100,
      start_age = input$d_start_age,
      disc_rate = input$d_disc_rate/100,
      utils = utils
    )
    #browser()  
    d_dat$shortfall_abs = d_dat$res$Qx[1] - input$d_remaining_qalys
    
    d_dat$shortfall_prop = d_dat$shortfall_abs / d_dat$res$Qx[1]
    
    d_dat$q_weight = ifelse(
      d_dat$shortfall_prop >= 0.95 | d_dat$shortfall_abs >= 18,
      1.7,
      ifelse(d_dat$shortfall_prop >= 0.85 | d_dat$shortfall_abs >= 12,
             1.2,
             1)
    )
  })
  
  # reactive values for Finland ----
  f_dat = reactiveValues()

  observe({

    if(input$f_utils == "f_mvh"){
      util_df = f_mvh_df
      utils = "tto"
    }

    if(input$f_utils == "f_vanHout" | input$f_utils== ""){
      util_df = f_ref_df
      utils = "cw"
    }

    if(input$f_utils == "f_tto" | input$f_utils== ""){
      util_df = f_ref_df
      utils = "tto"
    }

    if(input$f_utils == "f_dsu" ){
      util_df = f_ref_df
      utils = "co"
    }

    if(input$f_utils == "f_dsu_2014" ){
      util_df = f_ref_df
      utils = "dsu_2014"
    }

    f_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$f_sex_mix/100,
      start_age = input$f_start_age,
      disc_rate = input$f_disc_rate/100,
      utils = utils
    )

    f_dat$shortfall_abs = f_dat$res$Qx[1] - input$f_remaining_qalys

    f_dat$shortfall_prop = f_dat$shortfall_abs / f_dat$res$Qx[1]

    f_dat$q_weight = ifelse(
      f_dat$shortfall_prop >= 0.95 | f_dat$shortfall_abs >= 18,
      1.7,
      ifelse(f_dat$shortfall_prop >= 0.85 | f_dat$shortfall_abs >= 12,
             1.2,
             1)
    )
  })
  
  # reactive values for Norway ----
  n_dat = reactiveValues()

  observe({

    if(input$n_utils == "n_mvh"){
      util_df = n_mvh_df
      utils = "tto"
    }

    if(input$n_utils == "n_vanHout" | input$n_utils== ""){
      util_df = n_ref_df
      utils = "cw"
    }

    if(input$n_utils == "n_tto" | input$n_utils== ""){
      util_df = n_ref_df
      utils = "tto"
    }

    if(input$n_utils == "n_dsu" ){
      util_df = n_ref_df
      utils = "co"
    }

    if(input$n_utils == "n_dsu_2014" ){
      util_df = n_ref_df
      utils = "dsu_2014"
    }

    n_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$n_sex_mix/100,
      start_age = input$n_start_age,
      disc_rate = input$n_disc_rate/100,
      utils = utils
    )

    n_dat$shortfall_abs = n_dat$res$Qx[1] - input$n_remaining_qalys

    n_dat$shortfall_prop = n_dat$shortfall_abs / n_dat$res$Qx[1]

    n_dat$q_weight = ifelse(
      n_dat$shortfall_prop >= 0.95 | n_dat$shortfall_abs >= 18,
      1.7,
      ifelse(n_dat$shortfall_prop >= 0.85 | n_dat$shortfall_abs >= 12,
             1.2,
             1)
    )
  })
  
  # reactive values for Sweden ----
  s_dat = reactiveValues()

  observe({

    if(input$s_utils == "s_mvh"){
      util_df = s_mvh_df
      utils = "tto"
    }

    if(input$s_utils == "s_vanHout" | input$s_utils== ""){
      util_df = s_ref_df
      utils = "cw"
    }

    if(input$s_utils == "s_tto" | input$s_utils== ""){
      util_df = s_ref_df
      utils = "tto"
    }

    if(input$s_utils == "s_dsu" ){
      util_df = s_ref_df
      utils = "co"
    }

    if(input$s_utils == "s_dsu_2014" ){
      util_df = s_ref_df
      utils = "dsu_2014"
    }


    s_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$s_sex_mix/100,
      start_age = input$s_start_age,
      disc_rate = input$s_disc_rate/100,
      utils = utils
    )

    s_dat$shortfall_abs = s_dat$res$Qx[1] - input$s_remaining_qalys

    s_dat$shortfall_prop = s_dat$shortfall_abs / s_dat$res$Qx[1]


    s_dat$q_weight = ifelse(
      s_dat$shortfall_prop >= 0.95 | s_dat$shortfall_abs >= 18,
      1.7,
      ifelse(s_dat$shortfall_prop >= 0.85 | s_dat$shortfall_abs >= 12,
             1.2,
             1)
    )
  })
  
  # country wise output card 1 ----
  output$d_qales_healthy_txt = renderText({fRound(d_dat$res$Qx[1],2)})
  output$f_qales_healthy_txt = renderText({fRound(f_dat$res$Qx[1],2)})
  output$n_qales_healthy_txt = renderText({fRound(n_dat$res$Qx[1],2)})
  output$s_qales_healthy_txt = renderText({fRound(s_dat$res$Qx[1],2)})
  
  # country wise output card 2 ----
  output$d_qales_ill_txt = renderText({fRound(input$d_remaining_qalys)})
  output$f_qales_ill_txt = renderText({fRound(input$f_remaining_qalys)})
  output$n_qales_ill_txt = renderText({fRound(input$n_remaining_qalys)})
  output$s_qales_ill_txt = renderText({fRound(input$s_remaining_qalys)})
  
  # country wise output card 3 ----
  output$d_abs_short_txt = renderText({fRound(d_dat$shortfall_abs,2)})
  output$f_abs_short_txt = renderText({fRound(f_dat$shortfall_abs,2)})
  output$n_abs_short_txt = renderText({fRound(n_dat$shortfall_abs,2)})
  output$s_abs_short_txt = renderText({fRound(s_dat$shortfall_abs,2)})
  
  # country wise output card 4 ----
  output$d_prop_short_txt = renderText({paste0(fRound(d_dat$shortfall_prop*100,2),"%")})
  output$f_prop_short_txt = renderText({paste0(fRound(f_dat$shortfall_prop*100,2),"%")})
  output$n_prop_short_txt = renderText({paste0(fRound(n_dat$shortfall_prop*100,2),"%")})
  output$s_prop_short_txt = renderText({paste0(fRound(s_dat$shortfall_prop*100,2),"%")})
  
  # country wise output card 5 ----
  output$d_mltplr_txt = renderText({paste0("x ",d_dat$q_weight)})
  output$f_mltplr_txt = renderText({paste0("x ",f_dat$q_weight)})
  output$n_mltplr_txt = renderText({paste0("x ",n_dat$q_weight)})
  output$s_mltplr_txt = renderText({paste0("x ",s_dat$q_weight)})
  
  # country wise output card 6 ----
  
  # country wise output card 7 ----
  
  # country wise output card 8 ----
  
  # country wise output card 9 ----
  
  # country wise output card 10 ----
  
  
}

# RUN APP ----
shinyApp(ui, server)
