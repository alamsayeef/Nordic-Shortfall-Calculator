# libraries ----
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)
library(highcharter)

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

# Function to create Highchart
create_highchart <- function(plot_df, title, ytitle, y_max, color = "#7cb5ec") {
  highchart(
    hc_opts = list(),
    theme = getOption("highcharter.theme"),
    type = "chart",
    width = NULL,
    height = NULL,
    elementId = NULL,
    google_fonts = getOption("highcharter.google_fonts")
  ) %>%
    hc_add_series(
      plot_df, type = "area",
      name = "Shortfall", color = color,
      hcaes(x = "age", y = "var"),
      tooltip = list(enabled = FALSE),
      fast = TRUE
    ) %>%
    hc_title(
      text = title,
      y = 60, x = -50,
      style = list(fontSize = "16px")
    ) %>%
    hc_plotOptions(
      line = list(
        marker = list(
          enabled = FALSE
        )
      ),
      series = list(
        tooltip = list(
          enabled = TRUE,
          followPointer = TRUE,
          fillColor = "transparent"
        )
      ),
      area = list(
        states = list(
          hover = list(
            enabled = TRUE
          )
        ),
        marker = list(
          enabled = FALSE,
          fillColor = "blue",
          width = 1,
          height = 1,
          enabledThreshold = 10,
          radius = 1
        )
      )
    ) %>%
    hc_xAxis(
      title = list(text = "Age"),
      gridLineColor = 'lightgray',
      gridLineWidth = 1,
      gridLineDashStyle = "Dot",
      tickLength = 10,
      tickWidth = 2,
      tickmarkPlacement = 'between'
    ) %>%
    hc_yAxis(
      title = list(text = ytitle),
      max = y_max
    ) %>%
    hc_tooltip(
      enabled = TRUE,
      valueDecimals = 2,
      pointFormat = '{point.y} ',
      valueSuffix = ' '
    ) %>%
    hc_chart(
      style = list(
        fontFamily = "Inter"
      )
    ) %>%
    hc_legend(
      enabled = FALSE
    )
}

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
      card(fill = FALSE, card_header("without the disease:"), card_body(uiOutput("d_qales_healthy_txt"))),
      card(fill = FALSE, card_header("with the disease:"), card_body(uiOutput("d_qales_ill_txt"))),
      card(fill = FALSE, card_header("absolute shortfall:"), card_body(uiOutput("d_abs_short_txt"))),
      card(fill = FALSE, card_header("proportional shortfall:"), card_body(uiOutput("d_prop_short_txt"))),
      card(fill = FALSE, card_header("QALY weight:"), card_body(uiOutput("d_mltplr_txt")))
    ),
    layout_column_wrap(
      width = 1/2,
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Shortfall",
        nav_panel("Absolute", card_body(highchartOutput("d_hc_as"))),
        nav_panel("Proportional", card_body(highchartOutput("d_hc_ps")))),
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Other",
        nav_panel("Cumulative QALYs", card_body(highchartOutput("d_hc_cq"))),
        nav_panel("HRQoL by year", card_body(highchartOutput("d_hc_hrqol"))),
        nav_panel("Cumulative Survival", card_body(highchartOutput("d_hc_cs")))),
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
      width = 1/2,
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Shortfall",
        nav_panel("Absolute", card_body(highchartOutput("f_hc_as"))),
        nav_panel("Proportional", card_body(highchartOutput("f_hc_ps")))),
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Other",
        nav_panel("Cumulative QALYs", card_body(highchartOutput("f_hc_cq"))),
        nav_panel("HRQoL by year", card_body(highchartOutput("f_hc_hrqol"))),
        nav_panel("Cumulative Survival", card_body(highchartOutput("f_hc_cs")))),
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
      width = 1/2,
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Shortfall",
        nav_panel("Absolute", card_body(highchartOutput("n_hc_as"))),
        nav_panel("Proportional", card_body(highchartOutput("n_hc_ps")))),
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Other",
        nav_panel("Cumulative QALYs", card_body(highchartOutput("n_hc_cq"))),
        nav_panel("HRQoL by year", card_body(highchartOutput("n_hc_hrqol"))),
        nav_panel("Cumulative Survival", card_body(highchartOutput("n_hc_cs")))),
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
      width = 1/2,
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Shortfall",
        nav_panel("Absolute", card_body(highchartOutput("s_hc_as"))),
        nav_panel("Proportional", card_body(highchartOutput("s_hc_ps")))),
      navset_card_tab(
        height = 450,
        full_screen = TRUE,
        title = "Other",
        nav_panel("Cumulative QALYs", card_body(highchartOutput("s_hc_cq"))),
        nav_panel("HRQoL by year", card_body(highchartOutput("s_hc_hrqol"))),
        nav_panel("Cumulative Survival", card_body(highchartOutput("s_hc_cs")))),
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
  
  # absolute shortfall highchart
  highchart_d_as <- reactive({
    if (d_dat$shortfall_abs < 0) {
      p_error <- highchart() %>%
        hc_title(
          text = "Error: QALYs must be lower with the disease.",
          align = "center",
          x = -10,
          verticalAlign = 'middle',
          floating = TRUE,
          style = list(
            fontSize = "16px",
            color = "#7cb5ec"
          )
        )
      return(p_error)
    } else {
      short_fall <- data.frame(
        name = c("QALYs with disease", "Absolute shortfall", "QALYs without disease"),
        value = c(input$d_remaining_qalys, d_dat$shortfall_abs, max(d_dat$res$Qx[1])),
        color = c("#7cb5ec", "#6d757d", "#3e6386"),
        a = c(FALSE, FALSE, TRUE)
      )
      
      shortfall_str <- paste0("Absolute QALY shortfall:<b>", round(d_dat$shortfall_abs, 2), "</b>")
      
      p1 <- highchart() %>%
        hc_add_series(
          data = short_fall, "waterfall",
          pointPadding = "0",
          hcaes(
            name = name,
            y = value,
            isSum = a,
            color = color
          ),
          name = "QALYs"
        ) %>%
        hc_chart(
          style = list(
            fontFamily = "Inter"
          )
        ) %>%
        hc_tooltip(
          valueDecimals = 2
        ) %>%
        hc_xAxis(
          categories = short_fall$name
        ) %>%
        hc_boost(enabled = FALSE)
      
      return(p1)
    }
  })
  
  # proportional shortfall highchart
  highchart_d_ps <- reactive({
    short_fall <- data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - d_dat$shortfall_prop * 100, d_dat$shortfall_prop * 100),
      col = c("green", "gray")
    )
    
    shortfall_str <- paste0("Proportional<br>QALY<br>shortfall:<br><b>", round(d_dat$shortfall_prop * 100, 1), "%")
    
    p1 <- create_highchart(
      plot_df = short_fall,
      title = shortfall_str,
      ytitle = "QALE",
      y_max = NULL,
      color = c("#7cb5ec", "gray")
    )
    
    return(p1)
  })
  
  # cumulative QALY highchart
  highchart_d_cq <- reactive({
    disc_str <- input$d_disc_rate > 0
    y_max <- max(d_dat$res$Qx[1])
    title <- paste0("QALYs without the disease: <b>", round(max(d_dat$res$Qx[1]), 2), "</b>", ifelse(disc_str, "(discounted)", ""))
    ytitle <- "Cumulative QALYs"
    
    plot_df <- data.frame(
      age = d_dat$res$age,
      var = d_dat$res[, 6]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # HRQoL highchart
  highchart_d_hrqol <- reactive({
    disc_str <- input$d_disc_rate > 0
    y_max <- max(d_dat$res$Qx[1])
    title <- paste0("HRQoL over the lifecourse", ifelse(disc_str, "(undiscounted)", ""))
    ytitle <- "EQ-5D score"
    y_max <- 1
    
    plot_df <- data.frame(
      age = d_dat$res$age,
      var = d_dat$res[, 2]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # cumulative survival highchart
  highchart_d_cs <- reactive({
    title <- paste0("Cumulative survival")
    ytitle <- "S(t)"
    y_max <- 1
    
    plot_df <- data.frame(
      age = d_dat$res$age,
      var = d_dat$res[, 5]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
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
  
  # absolute shortfall highchart
  highchart_f_as <- reactive({
    if (f_dat$shortfall_abs < 0) {
      p_error <- highchart() %>%
        hc_title(
          text = "Error: QALYs must be lower with the disease.",
          align = "center",
          x = -10,
          verticalAlign = 'middle',
          floating = TRUE,
          style = list(
            fontSize = "16px",
            color = "#7cb5ec"
          )
        )
      return(p_error)
    } else {
      short_fall <- data.frame(
        name = c("QALYs with disease", "Absolute shortfall", "QALYs without disease"),
        value = c(input$f_remaining_qalys, f_dat$shortfall_abs, max(f_dat$res$Qx[1])),
        color = c("#7cb5ec", "#6d757d", "#3e6386"),
        a = c(FALSE, FALSE, TRUE)
      )
      
      shortfall_str <- paste0("Absolute QALY shortfall:<b>", round(f_dat$shortfall_abs, 2), "</b>")
      
      p1 <- highchart() %>%
        hc_add_series(
          data = short_fall, "waterfall",
          pointPadding = "0",
          hcaes(
            name = name,
            y = value,
            isSum = a,
            color = color
          ),
          name = "QALYs"
        ) %>%
        hc_chart(
          style = list(
            fontFamily = "Inter"
          )
        ) %>%
        hc_tooltip(
          valueDecimals = 2
        ) %>%
        hc_xAxis(
          categories = short_fall$name
        ) %>%
        hc_boost(enabled = FALSE)
      
      return(p1)
    }
  })
  
  # proportional shortfall highchart
  highchart_f_ps <- reactive({
    short_fall <- data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - f_dat$shortfall_prop * 100, f_dat$shortfall_prop * 100),
      col = c("green", "gray")
    )
    
    shortfall_str <- paste0("Proportional<br>QALY<br>shortfall:<br><b>", round(f_dat$shortfall_prop * 100, 1), "%")
    
    p1 <- create_highchart(
      plot_df = short_fall,
      title = shortfall_str,
      ytitle = "QALE",
      y_max = NULL,
      color = c("#7cb5ec", "gray")
    )
    
    return(p1)
  })
  
  # cumulative QALY highchart
  highchart_f_cq <- reactive({
    disc_str <- input$f_disc_rate > 0
    y_max <- max(f_dat$res$Qx[1])
    title <- paste0("QALYs without the disease: <b>", round(max(f_dat$res$Qx[1]), 2), "</b>", ifelse(disc_str, "(discounted)", ""))
    ytitle <- "Cumulative QALYs"
    
    plot_df <- data.frame(
      age = f_dat$res$age,
      var = f_dat$res[, 6]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # HRQoL highchart
  highchart_f_hrqol <- reactive({
    disc_str <- input$f_disc_rate > 0
    y_max <- max(f_dat$res$Qx[1])
    title <- paste0("HRQoL over the lifecourse", ifelse(disc_str, "(undiscounted)", ""))
    ytitle <- "EQ-5D score"
    y_max <- 1
    
    plot_df <- data.frame(
      age = f_dat$res$age,
      var = f_dat$res[, 2]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # cumulative survival highchart
  highchart_f_cs <- reactive({
    title <- paste0("Cumulative survival")
    ytitle <- "S(t)"
    y_max <- 1
    
    plot_df <- data.frame(
      age = f_dat$res$age,
      var = f_dat$res[, 5]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
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
  
  # absolute shortfall highchart
  highchart_n_as <- reactive({
    if (n_dat$shortfall_abs < 0) {
      p_error <- highchart() %>%
        hc_title(
          text = "Error: QALYs must be lower with the disease.",
          align = "center",
          x = -10,
          verticalAlign = 'middle',
          floating = TRUE,
          style = list(
            fontSize = "16px",
            color = "#7cb5ec"
          )
        )
      return(p_error)
    } else {
      short_fall <- data.frame(
        name = c("QALYs with disease", "Absolute shortfall", "QALYs without disease"),
        value = c(input$n_remaining_qalys, n_dat$shortfall_abs, max(n_dat$res$Qx[1])),
        color = c("#7cb5ec", "#6d757d", "#3e6386"),
        a = c(FALSE, FALSE, TRUE)
      )
      
      shortfall_str <- paste0("Absolute QALY shortfall:<b>", round(n_dat$shortfall_abs, 2), "</b>")
      
      p1 <- highchart() %>%
        hc_add_series(
          data = short_fall, "waterfall",
          pointPadding = "0",
          hcaes(
            name = name,
            y = value,
            isSum = a,
            color = color
          ),
          name = "QALYs"
        ) %>%
        hc_chart(
          style = list(
            fontFamily = "Inter"
          )
        ) %>%
        hc_tooltip(
          valueDecimals = 2
        ) %>%
        hc_xAxis(
          categories = short_fall$name
        ) %>%
        hc_boost(enabled = FALSE)
      
      return(p1)
    }
  })
  
  # proportional shortfall highchart
  highchart_n_ps <- reactive({
    short_fall <- data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - n_dat$shortfall_prop * 100, n_dat$shortfall_prop * 100),
      col = c("green", "gray")
    )
    
    shortfall_str <- paste0("Proportional<br>QALY<br>shortfall:<br><b>", round(n_dat$shortfall_prop * 100, 1), "%")
    
    p1 <- create_highchart(
      plot_df = short_fall,
      title = shortfall_str,
      ytitle = "QALE",
      y_max = NULL,
      color = c("#7cb5ec", "gray")
    )
    
    return(p1)
  })
  
  # cumulative QALY highchart
  highchart_n_cq <- reactive({
    disc_str <- input$n_disc_rate > 0
    y_max <- max(n_dat$res$Qx[1])
    title <- paste0("QALYs without the disease: <b>", round(max(n_dat$res$Qx[1]), 2), "</b>", ifelse(disc_str, "(discounted)", ""))
    ytitle <- "Cumulative QALYs"
    
    plot_df <- data.frame(
      age = n_dat$res$age,
      var = n_dat$res[, 6]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # HRQoL highchart
  highchart_n_hrqol <- reactive({
    disc_str <- input$n_disc_rate > 0
    y_max <- max(n_dat$res$Qx[1])
    title <- paste0("HRQoL over the lifecourse", ifelse(disc_str, "(undiscounted)", ""))
    ytitle <- "EQ-5D score"
    y_max <- 1
    
    plot_df <- data.frame(
      age = n_dat$res$age,
      var = n_dat$res[, 2]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # cumulative survival highchart
  highchart_n_cs <- reactive({
    title <- paste0("Cumulative survival")
    ytitle <- "S(t)"
    y_max <- 1
    
    plot_df <- data.frame(
      age = n_dat$res$age,
      var = n_dat$res[, 5]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
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
  
  # absolute shortfall highchart
  highchart_s_as <- reactive({
    if (s_dat$shortfall_abs < 0) {
      p_error <- highchart() %>%
        hc_title(
          text = "Error: QALYs must be lower with the disease.",
          align = "center",
          x = -10,
          verticalAlign = 'middle',
          floating = TRUE,
          style = list(
            fontSize = "16px",
            color = "#7cb5ec"
          )
        )
      return(p_error)
    } else {
      short_fall <- data.frame(
        name = c("QALYs with disease", "Absolute shortfall", "QALYs without disease"),
        value = c(input$s_remaining_qalys, s_dat$shortfall_abs, max(s_dat$res$Qx[1])),
        color = c("#7cb5ec", "#6d757d", "#3e6386"),
        a = c(FALSE, FALSE, TRUE)
      )
      
      shortfall_str <- paste0("Absolute QALY shortfall:<b>", round(s_dat$shortfall_abs, 2), "</b>")
      
      p1 <- highchart() %>%
        hc_add_series(
          data = short_fall, "waterfall",
          pointPadding = "0",
          hcaes(
            name = name,
            y = value,
            isSum = a,
            color = color
          ),
          name = "QALYs"
        ) %>%
        hc_chart(
          style = list(
            fontFamily = "Inter"
          )
        ) %>%
        hc_tooltip(
          valueDecimals = 2
        ) %>%
        hc_xAxis(
          categories = short_fall$name
        ) %>%
        hc_boost(enabled = FALSE)
      
      return(p1)
    }
  })
  
  # proportional shortfall highchart
  highchart_s_ps <- reactive({
    short_fall <- data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - s_dat$shortfall_prop * 100, s_dat$shortfall_prop * 100),
      col = c("green", "gray")
    )
    
    shortfall_str <- paste0("Proportional<br>QALY<br>shortfall:<br><b>", round(s_dat$shortfall_prop * 100, 1), "%")
    
    p1 <- create_highchart(
      plot_df = short_fall,
      title = shortfall_str,
      ytitle = "QALE",
      y_max = NULL,
      color = c("#7cb5ec", "gray")
    )
    
    return(p1)
  })
  
  # cumulative QALY highchart
  highchart_s_cq <- reactive({
    disc_str <- input$s_disc_rate > 0
    y_max <- max(s_dat$res$Qx[1])
    title <- paste0("QALYs without the disease: <b>", round(max(s_dat$res$Qx[1]), 2), "</b>", ifelse(disc_str, "(discounted)", ""))
    ytitle <- "Cumulative QALYs"
    
    plot_df <- data.frame(
      age = s_dat$res$age,
      var = s_dat$res[, 6]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # HRQoL highchart
  highchart_s_hrqol <- reactive({
    disc_str <- input$s_disc_rate > 0
    y_max <- max(s_dat$res$Qx[1])
    title <- paste0("HRQoL over the lifecourse", ifelse(disc_str, "(undiscounted)", ""))
    ytitle <- "EQ-5D score"
    y_max <- 1
    
    plot_df <- data.frame(
      age = s_dat$res$age,
      var = s_dat$res[, 2]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # cumulative survival highchart
  highchart_s_cs <- reactive({
    title <- paste0("Cumulative survival")
    ytitle <- "S(t)"
    y_max <- 1
    
    plot_df <- data.frame(
      age = s_dat$res$age,
      var = s_dat$res[, 5]
    )
    
    p1 <- create_highchart(
      plot_df = plot_df,
      title = title,
      ytitle = ytitle,
      y_max = y_max,
      color = "#7cb5ec"
    )
    
    return(p1)
  })
  
  # numeric outputs ----
  # country wise output card 1
  output$d_qales_healthy_txt = renderText({fRound(d_dat$res$Qx[1],2)})
  output$f_qales_healthy_txt = renderText({fRound(f_dat$res$Qx[1],2)})
  output$n_qales_healthy_txt = renderText({fRound(n_dat$res$Qx[1],2)})
  output$s_qales_healthy_txt = renderText({fRound(s_dat$res$Qx[1],2)})
  
  # country wise output card 2
  output$d_qales_ill_txt = renderText({fRound(input$d_remaining_qalys)})
  output$f_qales_ill_txt = renderText({fRound(input$f_remaining_qalys)})
  output$n_qales_ill_txt = renderText({fRound(input$n_remaining_qalys)})
  output$s_qales_ill_txt = renderText({fRound(input$s_remaining_qalys)})
  
  # country wise output card 3
  output$d_abs_short_txt = renderText({fRound(d_dat$shortfall_abs,2)})
  output$f_abs_short_txt = renderText({fRound(f_dat$shortfall_abs,2)})
  output$n_abs_short_txt = renderText({fRound(n_dat$shortfall_abs,2)})
  output$s_abs_short_txt = renderText({fRound(s_dat$shortfall_abs,2)})
  
  # country wise output card 4
  output$d_prop_short_txt = renderText({paste0(fRound(d_dat$shortfall_prop*100,2),"%")})
  output$f_prop_short_txt = renderText({paste0(fRound(f_dat$shortfall_prop*100,2),"%")})
  output$n_prop_short_txt = renderText({paste0(fRound(n_dat$shortfall_prop*100,2),"%")})
  output$s_prop_short_txt = renderText({paste0(fRound(s_dat$shortfall_prop*100,2),"%")})
  
  # country wise output card 5
  output$d_mltplr_txt = renderText({paste0("x ",d_dat$q_weight)})
  output$f_mltplr_txt = renderText({paste0("x ",f_dat$q_weight)})
  output$n_mltplr_txt = renderText({paste0("x ",n_dat$q_weight)})
  output$s_mltplr_txt = renderText({paste0("x ",s_dat$q_weight)})
  
  # highcharts ----
  # country wise output card 6
  output$d_hc_as = renderHighchart({highchart_d_as()})
  output$f_hc_as = renderHighchart({highchart_f_as()})
  output$n_hc_as = renderHighchart({highchart_n_as()})
  output$s_hc_as = renderHighchart({highchart_s_as()})
  
  # country wise output card 7
  output$d_hc_ps = renderHighchart({highchart_d_ps()})
  output$f_hc_ps = renderHighchart({highchart_f_ps()})
  output$n_hc_ps = renderHighchart({highchart_n_ps()})
  output$s_hc_ps = renderHighchart({highchart_s_ps()})
  
  # country wise output card 8
  output$d_hc_cq = renderHighchart({highchart_d_cq()})
  output$f_hc_cq = renderHighchart({highchart_f_cq()})
  output$n_hc_cq = renderHighchart({highchart_n_cq()})
  output$s_hc_cq = renderHighchart({highchart_s_cq()})
  
  # country wise output card 9
  output$d_hc_hrqol = renderHighchart({highchart_d_hrqol()})
  output$f_hc_hrqol = renderHighchart({highchart_f_hrqol()})
  output$n_hc_hrqol = renderHighchart({highchart_n_hrqol()})
  output$s_hc_hrqol = renderHighchart({highchart_s_hrqol()})
  
  # country wise output card 10
  output$d_hc_cs = renderHighchart({highchart_d_cs()})
  output$f_hc_cs = renderHighchart({highchart_f_cs()})
  output$n_hc_cs = renderHighchart({highchart_n_cs()})
  output$s_hc_cs = renderHighchart({highchart_s_cs()})
  
  
}

# RUN APP ----
shinyApp(ui, server)
