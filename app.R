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

# load the other functions
source("./scr/loadFnc.R")

# UI ----
ui <- page_navbar(
  theme = bs_theme(preset = "shiny", "primary" = "#0675DD"),
  lang = "en",
  title = "Nordic Shortfall Calculator",
  sidebar = sidebar(HTML('<p style="font-weight:bold;">Input for</p>'), width = 300, sidebar_acc),
  nav_spacer(),
  
  # Generate nav_panels for each country
  generate_nav_panel("Denmark", "d"),
  generate_nav_panel("Finland", "f"),
  generate_nav_panel("Norway", "n"),
  generate_nav_panel("Sweden", "s")
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
    
    util_df = switch(input$d_utils,
                     "d_mvh" = d_mvh_df,
                     "d_vanHout" = d_ref_df,
                     "d_tto" = d_ref_df,
                     "d_dsu" = d_ref_df,
                     "d_dsu_2014" = d_ref_df,
                     d_ref_df
    )
    
    utils = switch(input$d_utils,
                   "d_mvh" = "tto",
                   "d_vanHout" = "cw",
                   "d_tto" = "tto",
                   "d_dsu" = "co",
                   "d_dsu_2014" = "dsu_2014",
                   "cw"
    )
    
    d_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$d_sex_mix/100,
      start_age = input$d_start_age,
      disc_rate = input$d_disc_rate/100,
      utils = utils
    )
    
    d_dat$shortfall_abs = d_dat$res$Qx[1] - input$d_remaining_qalys
    d_dat$shortfall_prop = d_dat$shortfall_abs / d_dat$res$Qx[1]
    d_dat$q_weight = ifelse(d_dat$shortfall_prop >= 0.95 | d_dat$shortfall_abs >= 18, 1.7,
                      ifelse(d_dat$shortfall_prop >= 0.85 | d_dat$shortfall_abs >= 12, 1.2, 1)
    )
  })
  
  # absolute shortfall highchart
  highchart_d_as <- reactive({
    create_highchart_as(d_dat, input$d_remaining_qalys, "#7cb5ec")
  })
  
  # proportional shortfall highchart ----
  highchart_d_ps = reactive({
    short_fall = data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - d_dat$shortfall_prop*100, d_dat$shortfall_prop*100),
      col = c("green","gray")
    )
    
    shortfall_str = paste0(round(d_dat$shortfall_prop*100,1))
    shortfall_str = paste0("Proportional<br>QALY<br>shortfall:<br><b>",shortfall_str,"%</b>")
    
    p1 = highchart() %>%
      hc_add_series(short_fall, "pie", hcaes(name = type, y = percent), name = "QALE", innerSize="70%") %>%
      hc_title(text = shortfall_str, align = "center",x=0, verticalAlign = 'middle', floating = "true", style = list(fontSize = "16px")) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 1,
        valueSuffix = '%'
      ) %>%
      hc_colors(c("#7cb5ec","gray"))
    
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
    
    util_df = switch(input$f_utils,
                     "d_mvh" = f_mvh_df,
                     "d_vanHout" = f_ref_df,
                     "d_tto" = f_ref_df,
                     "d_dsu" = f_ref_df,
                     "d_dsu_2014" = f_ref_df,
                     f_ref_df
    )
    
    utils = switch(input$f_utils,
                   "d_mvh" = "tto",
                   "d_vanHout" = "cw",
                   "d_tto" = "tto",
                   "d_dsu" = "co",
                   "d_dsu_2014" = "dsu_2014",
                   "cw"
    )
    
    f_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$f_sex_mix/100,
      start_age = input$f_start_age,
      disc_rate = input$f_disc_rate/100,
      utils = utils
    )
    
    f_dat$shortfall_abs = f_dat$res$Qx[1] - input$f_remaining_qalys
    f_dat$shortfall_prop = f_dat$shortfall_abs / f_dat$res$Qx[1]
    f_dat$q_weight = ifelse(f_dat$shortfall_prop >= 0.95 | f_dat$shortfall_abs >= 18, 1.7,
                      ifelse(f_dat$shortfall_prop >= 0.85 | f_dat$shortfall_abs >= 12, 1.2, 1)
    )
  })
  
  # absolute shortfall highchart
  highchart_f_as <- reactive({
    create_highchart_as(f_dat, input$f_remaining_qalys, "#7cb5ec")
  })
  
  # proportional shortfall highchart ----
  highchart_f_ps = reactive({
    short_fall = data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - f_dat$shortfall_prop*100, f_dat$shortfall_prop*100),
      col = c("green","gray")
    )
    
    shortfall_str = paste0(round(f_dat$shortfall_prop*100,1))
    shortfall_str = paste0("Proportional<br>QALY<br>shortfall:<br><b>",shortfall_str,"%</b>")
    
    p1 = highchart() %>%
      hc_add_series(short_fall, "pie", hcaes(name = type, y = percent), name = "QALE", innerSize="70%") %>%
      hc_title(text = shortfall_str, align = "center",x=0, verticalAlign = 'middle', floating = "true", style = list(fontSize = "16px")) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 1,
        valueSuffix = '%'
      ) %>%
      hc_colors(c("#7cb5ec","gray"))
    
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
    
    util_df = switch(input$n_utils,
                     "d_mvh" = n_mvh_df,
                     "d_vanHout" = n_ref_df,
                     "d_tto" = n_ref_df,
                     "d_dsu" = n_ref_df,
                     "d_dsu_2014" = n_ref_df,
                     n_ref_df
    )
    
    utils = switch(input$n_utils,
                   "d_mvh" = "tto",
                   "d_vanHout" = "cw",
                   "d_tto" = "tto",
                   "d_dsu" = "co",
                   "d_dsu_2014" = "dsu_2014",
                   "cw"
    )
    
    n_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$n_sex_mix/100,
      start_age = input$n_start_age,
      disc_rate = input$n_disc_rate/100,
      utils = utils
    )
    
    n_dat$shortfall_abs = n_dat$res$Qx[1] - input$n_remaining_qalys
    n_dat$shortfall_prop = n_dat$shortfall_abs / n_dat$res$Qx[1]
    n_dat$q_weight = ifelse(n_dat$shortfall_prop >= 0.95 | n_dat$shortfall_abs >= 18, 1.7,
                            ifelse(n_dat$shortfall_prop >= 0.85 | n_dat$shortfall_abs >= 12, 1.2, 1)
    )
  })
  
  # absolute shortfall highchart
  highchart_n_as <- reactive({
    create_highchart_as(n_dat, input$n_remaining_qalys, "#7cb5ec")
  })
  
  # proportional shortfall highchart ----
  highchart_n_ps = reactive({
    short_fall = data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - n_dat$shortfall_prop*100, n_dat$shortfall_prop*100),
      col = c("green","gray")
    )
    
    shortfall_str = paste0(round(n_dat$shortfall_prop*100,1))
    shortfall_str = paste0("Proportional<br>QALY<br>shortfall:<br><b>",shortfall_str,"%</b>")
    
    p1 = highchart() %>%
      hc_add_series(short_fall, "pie", hcaes(name = type, y = percent), name = "QALE", innerSize="70%") %>%
      hc_title(text = shortfall_str, align = "center",x=0, verticalAlign = 'middle', floating = "true", style = list(fontSize = "16px")) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 1,
        valueSuffix = '%'
      ) %>%
      hc_colors(c("#7cb5ec","gray"))
    
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
    
    util_df = switch(input$s_utils,
                     "d_mvh" = s_mvh_df,
                     "d_vanHout" = s_ref_df,
                     "d_tto" = s_ref_df,
                     "d_dsu" = s_ref_df,
                     "d_dsu_2014" = s_ref_df,
                     s_ref_df
    )
    
    utils = switch(input$s_utils,
                   "d_mvh" = "tto",
                   "d_vanHout" = "cw",
                   "d_tto" = "tto",
                   "d_dsu" = "co",
                   "d_dsu_2014" = "dsu_2014",
                   "cw"
    )
    
    s_dat$res = compQale(
      ons_df = util_df,
      prop_female = input$s_sex_mix/100,
      start_age = input$s_start_age,
      disc_rate = input$s_disc_rate/100,
      utils = utils
    )
    
    s_dat$shortfall_abs = s_dat$res$Qx[1] - input$s_remaining_qalys
    s_dat$shortfall_prop = s_dat$shortfall_abs / s_dat$res$Qx[1]
    s_dat$q_weight = ifelse(s_dat$shortfall_prop >= 0.95 | s_dat$shortfall_abs >= 18, 1.7,
                            ifelse(s_dat$shortfall_prop >= 0.85 | s_dat$shortfall_abs >= 12, 1.2, 1)
    )
  })
  
  # absolute shortfall highchart
  highchart_s_as <- reactive({
    create_highchart_as(s_dat, input$s_remaining_qalys, "#7cb5ec")
  })
  
  # proportional shortfall highchart ----
  highchart_s_ps = reactive({
    short_fall = data.frame(
      type = c("With disease", "% Shortfall"),
      percent = c(100 - s_dat$shortfall_prop*100, s_dat$shortfall_prop*100),
      col = c("green","gray")
    )
    
    shortfall_str = paste0(round(s_dat$shortfall_prop*100,1))
    shortfall_str = paste0("Proportional<br>QALY<br>shortfall:<br><b>",shortfall_str,"%</b>")
    
    p1 = highchart() %>%
      hc_add_series(short_fall, "pie", hcaes(name = type, y = percent), name = "QALE", innerSize="70%") %>%
      hc_title(text = shortfall_str, align = "center",x=0, verticalAlign = 'middle', floating = "true", style = list(fontSize = "16px")) %>%
      hc_chart(
        style = list(
          fontFamily = "Inter"
        )
      ) %>%
      hc_tooltip(
        valueDecimals = 1,
        valueSuffix = '%'
      ) %>%
      hc_colors(c("#7cb5ec","gray"))
    
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
