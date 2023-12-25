# Load the required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(waiter)

# Load nordic life tables
# Denmark
d_ref_df = read.csv("C:/Github/BMS/data/d_ref_df_appended.csv") # Load Denmark reference data
d_mvh_df = read.csv("C:/Github/BMS/data/d_mvh_df.csv") # Load Denmark MVH data

# Finland
f_ref_df = read.csv("C:/Github/BMS/data/f_ref_df_appended.csv") # Load Finland reference data
f_mvh_df = read.csv("C:/Github/BMS/data/f_mvh_df.csv") # Load Finland MVH data

# Denmark
n_ref_df = read.csv("C:/Github/BMS/data/n_ref_df_appended.csv") # Load Norway reference data
n_mvh_df = read.csv("C:/Github/BMS/data/n_mvh_df.csv") # Load Norway MVH data

# Finland
s_ref_df = read.csv("C:/Github/BMS/data/s_ref_df_appended.csv") # Load Sweden reference data
s_mvh_df = read.csv("C:/Github/BMS/data/s_mvh_df.csv") # Load Sweden MVH data

# Define UI for application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Nordic Shortfall Calculator"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Norway", tabName = "norway"),
      menuItem("Sweden", tabName = "sweden"),
      menuItem("Denmark", tabName = "denmark"),
      menuItem("Finland", tabName = "finland")
    )
  ),
  dashboardBody(
    tabItem(tabName = "norway",
            fluidRow(
              column(width = 6,
                     sliderInput("n_start_age", "Age of the patient population", min = 0, max = 100, value = 0, step = 1),
                     sliderInput("n_sex_mix", "% female in the patient population", min = 0, max = 100, value = 50, step = 1),
                     selectInput("n_utils", "Select scenario", 
                                 choices = list("Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = "n_dsu_2014",
                                                "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = "n_dsu",
                                                "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = "n_vanHout",
                                                "Alternative C: MVH value set + health state profiles" = "n_mvh",
                                                "Alternative D: MVH value set + HSE 2012+14" = "n_tto"), 
                                 selected = "n_dsu_2014"),
                     sliderInput("n_remaining_qalys", "Remaining QALYS", min = 0, max = 49, value = 10, step = 1),
                     sliderInput("n_disc_rate", "Discount rate", min = 0, max = 10, value = 1.5, step = 0.5)
              ),
              column(width = 6,
                     plotOutput("n_plot1Norway"), 
                     plotOutput("n_plot2Norway") 
              )
            )
    ),
    tabItem(tabName = "sweden",
            fluidRow(
              column(width = 6,
                     sliderInput("s_start_age", "Age of the patient population", min = 0, max = 100, value = 0, step = 1),
                     sliderInput("s_sex_mix", "% female in the patient population", min = 0, max = 100, value = 50, step = 1),
                     selectInput("s_utils", "Select scenario", 
                                 choices = list("Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = "s_dsu_2014",
                                                "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = "s_dsu",
                                                "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = "s_vanHout",
                                                "Alternative C: MVH value set + health state profiles" = "s_mvh",
                                                "Alternative D: MVH value set + HSE 2012+14" = "s_tto"), 
                                 selected = "s_dsu_2014"),
                     sliderInput("s_remaining_qalys", "Remaining QALYS", min = 0, max = 49, value = 10, step = 1),
                     sliderInput("s_disc_rate", "Discount rate", min = 0, max = 10, value = 1.5, step = 0.5)
              ),
              column(width = 6,
                     plotOutput("s_plot1Sweden"), 
                     plotOutput("s_plot2Sweden") 
              )
            )
    ),
    tabItem(tabName = "denmark",
            fluidRow(
              column(width = 6,
                     sliderInput("d_start_age", "Age of the patient population", min = 0, max = 100, value = 0, step = 1),
                     sliderInput("d_sex_mix", "% female in the patient population", min = 0, max = 100, value = 50, step = 1),
                     selectInput("d_utils", "Select scenario", 
                                 choices = list("Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = "d_dsu_2014",
                                                "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = "d_dsu",
                                                "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = "d_vanHout",
                                                "Alternative C: MVH value set + health state profiles" = "d_mvh",
                                                "Alternative D: MVH value set + HSE 2012+14" = "d_tto"), 
                                 selected = "d_dsu_2014"),
                     sliderInput("d_remaining_qalys", "Remaining QALYS", min = 0, max = 49, value = 10, step = 1),
                     sliderInput("d_disc_rate", "Discount rate", min = 0, max = 10, value = 1.5, step = 0.5)
              ),
              column(width = 6,
                     plotOutput("d_plot1Denmark"), 
                     plotOutput("d_plot2Denmark") 
              )
            )
    ),
    tabItem(tabName = "finland",
            fluidRow(
              column(width = 6,
                     sliderInput("f_start_age", "Age of the patient population", min = 0, max = 100, value = 0, step = 1),
                     sliderInput("f_sex_mix", "% female in the patient population", min = 0, max = 100, value = 50, step = 1),
                     selectInput("f_utils", "Select scenario", 
                                 choices = list("Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = "f_dsu_2014",
                                                "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = "f_dsu",
                                                "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = "f_vanHout",
                                                "Alternative C: MVH value set + health state profiles" = "f_mvh",
                                                "Alternative D: MVH value set + HSE 2012+14" = "f_tto"), 
                                 selected = "f_dsu_2014"),
                     sliderInput("f_remaining_qalys", "Remaining QALYS", min = 0, max = 49, value = 10, step = 1),
                     sliderInput("f_disc_rate", "Discount rate", min = 0, max = 10, value = 1.5, step = 0.5)
              ),
              column(width = 6,
                     plotOutput("f_plot1Finland"), 
                     plotOutput("f_plot2Finland") 
              )
            )
    ),
    tags$footer(style="position:fixed;bottom:0;width:100%;",
                tags$p("Developed for BMS, Nordics",
                       style="text-align:right;padding-right:10px;"),
                tags$p("Developed by Mohammad Sayeef Alam",
                       style="text-align:right;padding-right:10px;")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Add your server-side reactive functions here
}

# Run the application 
shinyApp(ui = ui, server = server)
