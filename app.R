# libraries ----
library(shiny)
library(shinydashboard)

# input data ----
ref_df = read.csv("https://github.com/bitowaqr/shortfall/raw/main/app/data/ref_df_appended.csv")
mvh_df = read.csv("https://github.com/bitowaqr/shortfall/raw/main/app/data/mvh_df.csv")

# compute QALE ----
compQale = function(ons_df,
                    prop_female = 0.5,
                    start_age = 50,
                    disc_rate = 0.035,
                    utils = "cw") {
  compQaleInternal = function(ons_df,
                              prop_female = 0.5,
                              start_age = 50,
                              disc_rate = 0.035,
                              utils = "cw") {
    ons_df = ons_df[ons_df$age >= start_age, ]
    ons_df = ons_df[order(ons_df$age), ]
    df_female = ons_df[ons_df$sex == "female", c("age", utils, "lx", "dx", "mx", "ex")]
    df_male = ons_df[ons_df$sex == "male", c("age", utils, "lx", "dx", "mx", "ex")]
    
    df_comp = data.frame(
      age = df_female$age,
      utils = (1 - prop_female) * df_male[, utils]  + prop_female * df_female[, utils],
      lx = (1 - prop_female) * df_male$lx  + prop_female * df_female$lx,
      dx = (1 - prop_female) * df_male$dx  + prop_female * df_female$dx,
      mx = (1 - prop_female) * df_male$mx  + prop_female * df_female$mx,
      ex = (1 - prop_female) * df_male$ex  + prop_female * df_female$ex
    )
    
    # person years in year i
    df_comp$Lx = NA
    for (i in 2:nrow(df_comp)) {
      df_comp$Lx[i - 1] = df_comp$lx[i] + (0.5 * df_comp$dx[i - 1])
    }
    df_comp$Lx[nrow(df_comp)] = (df_comp$lx[nrow(df_comp)] - df_comp$dx[nrow(df_comp)]) + (0.5 * df_comp$dx[nrow(df_comp)])
    
    # person QALYs in year i
    df_comp$Yx = df_comp$utils * df_comp$Lx
    
    # apply discounting
    v_disc <- 1 / (1 + disc_rate) ^ (0:(length(df_comp$Yx) - 1))
    df_comp$Yx = df_comp$Yx * v_disc
    
    # remaining person QALYs?
    df_comp$Nx = NA
    df_comp$Nx[nrow(df_comp)] = df_comp$Yx[nrow(df_comp)]
    for (i in nrow(df_comp):2) {
      df_comp$Nx[i - 1] = df_comp$Yx[i - 1] + df_comp$Nx[i]
    }
    
    # Quality adjusted life expectancy
    df_comp$Qx = df_comp$Nx / df_comp$lx
    
    
    q_factor = sum(df_comp$Yx) / df_comp$Qx[1]
    
    df_comp$qalys_by_year = df_comp$Yx / q_factor
    df_comp$cumulative_qalys = cumsum(df_comp$qalys_by_year)
    
    # cumulative survival function
    df_comp$S = 1 - df_comp$mx
    df_comp$S_cumulativ =  cumprod(df_comp$S)
    df_comp$hrqol = df_comp$utils
    
    df_comp = df_comp[, c("age",
                          "hrqol",
                          "ex",
                          "Qx",
                          "S_cumulativ",
                          "cumulative_qalys")]
    
    return(df_comp)
    
  }
  
  
  qale_male = compQaleInternal(
    ons_df = ons_df,
    prop_female = 0,
    start_age = start_age,
    disc_rate = disc_rate,
    utils = utils
  )
  qale_female = compQaleInternal(
    ons_df = ons_df,
    prop_female = 1,
    start_age = start_age,
    disc_rate = disc_rate,
    utils = utils
  )
  qale_mix = qale_male * (1 - prop_female) + qale_female * prop_female
  
  return(qale_mix)
  
}

# ui ----
ui = dashboardPage(
  dashboardHeader(title = "Scandinavian Shortfall Calculator",
                  titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Denmark",
               tabName = "den"),
      menuItem("Finland",
               tabName = "fin"),
      menuItem("Norway",
               tabName = "nor"),
      menuItem("Sweden",
               tabName = "swe")
    )
  ),
  dashboardBody(width = 10,
                tabItems(
                  tabItem(
                    tabName = "den",
                    fluidRow(
                      box(
                        width = 3,
                        sliderInput(
                          "den_start_age",
                          "Age of the patient population",
                          min = 1,
                          max = 99,
                          value = 50
                        )
                      ),
                      box(
                        width = 3,
                        sliderInput(
                          "den_sex_mix",
                          "% female in the patient population",
                          min = 1,
                          max = 100,
                          value = 50
                        )
                      ),
                      box(
                        width = 2,
                        selectizeInput(
                          "den_utils",
                          "Select scenario",
                          selected = "dsu_2014",
                          choices = c(
                            "Reference case: MVH value set + HSE 2014 ALDVMM model (Hernandez Alava et al)" = "dsu_2014",
                            "Alternative A: 5L to 3L mapping (Hernandez Alava et al) + HSE 2017-2018" = "dsu",
                            "Alternative B: 5L to 3L mapping (van Hout et al) + HSE 2017-2018" = "vanHout",
                            "Alternative C: MVH value set + health state profiles" = "mvh",
                            "Alternative D: MVH value set + HSE 2012+14" = "tto"
                          )
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "den_remaining_qalys",
                          "Remaining QALYs of untreated (discounted)",
                          value = 10
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "den_disc_rate",
                          "Discount rate (%)",
                          value = 1.5,
                          min = 0,
                          max = 10
                        )
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Remaining QALYs", style = 'font-size:28px;color:black;'),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "without the disease: "),
                        div(style = "display: inline-block;", textOutput("den_qales_healthy_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "with the disease: "),
                        div(style = "display: inline-block;", textOutput("den_qales_ill_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "absolute shortfall: "),
                        div(style = "display: inline-block;", textOutput("den_abs_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "proportional shortfall: "),
                        div(style = "display: inline-block;", textOutput("den_prop_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "QALY weight: "),
                        div(style = "display: inline-block;", textOutput("den_mltplr_txt"))
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Absolute shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("den_bar")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Proportional shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("den_pie")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative QALYs", style = 'font-size:28px;color:black;'),
                        plotOutput("den_cumulative_qalys")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("HRQoL by year", style = 'font-size:28px;color:black;'),
                        plotOutput("den_hrqol")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative survival", style = 'font-size:28px;color:black;'),
                        plotOutput("den_S_cumulativ")
                      )
                    )
                  ),
                  tabItem(
                    tabName = "fin",
                    fluidRow(
                      box(
                        width = 3,
                        sliderInput(
                          "fin_start_age",
                          "Age of the patient population",
                          min = 1,
                          max = 99,
                          value = 50
                        )
                      ),
                      box(
                        width = 3,
                        sliderInput(
                          "fin_sex_mix",
                          "% female in the patient population",
                          min = 1,
                          max = 100,
                          value = 50
                        )
                      ),
                      box(
                        width = 2,
                        selectInput(
                          "fin_dropdown",
                          "Select scenario",
                          selected = "ref",
                          choices = c(
                            "Reference Case" = "ref",
                            "Alternative A" = "a",
                            "Alternative B" = "b",
                            "Alternative C" = "c",
                            "Alternative D" = "d"
                          )
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "fin_remaining_qalys",
                          "Remaining QALYs of untreated (discounted)",
                          value = 10
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "fin_disc_rate",
                          "Discount rate (%)",
                          value = 1.5,
                          min = 0,
                          max = 10
                        )
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Remaining QALYs", style = 'font-size:28px;color:black;'),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "without the disease: "),
                        div(style = "display: inline-block;", textOutput("fin_qales_healthy_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "with the disease: "),
                        div(style = "display: inline-block;", textOutput("fin_qales_ill_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "absolute shortfall: "),
                        div(style = "display: inline-block;", textOutput("fin_abs_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "proportional shortfall: "),
                        div(style = "display: inline-block;", textOutput("fin_prop_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "QALY weight: "),
                        div(style = "display: inline-block;", textOutput("fin_mltplr_txt"))
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Absolute shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("fin_bar")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Proportional shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("fin_pie")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative QALYs", style = 'font-size:28px;color:black;'),
                        plotOutput("fin_cumulative_qalys")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("HRQoL by year", style = 'font-size:28px;color:black;'),
                        plotOutput("fin_hrqol")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative survival", style = 'font-size:28px;color:black;'),
                        plotOutput("fin_S_cumulativ")
                      )
                    )
                  ),
                  tabItem(
                    tabName = "nor",
                    fluidRow(
                      box(
                        width = 3,
                        sliderInput(
                          "nor_start_age",
                          "Age of the patient population",
                          min = 1,
                          max = 99,
                          value = 50
                        )
                      ),
                      box(
                        width = 3,
                        sliderInput(
                          "nor_sex_mix",
                          "% female in the patient population",
                          min = 1,
                          max = 100,
                          value = 50
                        )
                      ),
                      box(
                        width = 2,
                        selectInput(
                          "nor_dropdown",
                          "Select scenario",
                          selected = "ref",
                          choices = c(
                            "Reference Case" = "ref",
                            "Alternative A" = "a",
                            "Alternative B" = "b",
                            "Alternative C" = "c",
                            "Alternative D" = "d"
                          )
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "nor_remaining_qalys",
                          "Remaining QALYs of untreated (discounted)",
                          value = 10
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "nor_disc_rate",
                          "Discount rate (%)",
                          value = 1.5,
                          min = 0,
                          max = 10
                        )
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Remaining QALYs", style = 'font-size:28px;color:black;'),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "without the disease: "),
                        div(style = "display: inline-block;", textOutput("nor_qales_healthy_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "with the disease: "),
                        div(style = "display: inline-block;", textOutput("nor_qales_ill_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "absolute shortfall: "),
                        div(style = "display: inline-block;", textOutput("nor_abs_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "proportional shortfall: "),
                        div(style = "display: inline-block;", textOutput("nor_prop_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "QALY weight: "),
                        div(style = "display: inline-block;", textOutput("nor_mltplr_txt"))
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Absolute shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("nor_bar")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Proportional shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("nor_pie")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative QALYs", style = 'font-size:28px;color:black;'),
                        plotOutput("nor_cumulative_qalys")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("HRQoL by year", style = 'font-size:28px;color:black;'),
                        plotOutput("nor_hrqol")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative survival", style = 'font-size:28px;color:black;'),
                        plotOutput("nor_S_cumulativ")
                      )
                    )
                  ),
                  tabItem(
                    tabName = "swe",
                    fluidRow(
                      box(
                        width = 3,
                        sliderInput(
                          "swe_start_age",
                          "Age of the patient population",
                          min = 1,
                          max = 99,
                          value = 50
                        )
                      ),
                      box(
                        width = 3,
                        sliderInput(
                          "swe_sex_mix",
                          "% female in the patient population",
                          min = 1,
                          max = 100,
                          value = 50
                        )
                      ),
                      box(
                        width = 2,
                        selectInput(
                          "swe_dropdown",
                          "Select scenario",
                          selected = "ref",
                          choices = c(
                            "Reference Case" = "ref",
                            "Alternative A" = "a",
                            "Alternative B" = "b",
                            "Alternative C" = "c",
                            "Alternative D" = "d"
                          )
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "swe_remaining_qalys",
                          "Remaining QALYs of untreated (discounted)",
                          value = 10
                        )
                      ),
                      box(
                        width = 2,
                        numericInput(
                          "swe_disc_rate",
                          "Discount rate (%)",
                          value = 1.5,
                          min = 0,
                          max = 10
                        )
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Remaining QALYs", style = 'font-size:28px;color:black;'),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "without the disease: "),
                        div(style = "display: inline-block;", textOutput("swe_qales_healthy_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "with the disease: "),
                        div(style = "display: inline-block;", textOutput("swe_qales_ill_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "absolute shortfall: "),
                        div(style = "display: inline-block;", textOutput("swe_abs_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "proportional shortfall: "),
                        div(style = "display: inline-block;", textOutput("swe_prop_short_txt")),
                        br(),
                        br(),
                        div(style = "display: inline-block;", "QALY weight: "),
                        div(style = "display: inline-block;", textOutput("swe_mltplr_txt"))
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Absolute shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("swe_bar")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("Proportional shortfall", style = 'font-size:28px;color:black;'),
                        plotOutput("swe_pie")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative QALYs", style = 'font-size:28px;color:black;'),
                        plotOutput("swe_cumulative_qalys")
                      )
                    ),
                    fluidRow(
                      box(
                        width = 6,
                        height = 450,
                        h3("HRQoL by year", style = 'font-size:28px;color:black;'),
                        plotOutput("swe_hrqol")
                      ),
                      box(
                        width = 6,
                        height = 450,
                        h3("Cumulative survival", style = 'font-size:28px;color:black;'),
                        plotOutput("swe_S_cumulativ")
                      )
                    )
                  )
                ))
)

# server ----
server = function(input, output, session) {
  
}

# app compiler ----
shinyApp(ui, server)