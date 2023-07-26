library(shiny)
library(shinydashboard)

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
  dashboardBody(tabItems(
    tabItem(
      tabName = "den",
      fluidRow(
        box(
          width = 2,
          sliderInput(
            "den_start_age",
            "Age of the patient population",
            min = 1,
            max = 99,
            value = 50
          )
        ),
        box(
          width = 2,
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
          selectInput(
            "den_dropdown",
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
        box(width = 6,
            "Remaining QALYs",
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
            div(style = "display: inline-block;", textOutput("den_mltplr_txt"))),
        box(width = 6,
            "Absolute shortfall",
            plotOutput("den_bar"))
      ),
      fluidRow(
        box(width = 6,
            "Proportional shortfall",
            plotOutput("den_pie")),
        box(
          width = 6,
          "Cumulative QALYs",
          plotOutput("den_cumulative_qalys")
        )
      ),
      fluidRow(
        box(width = 6,
            "HRQoL by year",
            plotOutput("den_hrqol")),
        box(width = 6,
            "Cumulative survival",
            plotOutput("den_S_cumulativ"))
      )
    ),
    tabItem(
      tabName = "fin",
      fluidRow(
        box(
          width = 2,
          sliderInput(
            "fin_start_age",
            "Age of the patient population",
            min = 1,
            max = 99,
            value = 50
          )
        ),
        box(
          width = 2,
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
        box(width = 6,
            "Remaining QALYs",
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
            div(style = "display: inline-block;", textOutput("fin_mltplr_txt"))),
        box(width = 6,
            "Absolute shortfall",
            plotOutput("fin_bar"))
      ),
      fluidRow(
        box(width = 6,
            "Proportional shortfall",
            plotOutput("fin_pie")),
        box(
          width = 6,
          "Cumulative QALYs",
          plotOutput("fin_cumulative_qalys")
        )
      ),
      fluidRow(
        box(width = 6,
            "HRQoL by year",
            plotOutput("fin_hrqol")),
        box(width = 6,
            "Cumulative survival",
            plotOutput("fin_S_cumulativ"))
      )
    ),
    tabItem(
      tabName = "nor",
      fluidRow(
        box(
          width = 2,
          sliderInput(
            "nor_start_age",
            "Age of the patient population",
            min = 1,
            max = 99,
            value = 50
          )
        ),
        box(
          width = 2,
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
        box(width = 6,
            "Remaining QALYs",
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
            div(style = "display: inline-block;", textOutput("nor_mltplr_txt"))),
        box(width = 6,
            "Absolute shortfall",
            plotOutput("nor_bar"))
      ),
      fluidRow(
        box(width = 6,
            "Proportional shortfall",
            plotOutput("nor_pie")),
        box(
          width = 6,
          "Cumulative QALYs",
          plotOutput("nor_cumulative_qalys")
        )
      ),
      fluidRow(
        box(width = 6,
            "HRQoL by year",
            plotOutput("nor_hrqol")),
        box(width = 6,
            "Cumulative survival",
            plotOutput("nor_S_cumulativ"))
      )
    ),
    tabItem(
      tabName = "swe",
      fluidRow(
        box(
          width = 2,
          sliderInput(
            "swe_start_age",
            "Age of the patient population",
            min = 1,
            max = 99,
            value = 50
          )
        ),
        box(
          width = 2,
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
        box(width = 6,
            "Remaining QALYs",
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
            div(style = "display: inline-block;", textOutput("swe_mltplr_txt"))),
        box(width = 6,
            "Absolute shortfall",
            plotOutput("swe_bar"))
      ),
      fluidRow(
        box(width = 6,
            "Proportional shortfall",
            plotOutput("swe_pie")),
        box(
          width = 6,
          "Cumulative QALYs",
          plotOutput("swe_cumulative_qalys")
        )
      ),
      fluidRow(
        box(width = 6,
            "HRQoL by year",
            plotOutput("swe_hrqol")),
        box(width = 6,
            "Cumulative survival",
            plotOutput("swe_S_cumulativ"))
      )
    )
  ))
)

server = function(input, output, session) {
  output$text <- renderText("Hello")
}

shinyApp(ui, server)