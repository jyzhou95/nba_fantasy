ui <- shinyUI(
  fluidPage(
    headerPanel("Breach Data Analysis"),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Fantasy Points Projection",
                           fluidRow(
                             column(3, textInput("playerName", "Player", value = ""))
                           ),
                           fluidRow(
                             column(9, plotlyOutput("fantasyPoints", width = 900, height = 600))
                             ),
                           br(),
                           fluidRow(
                             column(3, tableOutput("playerStatistics"))
                             )
                           )
      )
    )
  )
)