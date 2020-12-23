# Create app
ui <- shinyUI(
  fluidPage(
    headerPanel("NBA Fantasy"),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Weekly projection",
                           
                           fluidRow(
                             column(3, dateInput("NBA_Start_Date", "NBA Start Date", start_date)),
                             column(3, selectInput("player_position", "Player Position", c("ALL", "G", "F", "C"), "ALL")),                           
                             column(3, selectInput("status", "Availability: ", 
                                                   choices = c("all", "available", "unavailable"),
                                                   selected = "all")),
                             column(3, selectInput("owner", "Owner: ", c("all", "me", "alex", "kyle", 
                                                                         "edward", "liam", "daniel",
                                                                         "eugene", "greg", "stephen",
                                                                         "lee"),
                                                   selected = "all"))
                             ),
                           
                           # br(),
                           # 
                           # fluidRow(
                           #   column(3, materialSwitch(inputId = "historicalLookback", label = "Use past 10 days", status = "primary", right = FALSE))
                           # ),
                           
                           br(),
                           
                           fluidRow(
                             column(9, plotlyOutput("fantasyProjections", width = 1400, height = 700) %>% withSpinner(color="#0dc5c1"))
                           )
                  ),
                  
                  tabPanel("Trade Analyzer",
                           fluidRow(
                             fluidRow(
                               column(9, selectInput(inputId = "myPlayers", label = "My players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "me"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(9, selectInput(inputId = "alexPlayers", label = "Alex players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "alex"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(9, selectInput(inputId = "kylePlayers", label = "Kyle players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "kyle"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(9, selectInput(inputId = "eddiePlayers", label = "Eddie players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "eddie"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(9, selectInput(inputId = "danielPlayers", label = "Daniel players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "daniel"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             
                             fluidRow(
                               column(6, selectInput(inputId = "liamPlayers", label = "Liam players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "liam"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(6, selectInput(inputId = "eugenePlayers", label = "Eugene players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "eugene"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(6, selectInput(inputId = "stephenPlayers", label = "Stephen players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "stephen"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(6, selectInput(inputId = "leePlayers", label = "Lee players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "lee"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(6, selectInput(inputId = "gregPlayers", label = "Greg players: ",choices = unique(dt.final_players$player),
                                                     selected = dt.curr_players[owner == "greg"]$player, multiple = TRUE, width = 900))
                             ),
                             
                             fluidRow(
                               column(3, dataTableOutput("projectionTables"))
                             )
                             
                           )
                  ),
                  
                  tabPanel("Weekly Performance Tracker",
                           fluidRow(
                             column(3, materialSwitch(inputId = "playerPerformance", label = "Show player performance", status = "primary", right = FALSE))
                           ),
                           br(),
                           fluidRow(
                             column(9, plotlyOutput("weekly_performance", width = 1400, height = 1200) %>% withSpinner(color="#0dc5c1"))
                           )
                           ),
                  
                  tabPanel("Analyze Player",
                           fluidRow(
                             column(3, selectInput("player", "Enter Player: ", selected = "LeBron James", 
                                                   choices = unique(dt.final_players$player))),
                             column(3, selectInput("year", "Choose year: ", selected = 2019, choices = c(2017, 2018, 2019, 2020)))
                           ),
                           fluidRow(
                             column(9, plotlyOutput("timeSeries", width = 1400, height = 700) %>% withSpinner(color="#0dc5c1"))
                           )
                  ),
                  tabPanel("Research Player",
                           fluidRow(
                             column(9, dataTableOutput("playerBeta") %>% withSpinner(color="#0dc5c1"))
                           )
                  )
      )
    )
  )
)