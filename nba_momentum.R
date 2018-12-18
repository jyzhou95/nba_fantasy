library(rvest)
library(data.table)
library(glue)
library(lubridate)
library(RCurl)
library(XML)
library(ballr)
library(plotly)
library(ggplot2)

my_year <- 2019

dt.player_links <- data.table(NBAPerGameAdvStatistics(season = my_year))
dt.player_links <- unique(dt.player_links[,list(player, link)])

dt.all_player_data <- rbindlist({
  lapply(dt.player_links$player, function(x){
    print(x)
    link <- paste0("https://www.basketball-reference.com",
                   gsub(".html", glue("/gamelog/{my_year}/"), paste0(dt.player_links[player == x]$link)))
    
    season_games_list <- link %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="pgl_basic"]') %>%
      html_table(fill = TRUE)
    season_games_list <- season_games_list[[1]]
    dt.season_games_list <- data.table(season_games_list)
    
    dt.season_games_list[,G := as.numeric(G)]
    dt.season_games_list <- dt.season_games_list[!is.na(G)]
    dt.season_games_list <- dt.season_games_list[,list(player = x,
                                                       game = G,
                                                       dt = as.Date(Date),
                                                       team = Tm,
                                                       opponent = Opp,
                                                       two_point = as.numeric(FG) - as.numeric(`3P`),
                                                       two_point_attempt = as.numeric(FGA) - as.numeric(`3PA`),
                                                       three_point = as.numeric(`3P`),
                                                       three_point_attempt = as.numeric(`3PA`),
                                                       free_throw = as.numeric(FT),
                                                       free_throw_attempt = as.numeric(FTA),
                                                       rebounds = as.numeric(TRB),
                                                       assists = as.numeric(AST),
                                                       steals = as.numeric(STL),
                                                       blocks = as.numeric(BLK),
                                                       turnovers = as.numeric(TOV),
                                                       points = as.numeric(PTS)
    )]
    
    
    dt.season_games_list[,fantasy_points := two_point * 3 - two_point_attempt + 
                           three_point * 4 - three_point_attempt + 
                           free_throw * 2 - free_throw_attempt +
                           rebounds + assists + blocks * 3 + steals * 3 - turnovers]
    return (dt.season_games_list)
  })
  
})

# Run linear regression for each player to find uptrend
dt.regressions <- rbindlist(lapply(dt.player_links$player, function(x){
  dt.temp <- dt.all_player_data[player == x]
  if (nrow(dt.temp) > 10){
    lin_model <- lm(formula = dt.temp$fantasy_points ~ dt.temp$g)
    dt.return.this <- data.table(player = x,
                                 slope = as.numeric(lin_model$coefficients[2]))
    return (dt.return.this)
  }
}))

dt.regressions[!is.na(slope)][order(slope)]






