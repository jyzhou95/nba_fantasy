library(rvest)
library(data.table)
library(glue)
library(lubridate)
library(RCurl)
library(XML)
library(ballr)
library(plotly)
library(ggplot2)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
# https://github.com/abresler/requestsR
# https://stmorse.github.io/journal/espn-fantasy-python.html
# https://dusty-turner.netlify.com/post/mathlete-fantasy-football-analysis/
library(jsonlite)
library(httr)
library(nbastatR)

###################################################################################################
###################################################################################################
dt.nba_players <- data.table(nba_players())[isActive == T]
dt.nba_teams <- data.table(nba_teams())[,list(idTeam, slugTeam, teamNameFull)]
dt.nba_merged <- merge(dt.nba_players, dt.nba_teams, by = c("idTeam"))
dt.nba_merged[,player := tolower(namePlayer)]

dt.historical_performance <- fread("D:/git_repos/nba_fantasy/shiny/fantasy_league_performance.csv")
dt.historical_performance$player <- tolower(dt.historical_performance$player)
dt.nba_period <- fread("D:/git_repos/nba_fantasy/shiny/period_dates.csv")
dt.actual_performance <- merge(dt.historical_performance[,list(owner, period, player, 
                                                               actual_fantasy_points, 
                                                               predicted_fantasy_points, lineup)],
                               dt.nba_period[,list(period, dt)], by = c("period"))

int.latest_game <- max(dt.actual_performance$period)

# Fix people's names
dt.actual_performance[grepl("Jr\\.", player)]$player <- substr(dt.actual_performance[grepl("Jr\\.", player)]$player, 
                                                               start = 0, 
                                                               stop = nchar(dt.actual_performance[grepl("Jr\\.", player)]$player) - 4)
dt.actual_performance[,dt := as.Date(dt, "%m/%d/%Y")]
# Remove periods from people's names
# dt.actual_performance[grepl("\\.", player)]$player <- gsub("\\.", "", dt.actual_performance[grepl("\\.", player)]$player)

dt.curr_players <- dt.actual_performance[period == int.latest_game,list(owner, player)]

###################################################################################################
###################################################################################################


# Map team name to team code
dt.team_code <- data.table(team = c('Atlanta Hawks','Boston Celtics','Brooklyn Nets','Charlotte Hornets','Chicago Bulls',
                                    'Cleveland Cavaliers','Dallas Mavericks','Denver Nuggets','Detroit Pistons','Golden State Warriors',
                                    'Houston Rockets','Indiana Pacers','Los Angeles Clippers','Los Angeles Lakers','Memphis Grizzlies',
                                    'Miami Heat','Milwaukee Bucks','Minnesota Timberwolves','New Orleans Pelicans','New York Knicks',
                                    'Oklahoma City Thunder','Orlando Magic','Philadelphia 76ers','Phoenix Suns','Portland Trail Blazers',
                                    'Sacramento Kings','San Antonio Spurs','Toronto Raptors','Utah Jazz','Washington Wizards'),
                           team_code = c("ATL", "BRK", "BOS", "CHO", "CHI",
                                         "CLE", "DAL", "DEN", "DET", "GSW",
                                         "HOU", "IND", "LAC", "LAL", "MEM",
                                         "MIA", "MIL", "MIN", "NOP", "NYK",
                                         "OKC", "ORL", "PHI", "PHO", "POR",
                                         "SAC", "SAS", "TOR", "UTA", "WAS"))


lastmon <- function(x) 7 * floor(as.numeric(x-1+4)/7) + as.Date(1-4, origin="1970-01-01")

start_date <- lastmon(Sys.Date())


# Get current standings
url <- "https://www.basketball-reference.com/leagues/NBA_2021.html"
eastern_conference_list <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="confs_standings_E"]') %>%
  html_table()
eastern_conference_list <- eastern_conference_list[[1]]
eastern_conference_list <- eastern_conference_list[,c(1:3)]
colnames(eastern_conference_list) <- c("team", "wins", "losses")

western_conference_list <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="confs_standings_W"]') %>%
  html_table()
western_conference_list <- western_conference_list[[1]]
western_conference_list <- western_conference_list[,c(1:3)]
colnames(western_conference_list) <- c("team", "wins", "losses")


dt.all_standings <- rbind(data.table(eastern_conference_list), 
                          data.table(western_conference_list))
dt.all_standings[,total_games := wins+losses]
dt.all_standings[,wins := NULL]
dt.all_standings[,losses := NULL]
dt.all_standings[,team := unlist(strsplit(team, "\\("))[1], by = 1:nrow(dt.all_standings)]
dt.all_standings[,team := substr(team, 1, nchar(team) - 1)]

dt.player_links <- data.table(NBAPerGameAdvStatistics(season = 2020))
dt.player_links <- unique(dt.player_links[,list(player, link)])

players <- NBAPerGameStatistics(season = 2020)
dt.players <- data.table(players)
dt.players <- dt.players[,list(player = tolower(iconv(player, from = 'UTF-8', to = 'ASCII//TRANSLIT')), 
                               position = pos,
                               game = g,
                               minutes_played = mp,
                               two_point = x2p, 
                               two_point_attempt = x2pa, 
                               three_point = x3p, 
                               three_point_attempt = x3pa, 
                               free_throw = ft, 
                               free_throw_attempt = fta, 
                               rebounds = trb, 
                               assists = ast, 
                               steals = stl, 
                               blocks = blk, 
                               turnovers = tov)]
dt.players <- merge(dt.players, dt.nba_merged[,list(player, team_code = slugTeam)],by = c("player"), all = TRUE)

dt.players[,avg_fantasy_points := two_point * 3 - two_point_attempt + 
             three_point * 4 - three_point_attempt + 
             free_throw * 2 - free_throw_attempt +
             rebounds + assists * 2 + blocks * 3 + steals * 3 - turnovers * 2]

dt.final_players <- dt.players[,list(player, position, minutes_played, game, team_code, avg_fantasy_points)]
# Keep most recent team for player that have been traded
dt.final_players[,id := 1:nrow(dt.final_players)]
dt.final_players <- dt.final_players[dt.final_players[, .I[id == max(id)], by=player]$V1]
dt.final_players[,id := NULL]

dt.final_players <- merge(dt.final_players, dt.actual_performance[,list(player, predicted_fantasy_points)], by = c("player"), all.x = TRUE)
dt.final_players[!is.na(predicted_fantasy_points), avg_fantasy_points := predicted_fantasy_points]

funcGetInjuries <- function(){
  link <- "https://www.basketball-reference.com/friv/injuries.fcgi"
  injury_list <- link %>%
    read_html() %>%
    html_table()
  dt.return.this <- data.table(injury_list[[1]])
  dt.return.this <- dt.return.this[,list(player = Player,
                                         injured = TRUE)]
  return (dt.return.this)
}

dt.injuries <- funcGetInjuries()
dt.injuries[,player := tolower(player)]