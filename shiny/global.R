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
library(requestsR)
# https://github.com/abresler/requestsR
# https://stmorse.github.io/journal/espn-fantasy-python.html
library(requestsR)
library(reticulate)
library(jsonlite)
library(requestsR)

###################################################################################################
###################################################################################################


vec.my_players <- c("Chris Paul", "Devin Booker", "Kawhi Leonard", "LaMarcus Aldridge", "DeAndre Jordan",
                    "Thaddeus Young", "LeBron James", "Enes Kanter", "Steven Adams",
                    "Blake Griffin", "Serge Ibaka", "Josh Richardson", "Kent Bazemore")

vec.alex <- c("John Wall", "Bradley Beal", "Jayson Tatum", "Hassan Whiteside",
              "James Harden", "Russell Westbrook", "Clint Capela", "Nemanja Bjelica",
              "Deandre Ayton", "Donovan Mitchell", "Montrezl Harrell", "Nikola Mirotic", "Khris Middleton")

vec.kyle <- c("Nikola Jokic", "De'Aaron Fox", "Victor Oladipo", "Robert Covington", "John Collins",
              "Damian Lillard", "Domantas Sabonis", "Larry Nance", "Kevin Durant", "Rudy Gobert",
              "Eric Bledsoe", "Nikola Vucevic", "TJ Warren")

vec.edward <- c("Ricky Rubio", "CJ McCollum", "DeMar DeRozan", "Anthony Davis", "Karl-Anthony Towns",
                "Derrick Favors", "Aaron Gordon", "Kyle Lowry", "Myles Turner", "Willie Cauley-Stein",
                "Kemba Walker", "Al Horford", "Zach LaVine")

vec.daniel <- c("Kyrie Irving", "Luka Doncic", "Giannis Antetokounmpo", "Rudy Gay", "Jusuf Nurkic",
                "Ben Simmons", "Draymond Green", "Joel Embiid", "Jaren Jackson", "Danilo Gallinari",
                "JaVale McGee", "Julius Randle", "Pascal Siakam")

vec.liam <- c("Stephen Curry", "Kyle Kuzma", "Jimmy Butler", "Tobias Harris", "Marc Gasol",
              "Mike Conley", "Paul George", "Andre Drummond", "Klay Thompson", "Derrick Rose",
              "Jrue Holiday", "Jarrett Allen", "D'Angelo Russell")

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
url <- "https://www.basketball-reference.com/leagues/NBA_2019.html"
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

dt.player_links <- data.table(NBAPerGameAdvStatistics(season = 2019))
dt.player_links <- unique(dt.player_links[,list(player, link)])

players <- NBAPerGameStatistics(season = 2019)
dt.players <- data.table(players)
dt.players <- dt.players[,list(player, 
                               team_code = tm,
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
dt.players[,avg_fantasy_points := two_point * 3 - two_point_attempt + 
             three_point * 4 - three_point_attempt + 
             free_throw * 2 - free_throw_attempt +
             rebounds + assists + blocks * 3 + steals * 3 - turnovers]

dt.final_players <- dt.players[,list(player, position, minutes_played, game, team_code, avg_fantasy_points)]
# Keep most recent team for player that have been traded
dt.final_players[,id := 1:nrow(dt.final_players)]
dt.final_players <- dt.final_players[dt.final_players[, .I[id == max(id)], by=player]$V1]
dt.final_players[,id := NULL]

# funcGetInjuries <- function(vec_players){
#   dt.return.this <- rbindlist(lapply(vec_players, function(my_player){
#     print(my_player)
#     link <- paste0("https://www.basketball-reference.com",
#                    paste0(dt.player_links[player == my_player]$link))
#     
#     injury_list <- link %>%
#       read_html() %>%
#       html_nodes(xpath='//*[@id="injury"]') 
#     
#     
#     if (length(injury_list) > 0){
#       dt.return <- data.table(player = my_player,
#                               injured = TRUE)
#     } else{
#       
#       dt.return <- data.table(player = my_player,
#                               injured = FALSE)
#     }
#     return (dt.return)
#     
#   }))
#   
#   return(dt.return.this)
# }

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

# Grab player game data for the week
getPlayerPerformance <- function(vec_players){
  dt.return.this <- rbindlist(lapply(vec_players, function(my_player){
    print(my_player)
    link <- paste0("https://www.basketball-reference.com",
                   gsub(".html", glue("/gamelog/2019/"), paste0(dt.player_links[player == my_player]$link)))
    
    season_games_list <- link %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="pgl_basic"]') %>%
      html_table(fill = TRUE)
    season_games_list <- season_games_list[[1]]
    dt.season_games_list <- data.table(season_games_list)
    
    dt.season_games_list[,G := as.numeric(G)]
    dt.season_games_list <- dt.season_games_list[!is.na(G)]
    dt.season_games_list <- dt.season_games_list[,list(player = my_player,
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
    
  }))
  return (dt.return.this)
}

# dt.final_players[,owner := "none"]
# dt.final_players[,status := "available"]
# dt.final_players[player %in% vec.my_players]$owner <- "me"
# dt.final_players[player %in% vec.alex]$owner <- "alex"
# dt.final_players[player %in% vec.kyle]$owner <- "kyle"
# dt.final_players[player %in% vec.edward]$owner <- "edward"
# dt.final_players[player %in% vec.daniel]$owner <- "daniel"
# dt.final_players[player %in% vec.liam]$owner <- "liam"
# dt.final_players[owner != "none",status := "unavailable"]




