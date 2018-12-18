library(rvest)
library(data.table)
library(glue)
library(lubridate)
library(RCurl)
library(XML)
library(ballr)
library(plotly)
library(ggplot2)

###################################################################################################
###################################################################################################


vec.my_players <- c("Chris Paul", "Devin Booker", "Kawhi Leonard", "LaMarcus Aldridge", "DeAndre Jordan",
                    "Thaddeus Young", "LeBron James", "Enes Kanter", "Steven Adams",
                    "Blake Griffin", "Serge Ibaka", "Josh Richardson", "Kent Bazemore")

vec.alex <- c("John Wall", "Bradley Beal", "Jayson Tatum", "Hassan Whiteside",
              "James Harden", "Russell Westbrook", "Clint Capela", "Nemanja Bjelica",
              "Deandre Ayton", "Donovan Mitchell", "Montrezl Harrell")
# , "Nikola Mirotic", "Khris Middleton"
vec.kyle <- c("Nikola Jokic", "De'Aaron Fox", "Victor Oladipo", "Robert Covington", "John Collins",
              "Damian Lillard", "Domantas Sabonis", "Larry Nance", "Kevin Durant", "Rudy Gobert",
              "Eric Bledsoe", "Nikola Vucevic", "Jeremy Lamb")

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

start_date <- floor_date(Sys.Date())

# Get Sunday of this week
sunday <- ceiling_date(start_date, "week")


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

url <- "https://www.basketball-reference.com/leagues/NBA_2019_games-december.html"
games_list <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="schedule"]') %>%
  html_table()

games_list <- games_list[[1]]
games_list <- data.table(games_list)

colnames(games_list) <- c("dt", "time", "visitor", "visitor_pts", "home", "home_pts", "box_score", "blank", "attendance", "notes")
dt.games_list <- games_list[,list(dt, visitor, home)]

# Format date
dt.games_list <- rbindlist(lapply(1:nrow(dt.games_list), function(x){
  dt.temp <- dt.games_list[x]
  dt.temp$dt <- paste(unlist(strsplit(dt.temp$dt, ", "))[2:3], collapse = " ")
  dt.temp$dt <- as.Date(dt.temp$dt, format = "%b %d %Y")
  return(dt.temp)
}))

dt.games_list <- dt.games_list[dt >= start_date & dt <= sunday]
dt.games_list_flat <- rbind(dt.games_list[,list(dt,
                                                team = visitor)],
                            dt.games_list[,list(dt,
                                                team = home)])
dt.games_list_count <- dt.games_list_flat[,.N, by = list(team)]
dt.games_list_count <- dt.games_list_count[order(N, decreasing = TRUE)]
colnames(dt.games_list_count) <- c("team", "game_count")

dt.final_team <- merge(dt.games_list_count, dt.team_code, by = c("team"))
dt.final_team <- dt.final_team[order(game_count, decreasing = TRUE)]

player_links <- NBAPerGameAdvStatistics(season = 2019)
dt.player_links <- data.table(player_links)[,list(player, link)]

players <- NBAPerGameStatistics(season = 2019)
dt.players <- data.table(players)
dt.players <- dt.players[,list(player, 
                               team_code = tm,
                               position = pos,
                               game = g,
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

dt.final_players <- dt.players[,list(player, position, game, team_code, avg_fantasy_points)]

dt.final_data_set <- merge(dt.final_team, dt.final_players, by = c("team_code"))
dt.final_data_set[,projected_fantasy_points := avg_fantasy_points * game_count]
dt.final_data_set_with_games <- merge(dt.final_data_set, dt.all_standings, by = c("team"))

dt.final_data_set_with_games[,owned := FALSE]
dt.final_data_set_with_games[player %in% vec.my_players]$owned <- TRUE

# merge on links
dt.final_data_set_with_games <- merge(dt.final_data_set_with_games, dt.player_links, by = c("player"))

my_plt <- ggplot(dt.final_data_set_with_games, aes(x = game/total_games, 
                                                   y = projected_fantasy_points,
                                                   color = paste0(game_count, owned),
                                                   shape = position,
                                                   text = paste0("Projected fantasy points: ", projected_fantasy_points,
                                                                 "\nPlayer: ", player,
                                                                 "\nPosition: ", position))) + 
  geom_point(position = "jitter") + xlab("Percent of game started") + ylab("Projected fantasy points") + 
  theme_bw(base_size = 15) + scale_color_brewer(palette = "Set1")

ggplotly(my_plt, tooltip = c("text"))


# Calculate expected fantasy points
paste0("My expected score: ", sum(dt.final_data_set_with_games[owned == TRUE]$projected_fantasy_points))
paste0("Alex expected score: ", sum(dt.final_data_set_with_games[player %in% vec.alex]$projected_fantasy_points))
paste0("Kyle expected score: ", sum(dt.final_data_set_with_games[player %in% vec.kyle]$projected_fantasy_points))

# Analyze player
my_player <- "Stephen Curry"
link <- paste0("https://www.basketball-reference.com",
               gsub(".html", "/gamelog/2018/", paste0(dt.final_data_set_with_games[player == my_player]$link)))

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


plt.time_series <- ggplot(dt.season_games_list, 
                          aes(x = game, y = fantasy_points,
                              text = paste0("vs ", opponent,
                                            "\nFantasy Points: ", fantasy_points,
                                            "\nPoints: ", points,
                                            "\nRebounds: ", rebounds,
                                            "\nAssists", assists,
                                            "\nSteals: ", steals,
                                            "\nBlocks: ", blocks,
                                            "\nTurnovers: ", turnovers),
                              group = 1)) + 
  geom_point(color = "red") + geom_line(color = "lightblue") + theme_bw(base_size = 15) +
  xlab("Game in season") + ylab("Fantasy Points") + geom_smooth(method = "lm")

ggplotly(plt.time_series, tooltip = c("text"))

