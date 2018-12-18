library(rvest)
library(data.table)
library(glue)
library(lubridate)
library(RCurl)
library(XML)
library(ballr)

funcPrivateGetAllStandings <- function(){
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
  return (dt.all_standings)
}

funcPrivateGetMonthlyGames <- function(year = 2019, 
                                       month = tolower(month.name[month(Sys.Date())]),
                                       start_date = Sys.Date(), 
                                       end_date = ceiling_date(start_date, "week")){
  # Get team code
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  parent_dir <- getwd()
  dt.team_code <- fread(paste0(parent_dir, "/nba_team_code_map.csv"))
  
  
  url <- glue("https://www.basketball-reference.com/leagues/NBA_{year}_games-{month}.html")
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
  
  dt.games_list <- dt.games_list[dt >= start_date & dt <= end_date]
  dt.games_list_flat <- rbind(dt.games_list[,list(dt,
                                                  team = visitor)],
                              dt.games_list[,list(dt,
                                                  team = home)])
  dt.games_list_count <- dt.games_list_flat[,.N, by = list(team)]
  dt.games_list_count <- dt.games_list_count[order(N, decreasing = TRUE)]
  colnames(dt.games_list_count) <- c("team", "game_count")
  
  dt.final_team <- merge(dt.games_list_count, dt.team_code, by = c("team"))
  dt.final_team <- dt.final_team[order(game_count, decreasing = TRUE)]
  return (dt.final_team)
}

funcPrivateGetAllPlayers <- function(season = 2019){
  players <- NBAPerGameStatistics(season = season)
  dt.players <- data.table(players)
  dt.players <- dt.players[,list(player, 
                                 team_code = tm,
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
}

funcPublicGetPlayersProjectFantasyPoints <- function(start_date = Sys.Date(), 
                                                     end_date = ceiling_date(start_date, "week")){
  
  ########### Call private functions here ########
  dt.all_standings <- funcPrivateGetAllStandings()
  dt.final_team <- funcPrivateGetMonthlyGames(start_date = start_date,
                                              end_date = end_date)
  dt.players <- funcPrivateGetAllPlayers()
  ################################################
  
  
  
  dt.final_players <- dt.players[,list(player, game, team_code, avg_fantasy_points)]
  
  dt.final_data_set <- merge(dt.final_team, dt.final_players, by = c("team_code"))
  dt.final_data_set[,projected_fantasy_points := avg_fantasy_points * game_count]
  dt.final_data_set_with_games <- merge(dt.final_data_set, dt.all_standings, by = c("team"))
  return (dt.final_data_set_with_games)
}