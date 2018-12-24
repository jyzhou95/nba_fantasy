
server <- function(input, output, session) {
  getData <- reactive({
    start_date <- input$NBA_Start_Date
    # Get Sunday of this week
    sunday <- ceiling_date(start_date, "week")
    
    chr.month <- tolower(as.character(month(start_date, label = TRUE, abbr = FALSE)))
    
    url <- glue("https://www.basketball-reference.com/leagues/NBA_2019_games-{chr.month}.html")
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
    
    dt.final_players[,owner := "none"]
    dt.final_players[,status := "available"]
    dt.final_players[player %in% input$myPlayers]$owner <- "me"
    dt.final_players[player %in% input$alexPlayers]$owner <- "alex"
    dt.final_players[player %in% input$kylePlayers]$owner <- "kyle"
    dt.final_players[player %in% input$edwardPlayers]$owner <- "edward"
    dt.final_players[player %in% input$danielPlayers]$owner <- "daniel"
    dt.final_players[player %in% input$liamPlayers]$owner <- "liam"
    dt.final_players[owner != "none",status := "unavailable"]
    
    dt.final_data_set <- merge(dt.final_team, dt.final_players, by = c("team_code"))
    dt.final_data_set[,projected_fantasy_points := avg_fantasy_points * game_count]
    dt.final_data_set_with_games <- merge(dt.final_data_set, dt.all_standings, by = c("team"))
    
    dt.final_data_set_with_games[grepl("G", position)]$position <- "G"
    dt.final_data_set_with_games[grepl("F", position)]$position <- "F"
    
    #Get injuries data
    dt.final_data_set_with_games <- merge(dt.final_data_set_with_games,
                                          dt.injuries, by = c("player"),
                                          all.x = TRUE)
    dt.final_data_set_with_games[is.na(injured)]$injured <- FALSE
    
    
    if (nrow(dt.final_data_set_with_games[injured == TRUE])){
      dt.final_data_set_with_games[injured == TRUE]$status <- "unavailable"
    }
    
    # Merge on the schedule
    dt.player_schedule <- merge(merge(dt.games_list_flat, dt.final_team[,list(team, team_code, game_count)], by = c("team")),
                                dt.final_players[owner != "none"], by = c("team_code"), allow.cartesian = TRUE)
    
    
    return (list(dt.final_data_set_with_games, dt.player_schedule))
  })
  
  output$fantasyProjections <- renderPlotly({
    
    lst.final_data_set_with_games <- getData()
    dt.final_data_set_with_games <- lst.final_data_set_with_games[[1]]
    dt.player_schedule <- lst.final_data_set_with_games[[2]]
    player_position <- input$player_position
    player_status <- input$status
    team_owner <- input$owner
    
    if (player_position != "ALL"){
      dt.final_data_set_with_games_plot <- dt.final_data_set_with_games[position == player_position]
    } else{
      dt.final_data_set_with_games_plot <- dt.final_data_set_with_games
    }
    
    if (player_status != "all"){
      dt.final_data_set_with_games_plot <- dt.final_data_set_with_games_plot[status == player_status]
    }
    
    if (team_owner != "all"){
      dt.final_data_set_with_games_plot <- dt.final_data_set_with_games_plot[owner == team_owner]
    }
    
    my_plt <- ggplot(dt.final_data_set_with_games_plot, aes(x = minutes_played, 
                                                            y = projected_fantasy_points,
                                                            color = paste(game_count, status),
                                                            text = paste0("Projected fantasy points: ", projected_fantasy_points,
                                                                          "\nPlayer: ", player,
                                                                          "\nPosition: ", position))) + 
      geom_point(position = "jitter") + 
      geom_hline(yintercept = min(dt.final_data_set_with_games[owner == "me"]$projected_fantasy_points), linetype = "dashed") +
      xlab("Minutes Played") + ylab("Projected fantasy points") + 
      theme_bw(base_size = 15) + scale_color_brewer(palette = "Set1")
    
    ggplotly(my_plt, tooltip = c("text"))
    
  })
  
  output$projectionTables <- renderDataTable({
    
    lst.final_data_set_with_games <- getData()
    dt.player_schedule <- lst.final_data_set_with_games[[2]]
    dt.player_schedule <- dt.player_schedule[order(avg_fantasy_points, decreasing = TRUE)]
    dt.return.this <- dt.player_schedule[, head(.SD, 10), by=list(owner, dt)]
    
    dt.num_games <- dt.player_schedule[,.N, by = list(owner)]
    
    dt.return.this <- dt.return.this[,list(projected_fantasy_score = sum(avg_fantasy_points)),
                                           by = list(owner)][owner != "none"]
    dt.return.this <- merge(dt.return.this, dt.num_games, by = c("owner"))
    
    return (dt.return.this)
  })
  
  output$weekly_performance <- renderPlotly({
    lst.final_data_set_with_games <- getData()
    dt.player_projected_fantasy <- lst.final_data_set_with_games[[1]][,list(player, avg_fantasy_points)]
    dt.player_schedule <- lst.final_data_set_with_games[[2]]
    if (input$NBA_Start_Date > Sys.Date()){
      dt.player_schedule <- dt.player_schedule[order(avg_fantasy_points, decreasing = TRUE)]
      dt.player_schedule[, head(.SD, 10), by=list(owner, dt)]
      dt.plot.this.predicted <- dt.player_schedule[,list(fantasy_points = sum(avg_fantasy_points),
                                                     type = "predicted"),
                                               by = list(dt, owner)]
      plt <- ggplot(dt.plot.this.predicted, aes(x = dt, y = fantasy_points, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") + theme_bw(base_size = 15) +
        facet_wrap(~owner) + ylab("Fantasy Points")
    } else{
      # Get player actual performance
      dt.actual_performance_subset <- dt.actual_performance[dt >= min(dt.player_schedule$dt) & dt <= max(dt.player_schedule$dt)]
      dt.actual_performance_subset <- merge(dt.actual_performance_subset, dt.player_projected_fantasy, by = c("player"))
      dt.actual_performance_subset <- dt.actual_performance_subset[lineup != 12]
      dt.actual_performance_subset <- dt.actual_performance_subset[actual_fantasy_points != 0]
      
      if (input$playerPerformance){
        dt.actual_performance_aggregate <- dt.actual_performance_subset[,list(actual_fantasy_points = sum(actual_fantasy_points),
                                                                              projected_fantasy_points = sum(avg_fantasy_points)),
                                                                        by = list(owner, player)]
        
        dt.actual_performance_aggregate[,id := paste0(player, "_", owner)]
        dt.actual_performance_aggregate <- dt.actual_performance_aggregate[order(actual_fantasy_points, decreasing = TRUE)]
        dt.actual_performance_aggregate$id <- factor(dt.actual_performance_aggregate$id, levels = dt.actual_performance_aggregate$id)
        
        dt.actual_performance_aggregate <- melt(dt.actual_performance_aggregate, id.vars = c("owner", "player", "id"),
                                                measure.vars = c("actual_fantasy_points", "projected_fantasy_points"))
        
        
        plt <- ggplot(data = dt.actual_performance_aggregate, aes(x = id, y = value, fill = variable)) +
          geom_bar(stat = "identity", position = "dodge") + theme_bw(base_size = 15) + scale_fill_brewer(palette = "Set1") +
          facet_wrap(~owner, scales = "free_x") + ylab("Fantasy Points") + theme(axis.text.x = element_text(angle = 45)) + ylab("") +
          xlab("")
      } else{
        dt.actual_performance_aggregate <- dt.actual_performance_subset[,list(actual_fantasy_points = sum(actual_fantasy_points),
                                                                              projected_fantasy_points = sum(avg_fantasy_points)),
                                                                        by = list(owner, dt)]
        
        
        dt.actual_performance_aggregate$dt <- as.Date(dt.actual_performance_aggregate$dt)
        dt.actual_performance_aggregate <- melt(dt.actual_performance_aggregate, id.vars = c("owner", "dt"),
                                                measure.vars = c("actual_fantasy_points", "projected_fantasy_points"))
        
        plt <- ggplot(dt.actual_performance_aggregate, aes(x = dt, y = value, fill = variable)) +
          geom_bar(stat = "identity", position = "dodge") + theme_bw(base_size = 15) + scale_fill_brewer(palette = "Set1") +
          facet_wrap(~owner) + ylab("Fantasy Points")
      }
    }
    ggplotly(plt)
  })
  
  weekly_player_performance <- renderPlotly({
    lst.final_data_set_with_games <- getData()
    dt.player_projected_fantasy <- lst.final_data_set_with_games[[1]][,list(player, avg_fantasy_points)]
    dt.player_schedule <- lst.final_data_set_with_games[[2]]
  })
  
  output$timeSeries <- renderPlotly({
    
    # Analyze player
    my_player <- input$player
    my_year <- input$year
    link <- paste0("https://www.basketball-reference.com",
                   gsub(".html", glue("/gamelog/{my_year}/"), paste0(dt.player_links[player == my_player]$link)))
    
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
  })
  
  output$playerBeta <- renderDataTable({
    
    dt.all_player_data <- rbindlist({
      lapply(dt.player_links$player, function(x){
        print(x)
        link <- paste0("https://www.basketball-reference.com",
                       gsub(".html", glue("/gamelog/2019/"), paste0(dt.player_links[player == x]$link)))
        
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
  })
  
}