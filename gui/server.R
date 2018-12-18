server <- function(input, output, session) {
  output$fantasyPoints <- renderPlotly({
    my_plt <- ggplot(dt.final_data_set_with_games, aes(x = game/total_games, 
                                                       y = projected_fantasy_points, 
                                                       color = as.character(game_count),
                                                       text = paste0("Projected fantasy points: ", projected_fantasy_points,
                                                                     "\nPlayer: ", player))) + 
      geom_point() + xlab("Percent of game started") + ylab("Projected fantasy points") + 
      theme_bw(base_size = 15) + scale_color_brewer(palette = "Set1")
    
    ggplotly(my_plt, tooltip = c("text"))
  })
  
  
}