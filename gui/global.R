library(shiny)
library(plotly)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

source(paste0(parent_dir, "/funcUtil.R"))

dt.my_players <- fread(paste0(parent_dir, "/my_roster.csv"))
dt.final_data_set_with_games <- funcPublicGetPlayersProjectFantasyPoints()



