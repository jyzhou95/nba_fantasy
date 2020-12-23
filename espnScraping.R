library(RSelenium)
library(rvest)
library(data.table)
library(glue)
library(jsonlite)

url = "https://fantasy.espn.com/basketball/team?leagueId=1693628562&teamId=1&seasonId=2021"

driver <- rsDriver(browser=c("chrome"), port = 4444L, chromever="87.0.4280.88")
remDr <- driver[["client"]]
# remDr$open()
remDr$navigate(url)

# Login page is in a different frame
# iframe = remDr$findElement(using = "xpath", value = '//*[@id="disneyid-secure-responder"]')
# Switch to frame
# remDr$switchToFrame(iframe$getElementAttribute("id")[[1]])
# remDr$click(1)
# remDr$sendKeysToActiveElement(sendKeys = list(key = "tab"))
# remDr$sendKeysToActiveElement(sendKeys = list("user", key = "tab"))
# remDr$sendKeysToActiveElement(sendKeys = list("password", key = "tab"))
# remDr$sendKeysToActiveElement(sendKeys = list(key = "enter"))

# Owner order
vec.owner <- c("me", "eddie", "alex", "lee", "eugene",
               "stephen", "kyle", "liam", "greg", "daniel")
# Backfill
# Determine latest date we've scraped
# dt.historical_performance <- fread("D:/Desktop/nba_fantasy/fantasy_league_performance.csv")

# Determine latest date to fill
dt.nba_period <- fread("D:/git_repos/nba_fantasy/period_dates.csv")

vec.owner_id <- c(1:10)
# vec.period <- c((max(dt.historical_performance$period)):(max(dt.nba_period[dt < Sys.Date()]$period)))
vec.period <- c(1:6)

# Scrape historical performance
dt.write.this <- rbindlist(lapply(vec.owner_id, function(x){
  dt.temp <- rbindlist(lapply(vec.period, function(y){
    Sys.sleep(2)
    print(paste("Owner ID: ", x))
    print(paste("Game: ", y))
    
    api_url <- glue("http://fantasy.espn.com/apis/v3/games/fba/seasons/2021/segments/0/leagues/1693628562?forTeamId={x}&scoringPeriodId={y}&view=mRoster")
    remDr$navigate(api_url)
    my_df <- fromJSON(html_text(read_html(remDr$getPageSource()[[1]])))
    my_df_final2 <- my_df$teams$roster$entries[[1]]
    
    projected_url <- glue("http://fantasy.espn.com/apis/v3/games/fba/seasons/2021/segments/0/leagues/1693628562?forTeamId={x}&scoringPeriodId={y}&statSplit=projections&view=mRoster")
    remDr$navigate(projected_url)
    projected_df <- fromJSON(html_text(read_html(remDr$getPageSource()[[1]])))
    projected_df_2 <- projected_df$teams$roster$entries[[1]]
    
    dt.return.this <- data.table(owner = vec.owner[x],
                                 period = y,
                                 player = my_df_final2$playerPoolEntry$player$fullName,
                                 actual_fantasy_points = my_df_final2$playerPoolEntry$appliedStatTotal,
                                 predicted_fantasy_points = unlist(lapply(1:length(my_df_final2$playerPoolEntry$player$fullName),
                                                                          function(x){
                                                                            if (projected_df_2$playerPoolEntry$player$stats[x][[1]]$appliedAverage[5] > 0){
                                                                              return(projected_df_2$playerPoolEntry$player$stats[x][[1]]$appliedAverage[5])
                                                                            } else{
                                                                              return(projected_df_2$playerPoolEntry$player$stats[x][[1]]$appliedAverage[4])
                                                                            }
                                                                            })),
                                 lineup = my_df_final2$lineupSlotId) # 12 means you got benched
    print(dt.return.this)

    return (dt.return.this)
  }))
  return (dt.temp)
}))

# Get historical averages
# projected_url <- "http://fantasy.espn.com/apis/v3/games/fba/seasons/2021/segments/0/leagues/1693628562?view=mMatchup&view=mMatchupScore"
# remDr$navigate(projected_url)
# projected_df <- fromJSON(html_text(read_html(remDr$getPageSource()[[1]])))
# projected_df



remDr$closeall()
rm(driver)
gc()

# dt.bind.to.this <- fread("D:/Desktop/nba_fantasy/shiny/fantasy_league_performance.csv")
# dt.bind.to.this[,V1 := NULL]
# dt.bind.to.this <- rbind(dt.bind.to.this, dt.write.this)

write.csv(x = dt.write.this,
          file = "D:/git_repos/nba_fantasy/shiny/fantasy_league_performance.csv",
          row.names = F)
