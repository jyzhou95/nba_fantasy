library(data.table)

dt.nba_schedule_raw <- fread("D:/Desktop/nba_fantasy/nba_2018_schedule.csv")
dt.nba_schedule_raw[,dt := as.Date(Date, "%d/%m/%Y")]

dt.unique_dates <- dt.nba_schedule_raw[,list(dt)]
dt.unique_dates <- unique(dt.unique_dates)
dt.unique_dates <- dt.unique_dates[order(dt)]
dt.unique_dates[,period := 1:nrow(dt.unique_dates)]
write.csv(dt.unique_dates, "D:/Desktop/nba_fantasy/period_dates.csv")

