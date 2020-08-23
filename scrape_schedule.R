userDate <- Sys.Date()

# Get season (if after Jan 1, substract one digit)
userYear <- if (as.integer(substr(userDate, 6, 7)) > 01 & as.integer(substr(userDate, 6, 7)) < 10) {
  as.integer(substr(userDate, 1, 4)) - 1
} else {
  as.integer(substr(userDate, 1, 4))
}

start_date_csv <- "2019-10-01"
start_date_db <- "2019-08-11"
end_date <- "2020-09-30"
  
fschedule <- paste("schedule/", userYear, "schedule.csv", sep = "")

schedule <- sc.scrape_schedule(start_date = start_date_csv, end_date = end_date, print_sched = TRUE)
schedule %>% write_csv(fschedule)

schedule <- sc.scrape_schedule(start_date = start_date_db, end_date = end_date, print_sched = TRUE)
source("../initR/con.R")
dbWriteTable(
  con,
  value = schedule,
  name = deparse(substitute(schedule)),
  # name = paste0(table_names[grep(x, table_names)], "_test"),
  # remove test once proven
  append = TRUE
)
dbDisconnect(con)


# 
# 
# 
# if (userYear <= 2019) {
#   schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), end_date = paste(userYear + 1, "-07-01", sep = ""), print_sched = TRUE)
#   write.csv(schedule, file = fschedule, row.names = FALSE)
#   dbWriteTable(con, )
#   userYear = userYear + 1
# }
# 
# 
# schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), end_date = paste(userYear + 1, "-07-01", sep = ""), print_sched = TRUE)
# 
# write.csv(schedule, file = fschedule, row.names = FALSE)
# 
# schedule <- read.csv(file = fschedule)
# tibble(schedule)
# 
# today <- schedule %>% 
#   filter(game_date == paste(Sys.Date()))
# tibble(today)
# 
# sc.scrape_events_HTM(today$game_id[1])