userYear = 2019
userDate <- Sys.Date()
fschedule <- paste("schedule/", userYear, "schedule.csv", sep = "")

if (userYear <= 2019) {
  schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), end_date = paste(userYear + 1, "-07-01", sep = ""), print_sched = TRUE)
  
  write.csv(schedule, file = fschedule, row.names = FALSE)
  userYear = userYear + 1
}


schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), end_date = paste(userYear + 1, "-07-01", sep = ""), print_sched = TRUE)

write.csv(schedule, file = fschedule, row.names = FALSE)

schedule <- read.csv(file = fschedule)
tibble(schedule)

today <- schedule %>% 
  filter(game_date == paste(Sys.Date()))
tibble(today)

sc.scrape_events_HTM(today$game_id[1])