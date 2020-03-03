userYear = 2000

if (userYear <= 2019) {
  schedule <- sc.scrape_schedule(start_date = paste(userYear, "-10-01", sep = ""), end_date = paste(userYear + 1, "-07-01", sep = ""), print_sched = TRUE)
  
  write.csv(schedule, file = paste("schedule/", userYear, "schedule.csv", sep = ""), row.names = FALSE)
  userYear = userYear + 1
}
