source(f.con)
pbp_extras <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_extras` WHERE ('game_id' >= '20142015');")
pbp_extras_dups <- pbp_extras %>% distinct()
dbSendQuery(con, "DELETE FROM `hockey`.`pbp_extras` WHERE ('season' >= '20142015');")
dbWriteTable(con, value = pbp_extras_dups, name = "pbp_extras", append = TRUE)

pbp_base <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_base` WHERE ('season' >= '20142015');")
pbp_base_dups <- pbp_base %>% distinct()
dbSendQuery(con, "DELETE FROM `hockey`.`pbp_base` WHERE ('season' >= '20142015');")
dbWriteTable(con, pbp_base_dups, "pbp_base", append = TRUE)

dbDisconnect(con)


pbp_scrape_x <- readRDS(file = "data/pbp_scrape2.rds")

source(f.con)
tic("Total Upload")
tic("game_info")
dbWriteTable(
  con,
  value = pbp_scrape_x$game_info_df,
  name = "game_info",
  append = TRUE
)
print("Successfully added game_info")
toc()
tic("pbp_base")
dbWriteTable(
  con,
  value = pbp_scrape_x$pbp_base,
  name = "pbp_base",
  append = TRUE
)
print("Successfully added pbp_base")
toc()
tic("pbp_extras")
dbWriteTable(
  con,
  value = pbp_scrape_x$pbp_extras,
  name = "pbp_extras",
  append = TRUE
)
print("Successfully added pbp_extras")
toc()
tic("player_shifts")
dbWriteTable(
  con,
  value = pbp_scrape_x$player_shifts,
  name = "player_shifts",
  append = TRUE
)
print("Successfully added player_shifts")
toc()
tic("player_periods")
dbWriteTable(
  con,
  value = pbp_scrape_x$player_periods,
  name = "player_periods",
  append = TRUE
)
print("Successfully added player_periods")
toc()
tic("roster")
dbWriteTable(
  con,
  value = pbp_scrape_x$roster_df,
  name = "roster",
  append = TRUE)
print("Successfully added roster")
toc()
tic("scratches")
dbWriteTable(
  con,
  value = pbp_scrape_x$scratches_df,
  name = "scratches",
  append = TRUE)
print("Successfully added scratches")
toc()
tic("events_summary")
dbWriteTable(
  con,
  value = pbp_scrape_x$events_summary_df,
  name = "events_summary",
  append = TRUE
)
print("Successfully added events_summary")
toc()
tic("report")
dbWriteTable(
  con,
  value = pbp_scrape_x$report,
  name = "report",
  append = TRUE)
print("Successfully added report")
toc()
print("Successfully uploaded latest scrape to DB!")
toc()
toc()