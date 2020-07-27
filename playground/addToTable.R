# dbWriteTable(
#   con,
#   value = sc.scrape_schedule(
#     start_date = as.Date('2019-10-02'),
#     end_date =   Sys.Date() + 20
#   ),
#   name = "schedule",
#   append = TRUE
# )

# dbWriteTable(con, value = dbReadTable(con, "2019schedule"), name = "schedule", append = TRUE)

source("../initR/con.R")
schedule <- dbGetQuery(
  con,
  "SELECT * FROM `hockey`.`schedule` WHERE (`season` > '2009') AND (`game_status` = 'Final') ORDER BY `game_id`"
  )
dbDisconnect(con)

game_ids <- schedule %>% 
  filter(season > 2009) %>% 
  select(game_id) %>% 
  pull(game_id)

# game_ids <- game_ids$game_id

# dbWriteTable(
#   con,
#   value = sc.scrape_schedule(
#     start_date = as.Date('2019-10-02'),
#     end_date =   Sys.Date() + 20
#   ),
#   name = "schedule"
#   # append = TRUE
# )

# Get existing Game IDs from DB
source("../initR/con.R")
existing_game_ids <- dbGetQuery(con, "SELECT game_id FROM pbp_base;")
dbDisconnect(con)
# Create int of latest IDs
id_latest <- as.integer(substr(
  existing_game_ids$game_id[nrow(existing_game_ids)],
  7,
  nchar(existing_game_ids$game_id[nrow(existing_game_ids)])
))

pbp_scrape <-
  sc.scrape_pbp(games = game_ids[(id_latest + 1):(id_latest + u.scrape_interval)]) # 300 was last

source("../initR/con.R")

fx.append <- function(x){
  dbApp
}

qry_list <- names(pbp_scrape)

tic("Total Upload")
tic("game_info")
dbWriteTable(
  con,
  value = pbp_scrape$game_info_df,
  name = "game_info",
  append = TRUE
  )
print("Successfully added game_info")
toc()
tic("pbp_base")
dbWriteTable(
  con,
  value = pbp_scrape$pbp_base,
  name = "pbp_base",
  append = TRUE
  )
print("Successfully added pbp_base")
toc()
tic("pbp_extras")
dbWriteTable(
  con,
  value = pbp_scrape$pbp_extras,
  name = "pbp_extras",
  append = TRUE
  )
print("Successfully added pbp_extras")
toc()
tic("player_shifts")
dbWriteTable(
  con,
  value = pbp_scrape$player_shifts,
  name = "player_shifts",
  append = TRUE
  )
print("Successfully added player_shifts")
toc()
tic("player_periods")
dbWriteTable(
  con,
  value = pbp_scrape$player_periods,
  name = "player_periods",
  append = TRUE
)
print("Successfully added player_periods")
toc()
tic("roster")
dbWriteTable(
  con,
  value = pbp_scrape$roster_df,
  name = "roster",
  append = TRUE)
print("Successfully added roster")
toc()
tic("scratches")
dbWriteTable(
  con,
  value = pbp_scrape$scratches_df,
  name = "scratches",
  append = TRUE)
print("Successfully added scratches")
toc()
tic("events_summary")
dbWriteTable(
  con,
  value = pbp_scrape$events_summary_df,
  name = "events_summary",
  append = TRUE
)
print("Successfully added events_summary")
toc()
tic("report")
dbWriteTable(
  con,
  value = pbp_scrape$report,
  name = "report",
  append = TRUE)
print("Successfully added report")
toc()
print("Successfully uploaded latest scrape to DB!")
toc()
dbDisconnect(con)