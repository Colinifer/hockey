pbp_scrape <- readRDS("data/pbp_scrape3.rds")
pbp_scrape$report$game_id %>% min()

source(f.con)
schedule <- dbGetQuery(
  con,
  "SELECT * FROM `hockey`.`schedule` WHERE (`season` > '2009') AND (`game_status` = 'Final') ORDER BY `game_id`"
)
existing_report <- tbl(con, "report")
dbDisconnect(con)

if_else(
  pbp_scrape$report$game_id %>% 
    min() %in% existing_report$game_id,
  true = stop(),
  false = 
)

fx.add_to_table <- function(scrape) {
  if (length(names(scrape)) == 9) {
    print("Hello")
    tic("Total Upload")
    names(scrape) %>% 
      lapply(function(x)
        tic(x)
        dbWriteTable(
          con,
          value = scrape[],
          name = "game_info",
          append = TRUE
        )
        print(glue::glue("Successfully added ", x))
        toc()
        )
  } 
  if (length(names(pbp_scrape)) != 9) {
    print("Error, the list has a longer length than 9")
  }
  
  tic("Total Upload")
  tic("game_info")
  dbWriteTable(
    con,
    value = scrape$game_info_df,
    name = "game_info",
    append = TRUE
  )
  print("Successfully added game_info")
  toc()
  tic("pbp_base")
  dbWriteTable(
    con,
    value = scrape$pbp_base,
    name = "pbp_base",
    append = TRUE
  )
  print("Successfully added pbp_base")
  toc()
  tic("pbp_extras")
  dbWriteTable(
    con,
    value = scrape$pbp_extras,
    name = "pbp_extras",
    append = TRUE
  )
  print("Successfully added pbp_extras")
  toc()
  tic("player_shifts")
  dbWriteTable(
    con,
    value = scrape$player_shifts,
    name = "player_shifts",
    append = TRUE
  )
  print("Successfully added player_shifts")
  toc()
  tic("player_periods")
  dbWriteTable(
    con,
    value = scrape$player_periods,
    name = "player_periods",
    append = TRUE
  )
  print("Successfully added player_periods")
  toc()
  tic("roster")
  dbWriteTable(
    con,
    value = scrape$roster_df,
    name = "roster",
    append = TRUE)
  print("Successfully added roster")
  toc()
  tic("scratches")
  dbWriteTable(
    con,
    value = scrape$scratches_df,
    name = "scratches",
    append = TRUE)
  print("Successfully added scratches")
  toc()
  tic("events_summary")
  dbWriteTable(
    con,
    value = scrape$events_summary_df,
    name = "events_summary",
    append = TRUE
  )
  print("Successfully added events_summary")
  toc()
  tic("report")
  dbWriteTable(
    con,
    value = scrape$report,
    name = "report",
    append = TRUE)
  print("Successfully added report")
  toc()
  print("Successfully uploaded latest scrape to DB!")
  toc()
  toc()
  dbDisconnect(con)
}