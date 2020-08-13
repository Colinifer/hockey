pbp_scrape <- readRDS("data/pbp_scrape3.rds")
pbp_scrape$report$game_id %>% min()

f.scrape <- paste0("data/", list.files(path = "data/", pattern = "pbp_scrape"))

f.scrape[1:5]

fx.clean_scrape <- function(x) {
  y <- readRDS(x)
  y[grep("_df", names(y))]
  names(y)[1] <- y[1] %>% names() %>% substr(1, nchar(y[1] %>% names())-3)
  names(y)[6] <- y[6] %>% names() %>% substr(1, nchar(y[6] %>% names())-3)
  names(y)[7] <- y[7] %>% names() %>% substr(1, nchar(y[7] %>% names())-3)
  names(y)[8] <- y[8] %>% names() %>% substr(1, nchar(y[8] %>% names())-3)
  saveRDS(y, x)
  rm(y)
}

f.scrape %>% lapply(fx.clean_scrape)

# schedule <- sc.scrape_schedule(start_date = "2000-10-01", end_date = "2016-07-01")
# schedule1 <- sc.scrape_schedule(start_date = "2016-10-01", end_date = "2017-07-01")
# schedule2 <- sc.scrape_schedule(start_date = "2017-10-01", end_date = "2018-07-01")
# schedule3 <- sc.scrape_schedule(start_date = "2018-10-01", end_date = "2020-08-18")
# schedule <- rbind(schedule, schedule1, schedule2, schedule3)
# 
# source(f.con)
# DBI::dbWriteTable(con, value = schedule, name = "schedule")
# dbDisconnect(con)

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


# Functions ---------------------------------------------------------------

fx.add_to_table <- function(scrape) {
  source(f.con)
  table_names <- dbListTables(con)
  if (length(names(scrape)) == 9) {
    print("Hello")
    tic("Total Upload")
    fx.scrape_upload <- function(x) {
      tic(
        scrape[
          grep(x,
               scrape %>%
                 names()
               )
          ] %>%
          names()
        )
      dbWriteTable(
        con,
        value = as.data.frame(scrape[grep(x, names(scrape))]),
        name = paste0(table_names[grep(x, table_names)], "_test"),
        # remove test once proven
        append = TRUE
      )
      print(glue::glue("Successfully added ", x, "_test"))
      toc()
    }
    names(scrape) %>%
      lapply(fx.scrape_upload)
  }
  if (length(names(scrape)) != 9) {
    print("Error, the list has a longer length than 9")
  }
  dbDisconnect(con)
}


# Manual count ------------------------------------------------------------

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