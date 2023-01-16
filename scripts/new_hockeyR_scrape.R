map(.x = c(2010:2022), function(season) {
  full_season <- paste(season, substr(season+1, 3, 4), sep = "_")
  print(full_season)
  
  pbp_path <- glue('https://github.com/danmorse314/hockeyR-data/blob/main/data/play_by_play_{full_season}.rds?raw=true')
  
  pbp_payload <- readRDS(url(pbp_path)) |> 
    mutate(season = season %>% as.double(),
           date_time = date_time %>% lubridate::as_datetime(),
           home_id = home_id %>% as.integer(),
           away_id = away_id %>% as.integer(),
           game_length = game_length %>% as.double(),
    )
  
  print(pbp_payload)
  
  con <- initR::fx.db_con(x.host = 'localhost')
  RPostgres::dbWriteTable(con,
                          'hockeyR_pbp_xg',
                          pbp_payload,
                          append = TRUE,
                          row.names = FALSE)
  dbDisconnect(con)
})
