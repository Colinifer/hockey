library(tidyverse)
library(DBI)
library(RPostgreSQL)


db_tables <- c(
  'events_summary',
  'game_info',
  'pbp_base',
  'pbp_extras',
  'player_periods',
  'player_shifts',
  'report',
  'roster',
  'schedule',
  'scratches',
  NULL
)

# Refresh and save NHL schedule
get_nhl_schedule <- function(x){

  # Scrape
  schedule_list <- nhlapi::nhl_schedule_seasons(x)
  
  
  n_games <- schedule_list[[1]]$totalItems
  # return(n_games)
  n_days <- schedule_list[[1]]$dates$games %>% length()
  # return(n_days)
  
  schedule <- map_dfr(1:n_days, function(x) {
    schedule_list %>% 
      nth(1) %>% 
      nth(8) %>% 
      pull(games) %>% 
      nth(x) %>% 
      as_tibble()
  })
  
  season_first_year <- schedule %>% 
    pull(gamePk) %>% 
    first() %>% 
    substr(1,4)
  
  season_full <- schedule %>% 
    pull(season) %>% 
    first()
  
  payload <- sc.scrape_schedule(
    start_date = schedule %>%
      pull(gameDate) %>%
      min() %>%
      as.Date(),
    # start_date = schedule %>% pull(gameDate) %>% min() %>% as.Date(),
    end_date = schedule %>%
      pull(gameDate) %>%
      max() %>%
      as.Date(),
    print_sched = FALSE
  ) %>% 
    as_tibble() %>% 
    mutate(season = season %>% as.integer())
  

  # Save scrape
  # Write local parquet
  # payload %>% 
  #   write_parquet(glue('data/schedule/{season_full}/schedule_{season_full}.parquet'))
  
  # Connect to DB
  con <- initR::fx.db_con(x.host = 'localhost')
  
  map("schedule", function(x){
    DBI::dbSendQuery(con,
                    glue('DELETE from {x} where season = {season_full}'))
  })
  
  map("schedule", function(x) {
    DBI::dbWriteTable(con,
                      x,
                      payload,
                      append = TRUE,
                      row.names = FALSE)
  })
  
  DBI::dbDisconnect(con)
  
  return(payload)
}

# Get season data
annual_nhl_query <- function(x, con = initR::fx.db_con(x.host = 'localhost')) {
  
  # print("Get schedule")
  schedule_df <- get_nhl_schedule(x) %>% 
    invisible()
  
  if (x <= 2008) {
    schedule_df <- schedule_df %>% 
      filter(session != 'PR')
  }
  
  # print("Got schedule")
  season_full <- schedule_df %>% 
    pull(season) %>% 
    first()
  
  # data_base_column_types <- readRDS('')
  # print(season)
  # print("Getting IDs and dates")
  existing_ids <- tbl(con, 'game_info') %>% 
    filter(season == season_full) %>% 
    pull(game_id)
  
  bad_ids <- c(
    2008020259,
    2008020409,
    2008020547,
    2008020548,
    2008020549,
    2008020550,
    2008020551,
    2008020552,
    2008020553,
    2008020554,
    2008020555,
    2008020556,
    2008020557,
    2008020558,
    2008020559,
    2008020560,
    2008020561,
    2008020562,
    2008020563,
    2008020564,
    2008020565,
    2008020566,
    2008020567,
    2008020568,
    2008020569,
    2008020570,
    2008020571,
    2008020572,
    2008020573,
    2008020574,
    2008020575,
    2021010010,
    2021010016,
    NULL)
  
  season_start <- schedule_df %>% 
    head(1) %>% 
    pull(game_date)
  
  season_end <- schedule_df %>% 
    tail(1) %>% 
    pull(game_date)
  
  dates <- seq.Date(season_start,
                    season_end, by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  # print("Got IDs and dates")
  
  print(date_grid)
  
  safe_scrape_pbp <- purrr::safely(sc.scrape_pbp)
  
  payload_and_upload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          scrape_ids <- schedule_df %>% 
                            filter(game_date >= date_grid$start_date[.x] & 
                                     game_date <= date_grid$end_date[.x] & 
                                     !(game_id %in% existing_ids) & 
                                     !(game_id %in% bad_ids) &
                                     game_id != '2020030134' &
                                     game_status == 'Final') %>% 
                            pull(game_id)
                          scrape_ids %>% print()
                          
                          print(glue('Scraping games {scrape_ids}'))
                          pbp_payload <- safe_scrape_pbp(scrape_ids)
                          payload_names <- pbp_payload %>% 
                            pluck(1) %>% 
                            names()
                          
                          if (scrape_ids %>% length() >= 1) {
                            
                            print(glue('Setting up db connection'))
                            con <- fx.db_con(x.host = 'localhost')
                            
                            # game_info_df
                            print(glue('Rbinding game_info'))
                            # rbind(
                            #   pbp_payload$result$game_info_df,
                            #   game_info_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>% 
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/game_info/{season_full}/game_info_{season_full}.parquet'))
                            print(glue('Saved game_info parquet...'))
                            game_info <- pbp_payload$result$game_info_df
                            game_info %>% pull(game_id)
                            game_ids <- game_info %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting game_info duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from game_info WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading game_info scrape'))
                            map("game_info", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                game_info,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding pbp_base'))
                            # rbind(
                            #   pbp_payload$result$pbp_base,
                            #   pbp_base_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/pbp_base/{season_full}/pbp_base_{season_full}.parquet'))
                            print(glue('Saved pbp_base parquet...'))
                            pbp_base <- pbp_payload$result$pbp_base
                            pbp_base %>% pull(game_id)
                            game_ids <- pbp_base %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting pbp_base duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from pbp_base WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading pbp_base scrape'))
                            map("pbp_base", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                pbp_base,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding pbp_extras'))
                            # rbind(
                            #   pbp_payload$result$pbp_extras,
                            #   pbp_extras_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/pbp_extras/{season_full}/pbp_extras_{season_full}.parquet'))
                            print(glue('Saved pbp_extras parquet...'))
                            pbp_extras <- pbp_payload$result$pbp_extras
                            pbp_extras %>% pull(game_id)
                            game_ids <- pbp_extras %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting pbp_extras duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from pbp_extras WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading pbp_extras scrape'))
                            map("pbp_extras", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                pbp_extras,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding player_shifts'))
                            # rbind(
                            #   pbp_payload$result$player_shifts,
                            #   player_shifts_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/player_shifts/{season_full}/player_shifts_{season_full}.parquet'))
                            print(glue('Saved player_shifts parquet...'))
                            player_shifts <- pbp_payload$result$player_shifts
                            player_shifts %>% pull(game_id)
                            game_ids <- player_shifts %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting player_shifts duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from player_shifts WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading player_shifts scrape'))
                            map("player_shifts", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                player_shifts,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding player_periods'))
                            # rbind(
                            #   pbp_payload$result$player_periods,
                            #   player_periods_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/player_periods/{season_full}/player_periods_{season_full}.parquet'))
                            print(glue('Saved player_periods parquet...'))
                            player_periods <- pbp_payload$result$player_periods
                            player_periods %>% pull(game_id)
                            game_ids <- player_periods %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting player_periods duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from player_periods WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading player_periods scrape'))
                            map("player_periods", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                player_periods,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding roster'))
                            # rbind(
                            #   pbp_payload$result$roster_df,
                            #   roster_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/roster/{season_full}/roster_{season_full}.parquet'))
                            print(glue('Saved roster parquet...'))
                            roster <- pbp_payload$result$roster_df
                            roster %>% pull(game_id)
                            game_ids <- roster %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting roster duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from roster WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading roster scrape'))
                            map("roster", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                roster,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding scratches'))
                            # rbind(
                            #   pbp_payload$result$scratches_df,
                            #   scratches_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/scratches/{season_full}/scratches_{season_full}.parquet'))
                            print(glue('Saved scratches parquet...'))
                            scratches <- pbp_payload$result$scratches_df
                            scratches %>% pull(game_id)
                            game_ids <- scratches %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting scratches duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from scratches WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading scratches scrape'))
                            map("scratches", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                scratches,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding events_summary'))
                            # rbind(
                            #   pbp_payload$result$events_summary_df,
                            #   events_summary_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/events_summary/{season_full}/events_summary_{season_full}.parquet'))
                            print(glue('Saved events_summary parquet...'))
                            events_summary <- pbp_payload$result$events_summary_df
                            events_summary %>% pull(game_id)
                            game_ids <- events_summary %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting events_summary duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from events_summary WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading events_summary scrape'))
                            map("events_summary", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                events_summary,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            print(glue('Rbinding report'))
                            # rbind(
                            #   pbp_payload$result$report,
                            #   report_ds %>%
                            #     filter(year == season_full & 
                            #              !(game_id %in% scrape_ids)) %>%
                            #     select(-year) %>%
                            #     collect()
                            # ) %>% 
                            #   write_parquet(glue('data/report/{season_full}/report_{season_full}.parquet'))
                            print(glue('Saved report parquet...'))
                            report <- pbp_payload$result$report
                            report %>% pull(game_id)
                            game_ids <- report %>% pull(game_id) %>% toString()
                            # game_info_df database
                            print(glue('Deleting report duplicates'))
                            DBI::dbSendQuery(con, glue('DELETE from report WHERE game_id IN ({paste0(game_ids, collapse = ',')});'))
                            
                            print(glue('Uploading report scrape'))
                            map("report", function(x) {
                              RPostgres::dbWriteTable(con,
                                                x,
                                                report,
                                                append = TRUE,
                                                row.names = FALSE)
                            })
                            
                            # return(pbp_payload$result)
                            
                            print(glue('Disconnecting from db'))
                            DBI::dbDisconnect(con)
                          }
                        })
  
  # map(db_tables, function(x){
  #   Filter(function(x) length(x) > 1, purrr::map(payload, 'result')) %>% 
  #     pluck(1, x)
  # })
  
  # payload_df <- purrr::map(payload_and_upload, 'result')
  # 
  # combined <- payload_df %>%
  #   dplyr::bind_rows()
  # 
  # combined
  
  gc()
}

# Mutate and add variables
format_append_nhl <- function(df) {
  
  # function for appending new variables to the data set
    additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}

# Automate uploading the database
delete_and_upload <- function(scrape, season,
                              db_driver = "PostgreSQL",
                              dbname = proj_name,
                              user =  Sys.getenv('db_user'),
                              password = Sys.getenv('db_password'),
                              host = ifelse(
                                jsonlite::fromJSON(readLines(
                                  "http://api.hostip.info/get_json.php",
                                  warn = F
                                ))$ip == Sys.getenv('ip'),
                                Sys.getenv('local'),
                                Sys.getenv('ip')
                              ),
                              port = Sys.getenv('postgres_port')) {
  if (!exists('proj_name') == TRUE) {
    stop("The value is TRUE, so the script must end here")
  }
  
  pg <- dbDriver(db_driver)
  
  con <- dbConnect(
    pg,
    dbname = dbname,
    user = user,
    password = password,
    host = host,
    port = port
  )
  
  query <- glue('DELETE from {.x} where season = {season}')
  
  map(.x = db_tables, ~ {
    DBI::dbGetQuery(con, glue('DELETE from {.x} where season = {season}'))
  })
  
  map(.x = db_tables,
      DBI::dbWriteTable(
        con,
        proj_name,
        .x,
        append = TRUE,
        row.names = FALSE
      ))
  
  DBI::dbDisconnect(con)
  rm(con)
}
# map(2020, annual_nhl_query)

#  %>% (con, 'game_info') %>%
#   group_by(season) %>%
#   count() %>%
#   collect()
fx.delete_db_index <- function(con) {
  queries <- c(
    'DROP INDEX game_info_games;',
    'DROP INDEX pbp_base_games;',
    'DROP INDEX pbp_extras_games;',
    'DROP INDEX player_periods_games;',
    'DROP INDEX player_shifts_games;',
    'DROP INDEX report_games;',
    'DROP INDEX roster_games;',
    'DROP INDEX schedule_games;',
    'DROP INDEX scratches_games;'
  )
  
  map(queries, function(x){
    print(queries)
    dbSendQuery(con, queries)
  })
}

fx.create_db_index <- function(con) {
  queries <- c(
    'CREATE INDEX game_info_games ON game_info (game_id);',
    'CREATE INDEX pbp_base_games ON pbp_base (game_id);',
    'CREATE INDEX pbp_extras_games ON pbp_extras (game_id);',
    'CREATE INDEX player_periods_games ON player_periods (game_id);',
    'CREATE INDEX player_shifts_games ON player_shifts (game_id);',
    'CREATE INDEX report_games ON report (game_id);',
    'CREATE INDEX roster_games ON roster (game_id);',
    'CREATE INDEX schedule_games ON schedule (game_id);',
    'CREATE INDEX scratches_games ON scratches (game_id);'
  )
  
  map(queries, function(x){
    print(queries)
    dbSendQuery(con, queries)
  })
}