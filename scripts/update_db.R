library(tidyverse)
library(DBI)
library(RPostgreSQL)

annual_nhl_query <- function(season) {
  
  data_base_column_types <- readRDS('')
  
  schedule <- schedule_ds %>%
    filter(season == season) %>%
    collect() %>%
    group_by(season)
  
  season_start <- schedule %>% 
    head(1) %>% 
    pull(game_date)
  
  season_end <- schedule %>% 
    tail(1) %>% 
    pull(game_date)
  
  dates <- seq.Date(season_start,
                    season_end, by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_scrape_pbp <- purrr::safely(sc.scrape_pbp)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_scrape_pbp(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  combined <- payload_df %>%
    dplyr::bind_rows()
  
  combined
}


format_append_nhl <- function(df) {
  
  # function for appending new variables to the data set
    additional_info <- function(df) {
    
    # apply additional coding for custom variables
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
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


# automate uploading the database

delete_and_upload <- function(df, 
                              year, 
                              db_driver = "PostgreSQL", 
                              dbname = proj_name, 
                              user =  Sys.getenv('db_user'), 
                              password = Sys.getenv('db_password'), 
                              host = ifelse(
                                jsonlite::fromJSON(
                                  readLines("http://api.hostip.info/get_json.php",
                                            warn = F)
                                )$ip == Sys.getenv('ip'),
                                Sys.getenv('local'),
                                Sys.getenv('ip')
                              ), 
                              port = Sys.getenv('postgres_port')) {
  
  if (!exists('proj_name') == TRUE) {stop("The value is TRUE, so the script must end here")}
  
  pg <- dbDriver(db_driver)
  
  hockey_db <- dbConnect(pg, 
                           dbname = dbname, 
                           user = user, 
                           password = password,
                           host = host, 
                           port = port)
  
  query <- paste0('DELETE from hockey_db where game_year = ', year)
  
  DBI::dbGetQuery(hockey_db, query)
  
  DBI::dbWriteTable(hockey_db, "hockey", df, append = TRUE, row.names = FALSE)
  
  DBI::dbDisconnect(hockey_db)
  rm(hockey_db)
}


con <- dbConnect(
  RPostgres::Postgres(),
  host = ifelse(
    fromJSON(
      readLines("http://api.hostip.info/get_json.php",
                warn = F)
    )$ip == Sys.getenv('ip'),
    Sys.getenv('local'),
    Sys.getenv('ip')
  ),
  port = Sys.getenv('postgres_port'),
  user = Sys.getenv('db_user'),
  password = Sys.getenv('db_password'),
  dbname = proj_name,
  # database = "football",
  # Server = "localhost\\SQLEXPRESS",
  # Database = "datawarehouse",
  NULL
)

db_tables <- (
  'events_summary',
  'game_info',
  'pbp_base',
  'pbp_extras',
  'player_periods',
  'player_shifts',
  'report',
  'roster',
  'schedule',
  'scratches'
)

map(.x = db_tables,
    ~{tbl(con, x) %>%
        filter(game_year == 2008) %>%
        count() %>%
        collect()
    })


# map(.x = seq(2009, 2019, 1), 
map(.x = 2021,
    ~{payload_statcast <- annual_nhl_query(season = .x)
    
    message(paste0('Formatting payload for ', .x, '...'))
    
    df <- format_append_statcast(df = payload_statcast)
    
    message(paste0('Deleting and uploading ', .x, ' data to database...'))
    
    delete_and_upload(df, 
                      year = .x, 
                      db_driver = 'PostgreSQL', 
                      dbname = proj_name, 
                      user = Sys.getenv('db_user'), 
                      password = Sys.getenv('db_password'), 
                      host = ifelse(
                        fromJSON(
                          readLines("http://api.hostip.info/get_json.php",
                                    warn = F)
                        )$ip == Sys.getenv('ip'),
                        Sys.getenv('local'),
                        Sys.getenv('ip')
                      ), 
                      port = Sys.getenv('postgres_port'))
    
    message('Sleeping and collecting garbage...')
    
    Sys.sleep(5*60)
    
    gc()
    
    })


tbl(con, 'statcast') %>%
  group_by(game_year) %>%
  count() %>%
  collect()



dbGetQuery(con, "drop index statcast_index")

dbGetQuery(con, "create index statcast_index on statcast (game_date)")

dbGetQuery(con, "drop index statcast_game_year")

dbGetQuery(con, "create index statcast_game_year on statcast (game_year)")

dbGetQuery(con, "drop index statcast_type")

dbGetQuery(con, "create index statcast_type on statcast (type)")

dbGetQuery(con, "drop index statcast_pitcher_index")

dbGetQuery(con, "create index statcast_pitcher_index on statcast (pitcher)")

dbGetQuery(con, "drop index statcast_batter_index")

dbGetQuery(con, "create index statcast_batter_index on statcast (batter)")
