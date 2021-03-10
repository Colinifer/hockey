current_season <- 2020

schedule_df <- map_df(current_season, function(x){
  schedule_list <- nhlapi::nhl_schedule_seasons(x)
  
  
  n_games <- schedule_list[[1]]$totalItems
  # return(n_games)
  n_days <- schedule_list[[1]]$dates$games %>% length()
  # return(n_days)
  
  schedule <- map_df(1:n_days, function(x) {
    schedule_list %>% 
      nth(1) %>% 
      nth(7) %>% 
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
  
  sc.scrape_schedule(
    start_date = schedule %>%
      pull(gameDate) %>%
      min() %>%
      as.Date(),
    # start_date = schedule %>% pull(gameDate) %>% min() %>% as.Date(),
    end_date = schedule %>%
      pull(gameDate) %>%
      max() %>%
      as.Date()
  ) %>% 
    as_tibble() %>% 
    write_parquet(glue('data/schedule/{season_full}/schedule_{season_full}.parquet'))
})

# Peak at schedule
schedule_df %>% 
  as_tibble() %>% 
  filter(game_date == Sys.Date())

# Get all IDs currently in database
existing_game_ids <- game_info_ds %>% 
  collect() %>% 
  pull(game_id) %>% 
  unique()

# Get all possible IDs
complete_game_ids <- schedule_ds %>% 
  filter(session == 'R') %>% 
  collect() %>% 
  pull(game_id)

# Get missing IDs
new_game_ids <- schedule_ds %>% 
  filter(session == 'R' & 
           game_id != '2010020124' &
           game_id != '2019030016' &
           !(game_id %in% existing_game_ids) & 
           game_status == 'Final' & 
           EST_date <= Sys.Date()) %>% 
  collect() %>%
  pull(game_id)

new_game_ids

# Create season for missing IDs
new_season <- schedule_ds %>% 
  filter(session == 'R' & 
           game_id != '2010020124' &
           game_id != '2019030016' & 
           !(game_id %in% existing_game_ids)) %>% 
  collect() %>% 
  pull(season) %>% 
  first()

new_season


# EH scrape ---------------------------------------------------------------

# Scrape missing IDs
pbp_scrape <- sc.scrape_pbp(new_game_ids)

# List elements from scrape
# 
# pbp_scrape$game_info_df       ## game information data
# pbp_scrape$pbp_base           ## main play-by-play data
# pbp_scrape$pbp_extras         ## extra play-by-play data
# pbp_scrape$player_shifts      ## full player shifts data
# pbp_scrape$player_periods     ## player TOI sums per period
# pbp_scrape$roster_df          ## roster data
# pbp_scrape$scratches_df       ## scratches data
# pbp_scrape$events_summary_df  ## event summary data
# pbp_scrape$report             ## scrape report

# Bind the new scrape to existing files
rbind(
  pbp_scrape$game_info_df,
  game_info_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>% 
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/game_info/{new_season}/game_info_{new_season}.parquet'))

rbind(
  pbp_scrape$pbp_base,
  pbp_base_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/pbp_base/{new_season}/pbp_base_{new_season}.parquet'))

rbind(
  pbp_scrape$pbp_extras,
  pbp_extras_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/pbp_extras/{new_season}/pbp_extras_{new_season}.parquet'))

rbind(
  pbp_scrape$player_shifts,
  player_shifts_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/player_shifts/{new_season}/player_shifts_{new_season}.parquet'))

rbind(
  pbp_scrape$player_periods,
  player_periods_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/player_periods/{new_season}/player_periods_{new_season}.parquet'))

rbind(
  pbp_scrape$roster_df,
  roster_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/roster/{new_season}/roster_{new_season}.parquet'))

rbind(
  pbp_scrape$scratches_df,
  scratches_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/scratches/{new_season}/scratches_{new_season}.parquet'))

rbind(
    pbp_scrape$events_summary_df,
    events_summary_ds %>%
      filter(year == new_season & 
               !(game_id %in% new_game_ids)) %>%
      select(-year) %>%
      collect()
  ) %>% 
  write_parquet(glue('data/events_summary/{new_season}/events_summary_{new_season}.parquet'))

rbind(
  pbp_scrape$report,
  report_ds %>%
    filter(year == new_season & 
             !(game_id %in% new_game_ids)) %>%
    select(-year) %>%
    collect()
) %>% 
  write_parquet(glue('data/report/{new_season}/report_{new_season}.parquet'))

rm(pbp_scrape)


# Moneypuck scrape --------------------------------------------------------

# Grab existing moneypuck games
existing_moneypuck_ids <-
  gsub('.rds', '', dir(
    path = glue('data/moneypuck/{current_season}{current_season+1}/')
  ))

new_moneypuck_ids <- schedule_ds %>% 
  filter(session == 'R' & 
           season >= 20202021 & 
           game_id != '2010020124' &
           game_id != '2019030016' &
           !(game_id %in% existing_moneypuck_ids) & 
           game_status == 'Final' & 
           game_date <= Sys.Date()) %>% 
  collect() %>%
  pull(game_id)

new_moneypuck_ids

map_df(new_moneypuck_ids, fx.scrape_moneypuck)


# Natural Stat Trick scrape -----------------------------------------------

# Grab existing NST games
existing_nst_ids <-
  gsub('.rds', '', dir(
    path = glue('data/nst/{current_season}{current_season+1}/')
  ))

new_nst_ids <- schedule_ds %>% 
  filter(session == 'R' & 
           season >= 20202021 & 
           game_id != '2010020124' &
           game_id != '2019030016' &
           !(game_id %in% existing_nst_ids) & 
           game_status == 'Final' & 
           game_date <= Sys.Date()) %>% 
  collect() %>%
  pull(game_id)

new_nst_ids

map_df(new_nst_ids, fx.scrape_nst)


