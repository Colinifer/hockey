current_season <- 2020

map_df(current_season, function(x){
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
    as_tibble()
}) %>% 
  mutate(
    moneypuck_home_prob = NA,
    moneypuck_away_prob = NA,
    dom_home_prob = NA,
    dom_away_prob = NA,
    home_moneyline = NA,
    away_moneyline = NA
  ) %>% 
  select(
    game_id,
    game_date,
    season,
    session,
    away_team,
    # moneypuck_away_prob,
    dom_away_prob,
    away_moneyline,
    home_team,
    # moneypuck_home_prob,
    dom_home_prob,
    home_moneyline,
    game_datetime,
    EST_time_convert,
    EST_date
  ) %>% 
  write_csv(glue('data/betting/{current_season}{current_season+1}/dom_schedule_{current_season}{current_season+1}.csv'))
