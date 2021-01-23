current_season <- 20202021

results_df <- schedule_ds %>% 
  filter(season == current_season & 
           game_date == '2021-01-13') %>% 
  collect() %>% 
  mutate(
    moneypuck_home_prob = NA,
    dom_home_prob = NA,
    home_moneyline = NA,
    away_moneyline = NA
  )

results_df %>% 
  saveRDS('results.rds')