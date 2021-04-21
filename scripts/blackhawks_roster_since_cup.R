cup_roster <- roster_ds %>% 
  filter(season == 20142015 &
           team == 'CHI') %>% 
  select(player) %>% 
  collect() %>% 
  count(player, name = 'games_played') %>% 
  filter(games_played >= 10) %>% 
  arrange(-games_played) %>% 
  as_tibble() %>% 
  pull(player)

roster_ds %>% 
  filter(season == 20142015 &
           team == 'CHI') %>% 
  select(player) %>% 
  collect() %>% 
  count(player, name = 'games_played') %>% 
  filter(games_played <= 10) %>% 
  arrange(-games_played) %>% 
  as_tibble()

roster_ds %>%
  filter(season  >= 20152016 &
           team == 'CHI') %>%
  select(player, game_id) %>%
  collect() %>%  
  count(player, name = 'games_played') %>% 
  arrange(-games_played) %>% 
  filter(!player %in% cup_roster) %>% 
  as_tibble() %>% 
  write_csv('../../../Desktop/hawks_roster_since_1516.csv')
