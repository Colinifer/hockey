roster_df %>% 
  filter(!is.na(player_id)) %>% 
  pull(player_id)

map_df(
  .x = roster_df %>% 
    filter(!is.na(player_id)) %>% 
    # select(num_last_first, player_id)
    pull(player_id) %>% 
    as.character() %>% 
    head(),
  ~{nhlapi::nhl_players(playerIds = c(.x))}
)
