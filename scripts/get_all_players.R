
players_df <- roster_df %>% 
  left_join(
    roster_df %>% 
      filter(!is.na(player_id)) %>% 
      pull(player_id) %>% 
      nhlapi::nhl_players(playerIds = .) %>% 
      as_tibble() %>%
      janitor::clean_names() %>% 
      select(
        -copyright,
        -link,
        -current_team_link,
        -url,
        -full_name,
        -first_name,
        -last_name,
        -primary_position_code,
        player_id = id,
        NULL
      ),
    by = c('player_id')
  ) %>% 
  unique()