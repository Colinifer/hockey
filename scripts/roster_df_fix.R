roster_dict <- 

active_players <- nhlapi::nhl_teams_rosters(seasons = 2021) %>% 
  unnest(roster.roster) %>% 
  as_tibble() %>% 
  janitor::clean_names()

# Load roster db and join with latest roster scrape
roster_df <- tbl(con, 'roster') %>% 
  filter(season == current_full_season) %>% 
  select(-game_id,
         -game_date,
         -opponent,
         -is_home) %>% 
  collect() %>% 
  unique() %>% 
  as_tibble() %>% 
  rename(
    # player,
    first_name = firstname,
    last_name = lastname,
    jersey_number = player_num
  ) %>% 
  mutate(
    full_name = glue('{first_name} {last_name}')
  ) %>% 
  left_join(
    active_players %>% 
      select(
        team_name,
        full_name = person_full_name,
        jersey_number,
        player_id = person_id,
        NULL
      ) %>% 
      mutate(
        full_name = full_name %>% toupper(),
        jersey_number = jersey_number %>% as.double()
      ),
    by = c('full_name',
           'jersey_number')
  ) %>% 
  left_join(
    teamcolors %>% 
      select(
        league,
        team_name = mascot,
        logo
      ) %>% 
      filter(league == 'nhl') %>% 
      select(
        -league
      ),
    by = c('team_name')
  ) %>% 
  unique() %>% 
  mutate(
    headshot_url = glue('https://cms.nhl.bamgrid.com/images/headshots/current/168x168/{player_id}.jpg'),
    action_shot_url = glue('https://cms.nhl.bamgrid.com/images/actionshots/{player_id}.jpg')
  )
