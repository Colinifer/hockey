
# standings <- 
  nhlapi::nhl_standings() %>% 
  # select(teamRecords) %>% 
  unnest(cols = c(teamRecords)) %>% 
  as_tibble() %>% 
  identity() %>% 
  select(
    division = division.name,
    team_name = team.name,
    team_id = team.id,
    geams_played = gamesPlayed,
    points,
    row,
    goals_for = goalsScored,
    goals_against = goalsAgainst,
    # everything(),
    NULL
  ) %>% 
  mutate(
    goal_differential = goals_for - goals_against
  )

  