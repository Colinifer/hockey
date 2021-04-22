

# standings <-
nhlapi::nhl_standings(seasons = 2010) %>%
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
    goal_differential = goals_for - goals_against,
    off_scoring_rk = dense_rank(desc(goals_for)),
    def_scoring_rk = dense_rank(goals_against),
    index = (off_scoring_rk + def_scoring_rk) / 2,
    NULL
  ) %>% select(
    -team_id,
    -goals_for,
    -goals_against,
    -goal_differential ,
    -def_scoring_rk,
    -off_scoring_rk) %>% 
  arrange(index)

