source(url('https://github.com/mtthwastn/statswithmatt/raw/master/hockey-with-r/hockey-rink.R'))
source(url('https://github.com/mtthwastn/statswithmatt/raw/master/hockey-with-r/gg-rink.R'))

x.game_id <- schedule_ds %>% 
  filter(game_status == 'Final' &
           (home_team == 'CAR' |
              away_team == 'CAR')) %>% 
  collect() %>% 
  filter(row_number() == n()) %>% 
  pull(game_id)

nhl_rink <- geom_hockey('nhl')

x.game_id

pbp <- pbp_base_ds %>%
  filter(season == current_full_season &
           game_id %in% x.game_id &
           event_type %in% c('SHOT', 'MISS', 'GOAL')) %>%
  select(home_team_abbr = home_team,
         away_team_abbr = away_team,
         event_team_abbr = event_team,
         game_period,
         coords_x,
         coords_y) %>%
  collect() %>%
  mutate(
    home_team_abbr = case_when(home_team_abbr == 'T.B' ~ 'TBL',
                               TRUE ~ home_team_abbr),
    away_team_abbr = case_when(away_team_abbr == 'T.B' ~ 'TBL',
                               TRUE ~ away_team_abbr),
    event_team_abbr = case_when(event_team_abbr == 'T.B' ~ 'TBL',
                                TRUE ~ event_team_abbr),
    coords_x = case_when(game_period %% 2 == 0 ~ -1 * coords_x,
                         TRUE ~ coords_x),
    coords_y = case_when(game_period %% 2 == 0 ~ -1 * coords_y,
                         TRUE ~ coords_y)
  ) %>% 
  left_join(
    nhlapi::nhl_teams() %>% 
      select(
        id,
        abbreviation,
        teamName,
        shortName
      ) %>% 
      as_tibble(),
    by = c('event_team_abbr' = 'abbreviation')
  ) %>% 
  left_join(teamcolors::teamcolors %>% 
              filter(league == 'nhl') %>% 
              select(
                mascot,
                primary,
                secondary,
                NULL
              ),
            by = c('teamName' = 'mascot'))

# home_shots <- pbp %>%
#   filter(event_team == .$home_team) %>% 
#   as_tibble()
# 
# away_shots <- pbp %>%
#   filter(event_team == .$away_team) %>% 
#   as_tibble()

nhl_rink +
  # ggplot(aes(x = coords_x, y = coords_y)) +
  # gg_rink(side = "right", specs = "nhl") +
  # gg_rink(side = "left", specs = "nhl") +
  # geom_point(aes(x = coords_x, y = coords_y)) + 
  geom_point(data = pbp %>%
               filter(event_team_abbr == .$home_team), 
             aes(
               coords_x, 
               coords_y
               ),
             shape = 16,
             color = pbp %>%
               filter(event_team_abbr == .$home_team) %>% 
               pull(primary),
             size = 3
             ) + 
  geom_point(data =  pbp %>%
               filter(event_team_abbr == .$away_team), 
             aes(
               coords_x, 
               coords_y
               ),
             shape = 16,
             color = pbp %>%
               filter(event_team_abbr == .$away_team) %>% 
               pull(primary),
             size = 3
             ) + 
  labs(title = glue("shot chart: {current_season} NHL playoffs"),
       subtitle = "NHL rink",
       x = NULL,
       y = NULL) +
  # scale_x_continuous(breaks = seq(-30, 30, by = 5)) +
  # scale_y_continuous(breaks = seq(-15, 15, by = 3)) +
  NULL

