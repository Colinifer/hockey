# source(url('https://github.com/mtthwastn/statswithmatt/raw/master/hockey-with-r/hockey-rink.R'))
# source(url('https://github.com/mtthwastn/statswithmatt/raw/master/hockey-with-r/gg-rink.R'))

iso_team <- NULL


con <- fx.db_con()
x.game_id <- tbl(con, 'schedule') %>% 
  filter(
    game_status == 'Final' & 
      season == '20202021'
      # (home_team == iso_team |
      #    away_team == iso_team)
    ) %>% 
  collect() %>% 
  mutate(game_id = game_id %>% as.integer()) %>% 
  arrange(-game_id) %>% 
  # filter(row_number() == n()) %>%
  # head(5) %>% 
  pull(game_id)
dbDisconnect(con)

nhl_rink <- geom_hockey(
  'nhl', 
  background_color = color_cw[2],
  boards_color = color_cw[5],
  full_surf = F, 
  rotate = T, 
  rotation_dir = 'ccw'
  )

x.game_id

con <- fx.db_con()
pbp <- tbl(con, 'pbp_base') %>%
  filter(season == current_full_season &
           game_id %in% x.game_id &
           event_type %in% c('SHOT', 'MISS', 'GOAL')
         # & 
         #   event_team == iso_team
         ) %>%
  select(home_team_abbr = home_team,
         away_team_abbr = away_team,
         event_team_abbr = event_team,
         event_type,
         event_player_1,
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
    # coords_x = case_when(game_period %% 2 == 0 ~ -1 * coords_x,
    #                      TRUE ~ coords_x),
    # coords_y = case_when(game_period %% 2 == 0 ~ -1 * coords_y,
    #                      TRUE ~ coords_y)
    coords_x = ifelse(coords_x > 0, -1 * coords_x, coords_x),
    coords_y = ifelse(coords_x > 0, -1 * coords_y, coords_y)
    # coords_x = ifelse(event_team_abbr == iso_team & coords_x > 0, -1 * coords_x, coords_x),
    # coords_y = ifelse(event_team_abbr == iso_team & coords_x > 0, -1 * coords_y, coords_y),
    # coords_x = ifelse(event_team_abbr != iso_team & coords_x < 0, -1 * coords_x, coords_x),
    # coords_y = ifelse(event_team_abbr != iso_team & coords_x < 0, -1 * coords_y, coords_y)
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
dbDisconnect(con)

# Viridis hex plot
nhl_rink +
  geom_hex(data =  pbp,
           aes(
             coords_y,
             coords_x
           ),
           binwidth = c(2.5, 2.5),
           alpha = .5
  ) +
  scale_fill_viridis(option = "A") + 
  labs(title = glue("Shot Chart: {current_season} NHL season"),
       subtitle = "NHL rink",
       x = NULL,
       y = NULL) + 
  # theme_cw +
  # scale_x_continuous(breaks = seq(-30, 30, by = 5)) +
  # scale_y_continuous(breaks = seq(-15, 15, by = 3)) +
  NULL

# transparent plot
nhl_rink +
  geom_hex(data =  pbp,
           aes(
             coords_y,
             coords_x,
           ),
           alpha = .3,
           binwidth = c(2.5, 2.5)
  ) +
  labs(title = glue("Shot Chart: {current_season} NHL season"),
       subtitle = "NHL rink",
       x = NULL,
       y = NULL) +
  # scale_x_continuous(breaks = seq(-30, 30, by = 5)) +
  # scale_y_continuous(breaks = seq(-15, 15, by = 3)) +
  NULL

nhl_rink +
  geom_hex(data =  pbp %>%
             filter(event_team_abbr == iso_team),
           aes(
             coords_y,
             coords_x
           ),
           binwidth = c(5, 5),
           alpha = .75
  ) +
  # geom_hex(data =  pbp %>%
  #            filter(event_team_abbr == .$away_team),
  #          aes(
  #            coords_y, 
  #            coords_x
  #          ), 
  #          binwidth = c(5, 5), 
  #          alpha = .75
  # ) + 
  geom_text_repel(
      data = pbp %>%
        filter(event_type == 'GOAL'),
      aes(x = coords_y , y = coords_x, label = paste(event_player_1)),
      color = color_cw[5],
      size = 5,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
      ) +
  labs(title = glue("shot chart: {current_season} NHL playoffs"),
       subtitle = "NHL rink",
       x = NULL,
       y = NULL) +
  # scale_x_continuous(breaks = seq(-30, 30, by = 5)) +
  # scale_y_continuous(breaks = seq(-15, 15, by = 3)) +
  NULL

