library(arrow)


# Create Parquet files ----------------------------------------------------

lapply(1:5, function(x){
  pbp_base <- readRDS(
    glue(
      'data/pbp_scrape{x}.rds'
    )
  )
  pbp_base_seasons <- pbp_base$game_info %>% 
    pull(season) %>% 
    unique()
  
  # pbp_base_names <- pbp_base %>% names()
  pbp_base_names <- c(
    'game_info',
    'pbp_base',
    'player_shifts',
    'player_periods',
    'roster',
    'scratches',
    'events_summary'
  )
  
  lapply(pbp_base_names, function(y.names){
    dir.create(glue('data/{y.names}'))
  })
  
  lapply(pbp_base_seasons, function(x.season){
    lapply(pbp_base_names, function(y.names){
      dir.create(
        glue('data/{y.names}/{x.season}')
        )
      pbp_base %>% 
        purrr::pluck(y.names) %>% 
        filter(season == x.season) %>% 
        write_parquet(
          glue('data/{y.names}/{x.season}/{y.names}_{x.season}.parquet')
          )
      })
    })
  
  # # pbp_base_names <- pbp_base %>% names()
  # pbp_base_names <- c(
  #   'report',
  #   'pbp_extras'
  # )
  # 
  # lapply(pbp_base_names, function(y.names){
  #   dir.create(glue('data/{y.names}'))
  # })
  # 
  # lapply(pbp_base_seasons, function(x.season){
  #   lapply(pbp_base_names, function(y.names){
  #     dir.create(
  #       glue('data/{y.names}/{x.season}')
  #     )
  #     pbp_base %>%
  #       purrr::pluck(y.names) %>%
  #       filter(grepl(x.season %>% substr(1,4), game_id)) %>%
  #       write_parquet(
  #         glue('data/{y.names}/{x.season}/{y.names}_{x.season}.parquet')
  #       )
  #   })
  # })
})

lapply(1:5, function(x){
  pbp_base <- readRDS(
    glue(
      'data/pbp_scrape{x}.rds'
    )
  )
  pbp_base_seasons <- pbp_base$game_info %>% 
    pull(season) %>% 
    unique()
  
  # pbp_base_names <- pbp_base %>% names()
  pbp_base_names <- c(
    'report',
    'pbp_extras'
  )
  
  lapply(pbp_base_names, function(y.names){
    dir.create(glue('data/{y.names}'))
  })
  
  lapply(pbp_base_seasons, function(x.season){
    lapply(pbp_base_names, function(y.names){
      dir.create(
        glue('data/{y.names}/{x.season}')
      )
      pbp_base %>% 
        purrr::pluck(y.names) %>% 
        filter(grepl(x.season %>% substr(1,4), game_id)) %>% 
        write_parquet(
          glue('data/{y.names}/{x.season}/{y.names}_{x.season}.parquet')
        )
    })
  })
})


# Retrieve data -----------------------------------------------------------

game_ids <- game_info_ds %>% 
  filter(year == '20202021') %>% 
  collect() %>% 
  pull(game_id)

available_game_ids <- game_info_ds %>% 
  filter() %>% 
  collect() %>% 
  pull(game_id) %>% 
  unique()

all_game_ids <- c(2010020001:2010021230, 
  2011020001:2011021230, 
  2012020001:2012021230,
  2013020001:2013021230,
  2014020001:2014021230,
  2015020001:2015021230,
  2016020001:2016021230,
  2017020001:2017021271,
  2018020001:2018021271,
  2019020001:2019021271,
  2010020002:2020020868)

'%notin%' <- Negate('%in%')

all_game_ids %notin% available_game_ids

pbp_df <- pbp_base_ds %>% 
  # filter(game_id %in% game_ids) %>%
  filter(year >= 20202021) %>%  
  collect() %>% 
  as_tibble() %>% 
  group_by(game_id) %>% 
  mutate(
    goal = if_else(event_type == 'GOAL', event_player_1, ''),
    a1 = if_else(event_type == 'GOAL', event_player_2, ''),
    a2 = if_else(event_type == 'GOAL', event_player_3, ''),
    pen_t =  if_else(event_type == 'PENL', event_player_1, ''),
    pen_d =  if_else(event_type == 'PENL', event_player_2, ''),
    shot = if_else(event_type == 'SHOT', event_player_1, ''),
    block = if_else(event_type == 'BLOCK', event_player_2, ''),
    corsi_event = ifelse(event_type %in% st.corsi_events, 1, 0),
    home_corsi = case_when(corsi_event == 1 &
                                 ((event_type %in% st.corsi_events &
                                     event_team == home_team)) == TRUE ~ 1,
                               corsi_event == 1 &
                                 ((event_type %in% st.corsi_events &
                                     event_team != home_team)) == TRUE ~ -1,
                               TRUE ~ 0),
    away_corsi = case_when(corsi_event == 1 &
                                 ((event_type %in% st.corsi_events &
                                     event_team == away_team)) == TRUE ~ 1,
                               corsi_event == 1 &
                                 ((event_type %in% st.corsi_events &
                                     event_team != away_team)) == TRUE ~ -1,
                               TRUE ~ 0),
    faceoff_w = if_else(
      event_type == 'FAC',
      if_else(event_team == away_team, event_player_1, event_player_2),
      ''),
    faceoff_l = if_else(
      event_type == 'FAC',
      if_else(event_team != away_team, event_player_1, event_player_2),
      '')
  )
  # select(home_team, away_team, event_type, event_team, goal, a1, a2, shot, faceoff_w, faceoff_l) 

faceoffs <- pbp_df %>%
  filter(faceoff_w != '') %>%
  arrange(faceoff_w) %>%
  group_by(game_id, faceoff_w) %>%
  mutate(faceoff_w_tot = n()) %>%
  filter(row_number() == n()) %>%
  select(home_team, away_team, event_team, faceoff_w, faceoff_w_tot)

# Create penalties drawn
pen_d <- pbp_df %>%
  filter(pen_d != '') %>%
  arrange(pen_d) %>%
  group_by(game_id, pen_d) %>%
  mutate(pen_d_tot = n()) %>%
  filter(row_number() == n()) %>%
  select(home_team, away_team, event_team, pen_d, pen_d_tot)

# Create assists 1 and 2
a1 <- pbp_df %>%
  filter(a1 != '') %>%
  arrange(a1) %>%
  group_by(game_id, a1) %>%
  mutate(a1_tot = n()) %>%
  filter(row_number() == n()) %>%
  select(home_team, away_team, event_team, a1, a1_tot)

a2 <- pbp_df %>%
  filter(a2 != '') %>%
  arrange(a2) %>%
  group_by(game_id, a2) %>%
  mutate(a2_tot = n()) %>%
  filter(row_number() == n()) %>%
  select(home_team, away_team, event_team, a2, a2_tot)

# Create corsi events (Shots + Blocks + Misses)
corsi <- pbp_df %>%
  filter(corsi_event == 1) %>%
  select(event_index,
         event_type,
         contains('home'),
         -home_goalie,
         -home_team,
         -home_skaters,
         -home_score,
         corsi = home_corsi,
         corsi_event,
         game_strength_state) %>% 
  pivot_longer(
    cols = contains('home_on')
  ) %>% 
  mutate(
    corsi_for = ifelse(corsi > 0, 1, NA),
    corsi_against = ifelse(corsi < 0, 1, NA),
    goal_for = ifelse(corsi > 0 & event_type == 'GOAL' & game_strength_state == '5v5', 1, NA),
    goal_against = ifelse(corsi < 0 & event_type == 'GOAL' & game_strength_state == '5v5', 1, NA),
    full_ev_corsi_for = ifelse(corsi > 0 & game_strength_state == '5v5', 1, NA),
    full_ev_corsi_against = ifelse(corsi < 0 & game_strength_state == '5v5', 1, NA),
    full_ev_goal_for = ifelse(corsi > 0 & event_type == 'GOAL' & game_strength_state == '5v5', 1, NA),
    full_ev_goal_against = ifelse(corsi < 0 & event_type == 'GOAL' & game_strength_state == '5v5', 1, NA)
  ) %>% 
  rbind(
    pbp_df %>%
      filter(corsi_event == 1) %>%
      select(event_index,
             event_type,
             contains('away'),
             -away_goalie,
             -away_team,
             -away_skaters,
             -away_score,
             corsi = away_corsi,
             corsi_event,
             game_strength_state) %>% 
      pivot_longer(
        cols = contains('away_on')
      ) %>% 
      mutate(
        corsi_for = ifelse(corsi > 0, 1, NA),
        corsi_against = ifelse(corsi < 0, 1, NA),
        goal_for = ifelse(corsi > 0 & event_type == 'GOAL', 1, NA),
        goal_against = ifelse(corsi < 0 & event_type == 'GOAL', 1, NA),
        full_ev_corsi_for = ifelse(corsi > 0 & game_strength_state == '5v5', 1, NA),
        full_ev_corsi_against = ifelse(corsi < 0 & game_strength_state == '5v5', 1, NA),
        full_ev_goal_for = ifelse(corsi > 0 & event_type == 'GOAL' & game_strength_state == '5v5', 1, NA),
        full_ev_goal_against = ifelse(corsi < 0 & event_type == 'GOAL' & game_strength_state == '5v5', 1, NA)
      )
  ) %>% 
  rename(
    player = value
  ) %>% 
  filter(!is.na(player)) %>% 
  select(-name) %>% 
  group_by(game_id, player) %>% 
  summarize(corsi_for = sum(corsi_for, na.rm = T),
            corsi_against = sum(corsi_against, na.rm = T),
            goals_for = sum(goal_for, na.rm = T),
            goals_against = sum(goal_against, na.rm = T),
            full_ev_corsi_for = sum(full_ev_corsi_for, na.rm = T),
            full_ev_corsi_against = sum(full_ev_corsi_against, na.rm = T),
            full_ev_goals_for = sum(full_ev_goal_for, na.rm = T),
            full_ev_goals_against = sum(full_ev_goal_against, na.rm = T))


events_summary_ds %>%
  # filter(game_id %in% game_ids) %>%
  filter(year >= 20202021) %>% 
  collect() %>%
  as_tibble() %>%
  # select(player, game_id, position) %>% 
  left_join(pen_d %>% 
              select(
                game_id, 
                pen_d_player = pen_d, 
                pen_d = pen_d_tot),
            by = c(
              'game_id', 
              'player' = 'pen_d_player')
            ) %>% 
  mutate(pen_d = ifelse(is.na(pen_d), 0, pen_d)) %>% 
  left_join(a1 %>% 
              select(
                game_id, 
                a1_player = a1, 
                a1 = a1_tot),
            by = c(
              'game_id', 
              'player' = 'a1_player')
  ) %>% 
  left_join(a2 %>% 
              select(
                game_id, 
                a2_player = a2, 
                a2 = a2_tot),
            by = c(
              'game_id', 
              'player' = 'a2_player')
  ) %>% 
  mutate(a1 = ifelse(is.na(a1), 0, a1),
         a2 = ifelse(is.na(a2), 0 ,a2)) %>% 
  left_join(corsi,
            by = c(
              'game_id', 
              'player' = 'player')
            ) %>% 
  filter(position_type != 'G') %>% 
  mutate(gs = (0.75 * g) + # goals
           (0.7 * a1) + # assists 1
           (0.55 * a2) + # assists 1
           (0.075 * s) + # shots
           (0.05 * bs) + # blocked shots
           (0.15 * (pen_d - pen)) + # penalty differential
           (0.01 * (fw - fl)) + # faceoff differential
           (0.05 * (full_ev_corsi_for - full_ev_corsi_against)) + # corsi differential
           (0.15 * (full_ev_goals_for - full_ev_goals_against)) # goals differential
         ) %>% 
  select(
    player, 
    g,
    a,
    gs, 
    everything()
    ) %>% 
  arrange(-gs) %>% 
  group_by(player) %>% 
  summarise(games = n(),
            g = sum(g, na.rm = T),
            a1 = sum(a1, na.rm = T),
            a2 = sum(a2, na.rm = T),
            pts = sum(g, a, na.rm = T),
            gs_tot = sum(gs, na.rm = T),
            team = first(team),
            gsva = (median(gs, na.rm = T) + mean(gs, na.rm = T)) / 2
            # gsva = mean(gs, na.rm = T)
            ) %>% 
  # filter(games > 20) %>% 
  arrange(-gsva)

# (0.75 * G) + (0.7 * A1) + (0.55 * A2) + (0.075 * SOG) + (0.05 * BLK) + (0.15 * PD) – (0.15 * PT) + (0.01 * FOW) – (0.01 * FOL) + (0.05 * CF) – (0.05 * CA) + (0.15 * GF) – (0.15* GA)

# Feather -----------------------------------------------------------------

pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')

