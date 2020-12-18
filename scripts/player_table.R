library(arrow)

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

game_info <- open_dataset('data/game_info/', partitioning = 'year')
pbp <- open_dataset('data/pbp_base/', partitioning = 'year')
pbp_extras <- open_dataset('data/pbp_extras', partitioning = 'year')
player_shifts <- open_dataset('data/player_shifts', partitioning = 'year')
player_periods <- open_dataset('data/player_periods', partitioning = 'year')
roster <- open_dataset('data/roster/', partitioning = 'year')
scratches <- open_dataset('data/scratches/', partitioning = 'year')
events_summary <- open_dataset('data/events_summary/', partitioning = 'year')
report <- open_dataset('data/report/', partitioning = 'year')

game_ids <- game_info %>% 
  filter(season == '20192020' & (home_team == 'CHI' | away_team == 'CHI')) %>% 
  collect() %>% 
  pull(game_id)

available_game_ids <- game_info %>% 
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
  2019020001:2019021271)

'%notin%' <- Negate('%in%')

all_game_ids %notin% available_game_ids

pbp %>% 
  filter(game_id == game_ids[1]) %>% 
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
    faceoff_w = if_else(
      event_type == 'FAC',
      if_else(event_team == away_team, event_player_1, event_player_2),
      ''),
    faceoff_l = if_else(
      event_type == 'FAC',
      if_else(event_team != away_team, event_player_1, event_player_2),
      '')
  ) %>% 
  select(event_type, goal, a1, a2, shot, faceoff_w, faceoff_l)

events_summary %>% 
  filter(game_id %in% game_ids) %>% 
  collect() %>% 
  as_tibble() %>% 
  mutate(gs = (0.75 * g) + # goals
           (0.7 * a) + # assists
           (0.075 * s) + # shots
           (0.05 * bs) + # blocked shots
           (0.15 * PD) - # penalties drawn
           (0.15 * pen) + # penalties taken
           (0.01 * fw) - # faceoffs won
           (.01 * fl) + # faceoffs lost
           (0.05 * CF) - # corsi for
           (0.05 * CA) + # corsi against
           (0.15 * GF) - # goals for
           (0.15 * GA) # goals against
         )

# (0.75 * G) + (0.7 * A1) + (0.55 * A2) + (0.075 * SOG) + (0.05 * BLK) + (0.15 * PD) – (0.15 * PT) + (0.01 * FOW) – (0.01 * FOL) + (0.05 * CF) – (0.05 * CA) + (0.15 * GF) – (0.15* GA)

# Feather -----------------------------------------------------------------

pbp_ds <- open_dataset('data/pbp/fastr', partitioning = 'year')

