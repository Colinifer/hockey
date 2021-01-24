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
