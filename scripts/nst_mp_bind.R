nst_data <- do.call(bind_rows,
        lapply(list.files(
          path = glue('data/nst_games/{new_season}'),
          pattern = '.rds',
          full.names = T
        ),
        readRDS)) 

nst_data %>%
  write_parquet(glue('data/nst/{new_season}/nst_{new_season}.parquet'))

mp_data <- do.call(bind_rows,
                    lapply(list.files(
                      path = glue('data/moneypuck_games/{new_season}'),
                      pattern = '.rds',
                      full.names = T
                    ),
                    readRDS)) %>% 
  mutate(season = new_season) %>% 
  select(
    season,
    everything()
  )

mp_data %>%
  write_parquet(glue('data/moneypuck/{new_season}/mp_{new_season}.parquet'))
