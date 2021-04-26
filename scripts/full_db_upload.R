data_base_column_types <- 
  data.frame(
    variable = c(
      'game_id',
      'game_date',
      'season',
      'session',
      'game_status',
      'away_team',
      'home_team',
      'game_venue',
      'game_datetime',
      'EST_time_convert',
      'EST_date',
      'year'
    ),
    class = c(
      'numeric',
      'character',
      'numeric',
      'character',
      'character',
      'character',
      'character',
      'character',
      'character',
      'character',
      'character',
      'numeric'
      )
  )

character_columns <- data_base_column_types %>%
  dplyr::filter(class == "character") %>%
  dplyr::pull(variable)

numeric_columns <- data_base_column_types %>%
  dplyr::filter(class == "numeric") %>%
  dplyr::pull(variable)

integer_columns <- data_base_column_types %>%
  dplyr::filter(class == "integer") %>%
  dplyr::pull(variable)

df <- df %>%
  dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(df) %in% integer_columns, as.integer)

open_dataset('data/schedule/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'schedule', ., append = TRUE, row.names = FALSE)

open_dataset('data/game_info/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  as_tibble() %>% 
  DBI::dbWriteTable(con, 'game_info', ., append = TRUE, row.names = FALSE)

open_dataset('data/pbp_base/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'pbp_base', ., append = TRUE, row.names = FALSE)

open_dataset('data/pbp_extras', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'pbp_extras', ., append = TRUE, row.names = FALSE)

open_dataset('data/player_shifts', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'player_shifts', ., append = TRUE, row.names = FALSE)

open_dataset('data/player_periods', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'player_periods', ., append = TRUE, row.names = FALSE)

open_dataset('data/roster/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'roster', ., append = TRUE, row.names = FALSE)

open_dataset('data/scratches/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'scratches', ., append = TRUE, row.names = FALSE)

open_dataset('data/events_summary/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'events_summary', ., append = TRUE, row.names = FALSE)

open_dataset('data/report/', partitioning = 'year') %>%
  collect() %>%
  dplyr::mutate_if(names(.) %in% character_columns, as.character) %>%
  dplyr::mutate_if(names(.) %in% numeric_columns, as.numeric) %>%
  dplyr::mutate_if(names(.) %in% integer_columns, as.integer) %>% 
  DBI::dbWriteTable(con, 'report', ., append = TRUE, row.names = FALSE)

open_dataset('data/moneypuck/', partitioning = 'year') %>%
  collect() %>%
  DBI::dbWriteTable(con, 'moneypuck', ., append = TRUE, row.names = FALSE)

open_dataset('data/nst/', partitioning = 'year') %>%
  collect() %>%
  DBI::dbWriteTable(con, 'nst', ., append = TRUE, row.names = FALSE)
