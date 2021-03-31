
standings <- nhlapi::nhl_standings() %>% 
  # select(teamRecords) %>% 
  unnest(cols = c(teamRecords)) %>% 
  as_tibble()
