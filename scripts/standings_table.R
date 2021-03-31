
standings <- nhlapi::nhl_standings() %>% 
  select(teamRecords) %>% 
  unnest() %>% 
  as_tibble()
