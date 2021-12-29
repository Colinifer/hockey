nst_data <- map_df(dir('data/nst/20202021/', full = T), function(x){
  nst_df <- readRDS(x)
  return(nst_df)
}) %>% 
  filter(
    # game_id == 2020020308 & 
    #   grepl('PHIstall', table)
    game_id == 2020020343 & 
      grepl('CHIstall', table)
    ) %>% 
  identity()

nst_data %>% 
  arrange(-toi)
