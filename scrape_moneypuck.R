

fx.scrape_moneypuck <- function(gameid) {
  print(gameid)
  year <- substr(gameid,1,4) %>% as.integer()
  season <- glue('{year}{year+1}')
  base <- 'http://moneypuck.com/moneypuck/gameData'
  
  read_csv(url(glue('{base}/{season}/{gameid}.csv'))) %>% 
    saveRDS(glue('data/moneypuck/{season}/{gameid}.rds'))
  
  runif(1, 
        min=6, 
        max=8
        ) %>% 
    Sys.sleep()
}

csv_to_rds <- function(x){
  x %>% 
    read_csv() %>% 
    saveRDS(glue('{gsub(".csv", "", x)}.rds'))
}


# moneypuck_pbp_df <- map_df(dir(
#   path = glue('data/moneypuck/{current_season}{current_season+1}/'),
#   full.names = T
#   ),
#   read_csv)
# 
# moneypuck_pbp_df <- map_df(dir(
#   path = glue('data/moneypuck/{current_season}{current_season+1}/'),
#   full.names = T
# ),
# csv_to_rds)
