roster_df %>% 
  mutate(full_name = str_to_title(full_name),
         position = case_when(position == 'R' ~ 'RW',
                              position == 'L' ~ 'LW',
                              TRUE ~ position)) %>% 
  select(full_name, team, position) %>% write_csv('../../../Downloads/roster.csv')

rosters <- get_rosters()

rosters %>% 
  mutate(is_rookie = case_when(experience == 0 ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  select(player, team_abbr, position, is_rookie) %>% 
  write_csv('../../../Downloads/roster.csv')
