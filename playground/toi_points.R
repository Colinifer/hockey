todays_theme <- function () { 
  theme_linedraw(base_size=11, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA), 
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.3), 
      panel.grid.minor = element_blank()
    )
}

# get pbp data, roster info, and team info from hockeyR

data <- load_pbp(2022)
roster <- get_rosters(team = "all", season = 2022)
team_info <- team_logos_colors

position <- roster %>% 
  select(player, position) %>% 
  unique() %>% 
  mutate(position = case_when(position == "F" ~ "forward", 
                              TRUE ~ "defenseman"))

# calculate the total points for each player

points <- data %>% 
  # want all goals except for those in the shootout
  filter(event_type == "GOAL" & period != 5) %>% 
  # event players 1-3 are those who might get points on each goal
  select(game_id, game_date, team = event_team_abbr, event_player_1_name, 
         event_player_2_name, event_player_3_name, event_player_1_type, 
         event_player_2_type, event_player_3_type) %>% 
  # this will create a name/type combo for players 1 through 3
  pivot_longer(event_player_1_name:event_player_3_type,
               names_to = c(NA, ".value"),
               names_pattern = "(.+)_(.+)") %>% 
  # only want the players who either scored the goal or got an assist
  filter(type %in% c("Assist", "Scorer")) %>% 
  count(name, team, name = "points") %>% 
  mutate(name = str_replace_all(name, "\\.", " "))

# calculate TOI for each player

TOI <- data %>% 
  arrange(game_id, event_idx) |> 
  # create a variable for the length of each event
  mutate(length = case_when(lead(game_id) == game_id ~ 
                              lead(period_seconds) - period_seconds,
                            TRUE ~ 0)) %>% 
  select(length, game_id, home_abbreviation, away_abbreviation, 
         home_on_1:away_on_7) %>% 
  filter(length > 0) %>% 
  pivot_longer(home_on_1:away_on_7,
               names_to = "team",
               values_to = "name") %>% 
  filter(!is.na(name)) %>% 
  mutate(team = ifelse(str_detect(team, "home"), home_abbreviation, 
                       away_abbreviation)) %>% 
  select(-c(3:4)) %>% 
  group_by(name, team) %>% 
  # calculate total TOI and games played
  summarize(TOI = sum(length) / 60,
            GP = n_distinct(game_id)) %>% 
  mutate(name = str_replace_all(name, "\\.", " ")) %>% 
  filter(TOI > 200 & !(name %in% c("Marc Andre Fleury",
                                   "Ukko Pekka Luukkonen"))) %>% 
  # need to make some name adjustments to account for discrepancies in the data
  mutate(name = case_when(name == "Drew O Connor" ~ "Drew O'Connor",
                          name == "Logan O Connor" ~ "Logan O'Connor",
                          name == "Liam O Brien" ~ "Liam O'Brien",
                          name == "Ryan O Reilly" ~ "Ryan O'Reilly",
                          name == "Jean Gabriel Pageau" ~ "Jean-Gabriel Pageau",
                          name == "K Andre Miller" ~ "K'Andre Miller",
                          name == "Marc Edouard Vlasic" ~ "Marc-Edouard Vlasic",
                          name == "Pierre Edouard Bellemare" ~ "Pierre-Edouard Bellemare",
                          name == "Nicolas Aube Kubel" ~ "Nicolas Aube-Kubel",
                          name == "Oliver Ekman Larsson" ~ "Oliver Ekman-Larsson",
                          name == "Pierre Luc Dubois" ~ "Pierre-Luc Dubois",
                          name == "Ryan Nugent Hopkins" ~ "Ryan Nugent-Hopkins",
                          name == "Zach Aston Reese" ~ "Zach Aston-Reese",
                          TRUE ~ name)) %>% 
  # join in points data to calculate rate
  left_join(points, by = c("name", "team")) %>% 
  mutate(points = replace_na(points, 0),
         pts_per_60 = points * 60 / TOI)

top_points <- TOI %>% 
  left_join(position, by = c("name" = "player")) %>% 
  # filter to only the top 10 players per team
  group_by(team) %>% 
  top_n(10, pts_per_60) %>% 
  left_join(select(team_info, full_team_name, team = team_abbr),
            by = "team")
