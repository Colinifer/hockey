current_season <- 2021
map(2020:2021, function(x){
# standings <-
standings <- nhlapi::nhl_standings(seasons = x) %>%
  # select(teamRecords) %>%
  unnest(cols = c(teamRecords)) %>%
  as_tibble() %>%
  identity() %>%
  select(
    team_name = team.name,
    division = division.name,
    team_id = team.id,
    games_played = gamesPlayed,
    points,
    row,
    goals_for = goalsScored,
    goals_against = goalsAgainst,
    # everything(),
    NULL
  ) %>% 
  mutate(
    team_name = case_when(team_id == 8 ~ 'Montreal Canadiens',
                          TRUE ~ team_name)
  ) %>% 
  left_join(teamcolors::teamcolors %>% 
              filter(league == 'nhl') %>% 
              select(
                sportslogos_name,
                primary,
                secondary,
                logo,
                NULL
              ),
            by = c('team_name' = 'sportslogos_name')) %>% 
  mutate(
    goal_differential = goals_for - goals_against,
    off_scoring_rk = dense_rank(desc(goals_for)),
    def_scoring_rk = dense_rank(goals_against),
    index = (off_scoring_rk + def_scoring_rk) / 2,
    NULL
  ) %>% select(
    -team_id,
    -goals_for,
    -goals_against,
    -goal_differential,
    # -def_scoring_rk,
    # -off_scoring_rk,
    -primary,
    -secondary,
    NULL) %>% 
  arrange(index, desc(row))

standings %>% 
  gt() %>% 
  tab_header(title = glue('{x} NHL Standings')) %>% 
  cols_move_to_start(columns = c(index, logo, team_name)) %>% 
  cols_label(
    division = 'Division',
    logo = 'Team',
    team_name = '',
    games_played = 'GP',
    points = 'Points',
    row = 'ROW',
    off_scoring_rk = 'Offense',
    def_scoring_rk = 'Defense',
    index = 'Index Rank'
  ) %>% 
  tab_style(style = cell_text(font = "Chivo", size = 'x-large', weight = 'bold'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(c(division, team_name))) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(4,6)
      )
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Chivo", weight = "bold"),
    locations = cells_body(
      columns = c(index, team_name)
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Montserrat"),
    locations = cells_body(
      columns = c(3:8)
    )
  ) %>% 
  tab_spanner(label = 'Scoring Rank', columns = c(off_scoring_rk, def_scoring_rk)) %>% 
  tab_source_note(source_note = 'Chart: Colin Welsh | Data: @NHL') %>% 
  data_color(
    columns = c(index),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(min(standings$index), max(standings$index))),
    autocolor_text = FALSE
  ) %>% 
  data_color(
    columns = c(off_scoring_rk),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(min(standings$off_scoring_rk), max(standings$off_scoring_rk))),
    autocolor_text = FALSE
  ) %>% 
  data_color(
    columns = c(def_scoring_rk),
    colors = scales::col_quantile(palette = c(color_cw[6], color_cw[2], color_cw[8]), domain = c(min(standings$def_scoring_rk), max(standings$def_scoring_rk))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(c(logo)),
    fn = function(x) web_image(url = x)
  ) %>%
  # text_transform(
  #   locations = cells_body(vars(defteam)),
  #   fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
  # ) %>% 
  # cols_width(vars(defteam) ~ px(45)) %>% 
  # tab_options(
  #   table.font.color = color_cw[5],
  #   data_row.padding = '2px',
  #   row_group.padding = '3px',
  #   column_labels.border.bottom.color = color_cw[5],
  #   column_labels.border.bottom.width = 1.4,
  #   column_labels.font.weight = "bold",
  #   table_body.border.top.color = color_cw[5],
  #   row_group.border.top.width = 1.5,
  #   row_group.border.top.color = '#999999',
  #   table_body.border.bottom.width = 0.7,
  #   table_body.border.bottom.color = '#999999',
  #   row_group.border.bottom.width = 1,
  #   row_group.border.bottom.color = color_cw[5],
  #   table.border.top.color = 'transparent',
  #   table.background.color = color_cw[1],
  #   table.border.bottom.color = 'transparent',
  #   row.striping.background_color = color_cw[2],
  #   row.striping.include_table_body = TRUE
  # ) %>% 
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>% 
  gtsave(filename = glue("standings_{x}.png"), path = "plots/desktop")
})

