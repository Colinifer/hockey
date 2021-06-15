skater_game_score <- readRDS('https://github.com/Colinifer/hockey/blob/master/gt_help_dataset.rds?raw=true')

skater_game_score %>% 
  dplyr::slice(1:25) %>% 
  select(
    full_name,
    headshot_url,
    logo,
    games,
    g,
    a1,
    a2,
    pts,
    gs_tot
  ) %>%
  arrange(-gs_tot) %>% 
  # dplyr::slice(1:50) %>% 
  mutate(Rank = paste0('#',row_number())) %>% 
  gt() %>% 
  tab_header(title = glue('Total Game Score {current_season}')) %>% 
  cols_move_to_start(columns = vars(Rank)) %>% 
  cols_label(
    full_name = 'Name',
    headshot_url = '',
    logo = '',
    games = 'GP',
    g = 'Goals',
    a1 = 'A1',
    a2 = 'A2',
    pts = 'Points',
    gs_tot = 'Total Gamescore'
  ) %>% 
  tab_style(style = cell_text(font = "Chivo", size = 'x-large', weight = 'bold'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
  tab_style(style = cell_text(align = 'left'), locations = cells_body(vars(full_name))) %>% 
  tab_style(
    style = cell_borders(
      sides = "left",
      color = color_cw[5],
      weight = px(3)
    ),
    locations = list(
      cells_body(
        columns = c(5,6,10)
      )
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Chivo", weight = "bold"),
    locations = cells_body(
      columns = vars(Rank, full_name)
    )
  ) %>% 
  tab_style(
    style = cell_text(font = "Montserrat"),
    locations = cells_body(
      columns = c(5:10)
    )
  ) %>% 
  tab_spanner(label = 'Assists', columns = vars(a1, a2)) %>% 
  tab_source_note(source_note = 'Chart: Colin Welsh | Data: NHL') %>% 
  data_color(
    columns = vars(gs_tot),
    colors = scales::col_numeric(palette = c(color_cw[8], color_cw[2], color_cw[6]), domain = c(max(skater_game_score$gs_tot), 0, min(skater_game_score$gs_tot))),
    autocolor_text = FALSE
  ) %>% 
  text_transform(
    locations = cells_body(vars(headshot_url)),
    fn = function(x) web_image(url = x)
    # image_read(x) %>% 
    # image_background(none)
  ) %>% 
  text_transform(
    locations = cells_body(vars(logo)),
    fn = function(x) web_image(url = x)
  ) %>% 
  cols_width(vars(logo) ~ px(45)) %>% 
  gt::tab_options(
    table.font.color = color_cw[5],
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = color_cw[5],
    column_labels.border.bottom.width = 1.4,
    column_labels.font.weight = "bold",
    table_body.border.top.color = color_cw[5],
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = color_cw[5],
    table.border.top.color = 'transparent',
    table.background.color = color_cw[1],
    table.border.bottom.color = 'transparent',
    row.striping.background_color = color_cw[2],
    row.striping.include_table_body = TRUE
  ) %>% 
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>% 
  gtsave(filename = glue('gt_help_table.png'))