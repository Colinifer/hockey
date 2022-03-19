library(readr)
library(dplyr)
library(lubridate)
library(elo)
library(MLmetrics)


con <- fx.db_con(x.host = 'localhost')
raw_data <- tbl(con, 'game_info') %>%
  filter(season %in% c(20202021, 20212022)) %>%
  collect()
dbDisconnect(con)

colnames(raw_data)

raw_data <- raw_data %>% 
  mutate(home_result = case_when(home_score > away_score ~ 1,
                                 home_score < away_score ~ 0,
                                 home_score == away_score ~ 0.5),
         away_result = case_when(home_score < away_score ~ 1,
                                 home_score > away_score ~ 0,
                                 home_score == away_score ~ 0.5),
         margin = abs(home_score - away_score)) |> 
  arrange(game_date)

elo_model <- elo::elo.run(data = raw_data,
                          formula = home_result ~ home_team + away_team + k(6 + 6*margin))

elo_model <- elo::elo.run(data = raw_data,
                          formula = 6 * ((.6686 * log(margin) + 0.8048) * (2.05 / ((home_team - away_team) * .001 + 2.05))) * (home_result - home_results))

elo_results <- elo_model %>%
  as.data.frame()

elo_results %>% tail(n = 10)

final_elos <- final.elos(elo_model)
final_elos %>% sort(decreasing = TRUE) %>% head(n = 20)

draw_rates <- data.frame(win_prob = elo_model$elos[,3],
                         win_loss_draw = elo_model$elos[,4]) %>%
  mutate(prob_bucket = abs(round((win_prob)*20)) / 20) %>%   # Round the predicted win probabilities to the nearest 0.05
  group_by(prob_bucket) %>%
  summarise(draw_prob = sum(ifelse(win_loss_draw == 0.5, 1, 0)) / n())   # Calculate the rate their was a draw for this win prob - elo package codes a draw as a 0.5

draw_rates %>% head(n=20)

data_with_probabilities <- raw_data %>% 
  select(game_date, season, game_id, home_team, away_team, home_result, away_result) %>%   # Remove some redundant columns
  mutate(home_elo = elo_results$elo.A - elo_results$update.A,    # Add in home team's elo rating (need to subtract the points update to obtain pre-match rating)
         away_elo = elo_results$elo.B - elo_results$update.B,    # Add in away team's elo rating (need to subtract the points update to obtain pre-match rating)
         home_prob = elo_results$p.A,                            # Add in home team's win/loss probability
         away_prob = 1 - home_prob) %>%                          # Add in away team's win/loss probability
  mutate(prob_bucket = round(20*home_prob)/20) %>%               # Bucket the home team's win/loss probability into a rounded increment of 0.05
  left_join(draw_rates, by = "prob_bucket") %>%                  # Join in our historic draw rates using the probability buckets
  relocate(draw_prob, .after = home_prob) %>% 
  select(-prob_bucket)

data_with_probabilities <- data_with_probabilities |> 
  mutate(home_prob = home_prob - home_prob * draw_prob,          # Redistribute home team's probabilities proportionally to create win/draw/loss probabilities
         away_prob = away_prob - away_prob * draw_prob)          # Redistribute away team's probabilities proportionally to create win/draw/loss probabilities

data_with_probabilities |> 
  select(game_id, season, home_team, away_team, home_prob, draw_prob, away_prob) |> 
  tail(n=10)

matches <- data_with_probabilities |>  
  filter(season == current_full_season) |>                      # Filter down to only 2021 matches
  mutate(home_win = ifelse(home_result == 1, 1, 0),       # Include new columns which show the true outcome of the match
         draw = ifelse(home_result == 0.5, 1, 0),
         away_win = ifelse(away_result == 1, 1, 0)) |> 
  select(game_date, season, game_id, home_team, away_team, home_prob, draw_prob, away_prob, home_win, draw, away_win)


# Run the multinomial log loss function from MLmetrics to output a log loss score for our sample
MultiLogLoss(
  y_pred = matches[,c("home_prob", "draw_prob", "away_prob")] %>% as.matrix(),
  y_true = matches[,c("home_win", "draw", "away_win")] %>% as.matrix()
)

schedule_df <- get_nhl_schedule(current_full_season) |> 
  filter(session == 'R') |> 
  mutate(game_id = as.numeric(game_id)) |> 
  invisible() |> 
  suppressWarnings()

existing_ids <- matches |> 
  pull(game_id)


# Predict Next Match ------------------------------------------------------

future_matches <- schedule_df |> 
  filter(!(game_id %in% existing_ids) & 
           game_date == Sys.Date()) |> 
  select(
    team_a = home_team,
    team_b = away_team
  ) |> 
  mutate(elo_a = final_elos[team_a],
         elo_b = final_elos[team_b],
         team_a_win_prob = elo.prob(elo.A = elo_a,
                                    elo.B = elo_b)
  )

future_matches


# Season ELO Rankings -----------------------------------------------------
data_with_probabilities |> 
  select(team = home_team) |> 
  unique() |> 
  anti_join(team_logos_colors |> 
              select(team = team_abbr))

team_logos_colors |> 
  select(team = team_abbr) |> 
  anti_join(data_with_probabilities |> 
              select(team = home_team) |> 
              unique())


gg_data <- data_with_probabilities |> 
  select(game_date,
         season,
         team = home_team,
         elo = home_elo,
         NULL) |> 
  rbind(
    data_with_probabilities |> 
      select(game_date,
             season,
             team = away_team,
             elo = away_elo,
             NULL)
  ) |> 
  mutate(
    team = case_when(team == 'N.J' ~ 'NJD',
                     team == 'S.J' ~ 'SJS',
                     team == 'L.A' ~ 'LAK',
                     team == 'T.B' ~ 'TBL', 
                     TRUE ~ team)
  ) |> 
  left_join(
    team_logos_colors |> 
    select(team = team_abbr,
           division,
           team_color1)  
  ) |> 
  mutate(
    days_ago = as.numeric(Sys.Date() - as.Date(game_date))
  ) |> 
  arrange(days_ago) |> 
  filter(season == current_full_season & 
           days_ago <= 14)

last_games <- gg_data |> 
  arrange(days_ago) |> 
  group_by(team) |> 
  filter(row_number()==1) |> 
  mutate(
    days_ago = 0
  ) |> 
  select(team, elo, division, days_ago)


theme_set(theme_cw_dark)
p <- gg_data |>
  ggplot(aes(
    x = days_ago,
    y = elo,
    group = team,
    color = team_color1
  )) +
  geom_line() +
  scale_colour_identity() +
  scale_x_reverse() +
  facet_wrap( ~ division, nrow = 1) +
  geom_text_repel(data = last_games,
                  aes(x = -1, label = team, color = theme_get()$text$colour),
                  direction = 'y') + 
  labs(
    title = glue('{current_season} Team Elo Rankings'),
    x = 'Days Ago',
    y = 'Elo'
  )

p

brand_plot(p, asp = 16/10, save_name = glue('plots/desktop/team_elo_{current_season}.png'), data_home = 'Data: NHL', fade_borders = '')
