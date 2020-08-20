games <- c(2019030151:2019030155)
pbp <- sc.scrape_pbp(games = games)

colnames(pbp$pbp_base)

pbp$pbp_base$event_zone %>% unique()
pbp$pbp_base$event_type %>% unique()

pbp$pbp_base %>% filter(event_type == "SHOT" | event_type == "GOAL")

rink <- png::readPNG("plots/assets/full-rink-lateral.png")

d <- ggplot(pbp$pbp_base %>% filter(event_type == "SHOT" | event_type == "GOAL") %>% filter(event_zone == "Off"), aes(coords_x, coords_y)) +
  background_image(rink) +
  geom_hex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


pbp1 <- sc.scrape_pbp(games = 2019030115, live_scrape = T)

ggplot(pbp1$pbp_base %>% filter(event_type == "SHOT" | event_type == "GOAL") %>% filter(event_zone == "Off"), aes(coords_x, coords_y)) +
  background_image(rink) +
  geom_point(aes(color=event_type), size = 2.5) +
  # geom_hex(binwidth = c(2.5, 2.5)) +
  # scale_fill_continuous(type = "viridis") +
  theme_bw() + 
  # geom_text_repel(aes(label = pbp1$pbp_base$event_player_1))
  
  paste(
    pbp1$pbp_base$away_team[nrow(pbp1$pbp_base)],
    "-",
    pbp1$pbp_base$away_score[nrow(pbp1$pbp_base)],
    "|",
    pbp1$pbp_base$home_team[nrow(pbp1$pbp_base)],
    "-",
    pbp1$pbp_base$home_score[nrow(pbp1$pbp_base)]
  )
  