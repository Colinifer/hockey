games <- c(2019030151:2019030155)
pbp <- sc.scrape_pbp(games = games)

colnames(pbp$pbp_base)

pbp$pbp_base$event_zone %>% unique()
pbp$pbp_base$event_type %>% unique()

pbp$pbp_base %>% filter(event_type == "SHOT" | event_type == "GOAL")

rink <- png::readPNG("plots/assets/full-rink-lateral.png")
rink1 <- png::readPNG("plots/assets/nhl_rink 2.png")

d <- ggplot(pbp$pbp_base %>% filter(event_type == "SHOT" | event_type == "GOAL") %>% filter(event_zone == "Off"), aes(coords_x, coords_y)) +
  background_image(rink) +
  geom_hex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


# pbp1 <- sc.scrape_pbp(games = 2019030111:2019030145, live_scrape = T)
pbp1 <- readRDS("data/pbp_scrape3.rds")
pbp1 <- pbp1$pbp_base
source("../initR/con.R")
pbp1 <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_base` WHERE (`season` = '20112012');")
shots <- pbp1 %>% filter(event_type == "SHOT" | event_type == "GOAL") %>% filter(event_zone == "Off")
shots$coords_x <- abs(shots$coords_x)

ggplot(shots, aes(coords_x, coords_y)) +
  background_image(rink1) +
  # geom_point(
  #   aes(color = event_type),
  #   size = 2.5
  # )
    # ) +
  geom_hex(binwidth = c(2.5, 2.5), alpha = .75) +
  scale_fill_continuous(type = "viridis")
  theme_bw()
  # geom_text_repel(
  #   data = subset(shots, event_type == "GOAL"), 
  #   aes(label = paste(event_player_1)),
  #   size = 5,
  #   box.padding = unit(0.35, "lines"),
  #   point.padding = unit(0.3, "lines")
  #   )


paste(
  pbp1$pbp_base$away_team[nrow(pbp1$pbp_base)],
  "-",
  pbp1$pbp_base$away_score[nrow(pbp1$pbp_base)],
  "|",
  pbp1$pbp_base$home_team[nrow(pbp1$pbp_base)],
  "-",
  pbp1$pbp_base$home_score[nrow(pbp1$pbp_base)]
)
  

function (x) {
  if (condition) {
    
  }
}