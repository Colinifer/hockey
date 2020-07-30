source("../initR/con.R")
shots <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_base` WHERE (`game_id` >= '2012030411') AND (`season` = '20122013');")
dbDisconnect(con)

games <- unique(shots$game_id)

shots.clean <- shots[!is.na(shots$coords_x), ] %>% filter(game_id == games[6])

# Move the coordinates to non-negative values before plotting
shots.clean$coordx <- shots.clean$coords_x + abs(min(shots.clean$coords_x))
shots.clean$coordy <- shots.clean$coords_y + abs(min(shots.clean$coords_y))

# goals <- plays[result.event == "Goal"]

ggplot(shots.clean, aes(x = coordx, y = coordy)) +
  labs(title = "2013 SCF Shots") +
  geom_point(alpha = 0.5, size = 0.2) +
  xlim(0, 198) + ylim(0, 84) +
  # stat_density2d(aes(fill = stat(level)), geom="polygon") +
  geom_density_2d_filled(alpha = .8, show.legend = FALSE) +
  theme(legend.position = "magma")
  # theme_void()
