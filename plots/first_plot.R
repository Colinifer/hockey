source("../initR/con.R")
shots <- dbGetQuery(con, "SELECT * FROM `hockey`.`pbp_base` WHERE (`season` = '20092010') AND (`event_type` = 'shot');")
dbDisconnect(con)

shots.clean <- shots[!is.na(shots$coords_x), ]

# Move the coordinates to non-negative values before plotting
shots.clean$coordx <- shots.clean$coords_x + abs(min(shots.clean$coords_x))
shots.clean$coordy <- shots.clean$coords_y + abs(min(shots.clean$coords_y))

# goals <- plays[result.event == "Goal"]

ggplot(shots.clean, aes(x = coordx, y = coordy)) +
  labs(title = "2009 Shots") +
  geom_point(alpha = 0.1, size = 0.2) +
  xlim(0, 198) + ylim(0, 84) +
  geom_density_2d_filled(alpha = 1, show.legend = FALSE) +
  theme_void()
