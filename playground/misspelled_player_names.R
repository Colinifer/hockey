names_df <- on_ice_data |>
  anti_join(all_events_data,
            by = c('season', 'game_id', 'player_name')) |>
  select(player_name) |>
  unique() |> 
  rbind(
    all_events_data |>
      anti_join(on_ice_data,
                by = c('season', 'game_id', 'player_name')) |>
      select(player_name) |>
      unique()
  )

stringdist_names <- names_df |> 
  pull(player_name)

dist.matrix <- stringdistmatrix(stringdist_names,
                                stringdist_names,
                                method = 'jw',
                                p = 0.01)

row.names(dist.matrix) <- stringdist_names
names(dist.matrix) <- stringdist_names
dist.matrix <- as.dist(dist.matrix)

clusts <- hclust(dist.matrix,method="ward.D2") 

plot(clusts)

names_df$likely_name <- stats::cutree(clusts,h=0.2)

find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

names_df |> 
  group_by(likely_name) |> 
  mutate(group_name = find_mode(player_name)) |> 
  ungroup() |> 
  select(group_name) |> 
  unique()
