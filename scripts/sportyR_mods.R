geom_nhl_personal <- function (full_surf = TRUE, rotate = FALSE, rotation_dir = "ccw", 
                      unit = "ft", caption_color = "#707372", background_color = '#fcfcfc', 
                      ...) 
{
  faceoff_spots = list(center = c(0, 0), spot_1 = c(-69, -22), 
                       spot_2 = c(-69, 22), spot_3 = c(-20, -22), spot_4 = c(-20, 
                                                                             22))
  if (full_surf) {
    faceoff_spots$spot_5 = c(-1 * faceoff_spots$spot_4[1], 
                             faceoff_spots$spot_4[2])
    faceoff_spots$spot_6 = c(-1 * faceoff_spots$spot_3[1], 
                             faceoff_spots$spot_3[2])
    faceoff_spots$spot_7 = c(-1 * faceoff_spots$spot_2[1], 
                             faceoff_spots$spot_2[2])
    faceoff_spots$spot_8 = c(-1 * faceoff_spots$spot_1[1], 
                             faceoff_spots$spot_1[2])
  }
  color_list = nhl_features_set_colors(...)
  boards = nhl_feature_boards(full_surf, rotate, rotation_dir)
  center_line = nhl_feature_center_line(full_surf, rotate, 
                                        rotation_dir)
  blue_line = nhl_feature_blue_line(full_surf, rotate, rotation_dir)
  goal_line = nhl_feature_goal_line(full_surf, rotate, rotation_dir)
  goalkeepers_restricted_area = nhl_feature_goalkeepers_restricted_area(full_surf, 
                                                                        rotate, rotation_dir)
  goal_crease = nhl_feature_goal_crease(full_surf, rotate, 
                                        rotation_dir)
  referee_crease = nhl_feature_referee_crease(full_surf, rotate, 
                                              rotation_dir)
  goal = nhl_feature_goal(full_surf, rotate, rotation_dir)
  if (!(unit %in% c("ft", "feet"))) {
    boards = convert_units(boards, "ft", unit, conversion_columns = c("x", 
                                                                      "y"))
    center_line = convert_units(center_line, "ft", unit, 
                                conversion_columns = c("x", "y"))
    blue_line = convert_units(blue_line, "ft", unit, conversion_columns = c("x", 
                                                                            "y"))
    goal$goal = convert_units(goal$goal, "ft", unit, conversion_columns = c("x", 
                                                                            "y"))
    goal$goal_fill = convert_units(goal$goal_fill, "ft", 
                                   unit, conversion_columns = c("x", "y"))
    goalkeepers_restricted_area = convert_units(goalkeepers_restricted_area, 
                                                "ft", unit, conversion_columns = c("x", "y"))
    goal_line = convert_units(goal_line, "ft", unit, conversion_columns = c("x", 
                                                                            "y"))
    goal_crease$goal_crease_outline = convert_units(goal_crease$goal_crease_outline, 
                                                    "ft", unit, conversion_columns = c("x", "y"))
    goal_crease$goal_crease_fill = convert_units(goal_crease$goal_crease_fill, 
                                                 "ft", unit, conversion_columns = c("x", "y"))
    referee_crease = convert_units(referee_crease, "ft", 
                                   unit, conversion_columns = c("x", "y"))
  }
  g = create_plot_base(rotate, caption_color, background_color)
  g = add_feature(g, boards, color_list$boards_color)
  g = add_feature(g, center_line, color_list$center_line_color)
  g = add_feature(g, blue_line, color_list$blue_line_color)
  g = add_feature(g, goal$goal, color_list$goal_color)
  g = add_feature(g, goal$goal_fill, color_list$goal_fill_color)
  g = add_feature(g, goal_line, color_list$goal_line_color)
  g = add_feature(g, goalkeepers_restricted_area, color_list$goalkeepers_restricted_area_color)
  g = add_feature(g, goal_crease$goal_crease_outline, color_list$goal_crease_outline_color)
  g = add_feature(g, goal_crease$goal_crease_fill, color_list$goal_crease_fill_color)
  g = add_feature(g, referee_crease, color_list$referee_crease_color)
  for (spot in 1:length(faceoff_spots)) {
    spot_name = names(faceoff_spots[spot])
    center = faceoff_spots[[spot]]
    faceoff_spot = nhl_feature_faceoff_spot(center, full_surf, 
                                            rotate, rotation_dir)
    faceoff_circle = nhl_feature_faceoff_circle(center, 
                                                full_surf, rotate, rotation_dir)
    faceoff_lines = nhl_feature_faceoff_lines(center, full_surf, 
                                              rotate, rotation_dir)
    if (identical(center, c(0, 0))) {
      if (!(unit %in% c("ft", "feet"))) {
        faceoff_spot = convert_units(faceoff_spot, "ft", 
                                     unit, conversion_columns = c("x", "y"))
        faceoff_circle = convert_units(faceoff_circle, 
                                       "ft", unit, conversion_columns = c("x", "y"))
      }
      g = add_feature(g, faceoff_spot, color_list$center_faceoff_spot_color)
      g = add_feature(g, faceoff_circle, color_list$center_faceoff_circle_color)
    }
    else if (spot_name %in% c("spot_1", "spot_2", "spot_7", 
                              "spot_8")) {
      if (!(unit %in% c("ft", "feet"))) {
        faceoff_spot$spot_outer_ring = convert_units(faceoff_spot$spot_outer_ring, 
                                                     "ft", unit, conversion_columns = c("x", "y"))
        faceoff_spot$spot_fill = convert_units(faceoff_spot$spot_fill, 
                                               "ft", unit, conversion_columns = c("x", "y"))
        faceoff_circle = convert_units(faceoff_circle, 
                                       "ft", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_ul = convert_units(faceoff_lines$faceoff_line_ul, 
                                                      "ft", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_ur = convert_units(faceoff_lines$faceoff_line_ur, 
                                                      "ft", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_ll = convert_units(faceoff_lines$faceoff_line_ll, 
                                                      "ft", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_lr = convert_units(faceoff_lines$faceoff_line_lr, 
                                                      "ft", unit, conversion_columns = c("x", "y"))
      }
      g = add_feature(g, faceoff_spot$spot_outer_ring, 
                      color_list$faceoff_spot_outer_ring_color)
      g = add_feature(g, faceoff_spot$spot_fill, color_list$faceoff_spot_fill_color)
      g = add_feature(g, faceoff_circle, color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_ul, 
                      color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_ur, 
                      color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_ll, 
                      color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_lr, 
                      color_list$non_center_faceoff_circle_color)
    }
    else {
      if (!(unit %in% c("ft", "feet"))) {
        faceoff_spot$spot_outer_ring = convert_units(faceoff_spot$spot_outer_ring, 
                                                     "ft", unit, conversion_columns = c("x", "y"))
        faceoff_spot$spot_fill = convert_units(faceoff_spot$spot_fill, 
                                               "ft", unit, conversion_columns = c("x", "y"))
      }
      g = add_feature(g, faceoff_spot$spot_outer_ring, 
                      color_list$faceoff_spot_outer_ring_color)
      g = add_feature(g, faceoff_spot$spot_fill, color_list$faceoff_spot_fill_color)
    }
  }
  return(g)
}

create_base_plot <- function (rotate = FALSE, caption_color = "#707372", background_color = NULL) 
{
  if (is.null(caption_color)) {
    stop("Caption color must not be NULL")
  }
  if (!is.null(background_color)) {
    background = ggplot2::element_rect(fill = background_color)
  }
  else {
    background = ggplot2::element_blank()
  }
  if (rotate) {
    g = ggplot2::ggplot() + ggplot2::coord_fixed() + ggplot2::theme(plot.caption = ggplot2::element_text(color = caption_color, 
                                                                                                         hjust = 0.5), plot.margin = ggplot2::margin(0, -1, 
                                                                                                                                                     0, -1, "cm"), panel.border = ggplot2::element_blank(), 
                                                                    panel.background = background, axis.title = ggplot2::element_blank(), 
                                                                    axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
                                                                    panel.grid = ggplot2::element_blank(), ) + ggplot2::labs(caption = "Plot made via sportyR")
  }
  else {
    g = ggplot2::ggplot() + ggplot2::coord_fixed() + ggplot2::theme(plot.caption = ggplot2::element_text(color = caption_color, 
                                                                                                         hjust = 0.5), plot.margin = ggplot2::margin(-1, 
                                                                                                                                                     0, -1, 0, "cm"), panel.border = ggplot2::element_blank(), 
                                                                    panel.background = background, axis.title = ggplot2::element_blank(), 
                                                                    axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
                                                                    panel.grid = ggplot2::element_blank(), ) + ggplot2::labs(caption = "Plot made via sportyR")
  }
  return(g)
}

