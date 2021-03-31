
# Player Images -----------------------------------------------------------

ex_url <- 'https://cms.nhl.bamgrid.com/images/headshots/current/168x168/8478402.jpg'

str_replace(ex_url, ".*[/]([^.]+)[.].*", "\\1")

ex_url %>% 
  str_remove('https://cms.nhl.bamgrid.com/images/headshots/current/168x168/') %>% 
  str_remove('.jpg')

# clean image and write to disk
clean__img_transparent <- function(img_url, trim = FALSE){

  # find the name of the img and extract it
  img_name <- str_replace(img_url, ".*[/]([^.]+)[.].*", "\\1")
  
  # some images need to be trimmed
  trim_area <- if(isTRUE(trim)){
    geometry_area(0, 0, 0, 10)
  } else {
    geometry_area(0, 0, 0, 0)
  }
  
  # Tom Mock method ---
  # img_url %>% 
  #   image_read() %>% 
  #   image_crop(geometry = trim_area) %>% 
  #   image_fill(
  #     color = 'transparent', 
  #     refcolor = 'white', 
  #     fuzz = 4,
  #     point = '+1+1'
  #   ) %>% 
  #   image_write(path = glue('assets/player_images/{img_name}.png'), format ='png')
  
  # @dmi3k erode method ---
  img <- img_url %>% 
    image_read()
  
  mask <- img %>% 
    image_convert(type = 'Grayscale') %>% 
    image_threshold(type = 'black', threshold = '85%') %>% 
    image_negate() %>% 
    image_morphology('Close', 'Diamond:3') %>% 
    image_morphology('Erode', 'Diamond')
  mask
  
  img %>% 
    image_composite(mask, operator = 'CopyOpacity') %>% 
    image_background('none') %>% 
    # image_scale('200%') %>%
    image_write(path = glue('assets/player_images/{img_name}.png'), format ='png')
  
  Sys.sleep(14)
}

existing_player_images <- glue('https://cms.nhl.bamgrid.com/images/headshots/current/168x168/{list.files("assets/player_images/")}')

existing_player_images <- glue('https://cms.nhl.bamgrid.com/images/headshots/current/168x168/{list.files("assets/player_images/") %>% 
  strtrim(nchar(.)-4)}.jpg')

skater_include <- roster_df %>% 
  # slice(1:10) %>%
  select(img_url = headshot_url) %>% 
  filter(
    str_detect(img_url, pattern = 'NA.jpg', negate = TRUE) &
      !(img_url %in% existing_player_images[3:length(existing_player_images)]) &
      !(img_url %in% c('https://cms.nhl.bamgrid.com/images/headshots/current/168x168/8482142.jpg',
                       'https://cms.nhl.bamgrid.com/images/headshots/current/168x168/8482241.jpg',
                       'https://cms.nhl.bamgrid.com/images/headshots/current/168x168/8480313.jpg',
                       'https://cms.nhl.bamgrid.com/images/headshots/current/168x168/8480844.jpg',
                       NULL)
        )
           ) %>% 
  # mutate(trim = c(TRUE, rep(FALSE, 791))) %>% 
  identity()

skater_include

skater_include %>% 
  pwalk(clean__img_transparent)



# Team logos --------------------------------------------------------------

logo_url  <- "http://content.sportslogos.net/logos/1/16/thumbs/124.gif"                     

raw_logo <- logo_url %>%
  image_read() 

raw_logo %>%
  image_ggplot()

img_filled <- raw_logo %>% 
  image_fill("green", "+1+1", fuzz = 50, refcolor = "white") %>% 
  image_fill("green", "+140+1", fuzz = 50, refcolor = "white") %>% 
  image_fill("green", "+1+99", fuzz = 50, refcolor = "white") %>% 
  image_fill("green", "+140+99", fuzz = 50, refcolor = "white")

img_filled %>% 
  image_ggplot()

img_filled <- raw_logo %>% 
  image_fill("transparent", "+1+1", fuzz = 50, refcolor = "white") %>% 
  image_fill("transparent", "+140+1", fuzz = 50, refcolor = "white") %>% 
  image_fill("transparent", "+1+99", fuzz = 50, refcolor = "white") %>% 
  image_fill("transparent", "+140+99", fuzz = 50, refcolor = "white")

img_filled %>% 
  image_channel("Opacity") %>% 
  image_convert(matte=FALSE) %>% 
  image_ggplot()


logo_mask <- img_filled %>% 
  image_channel("Opacity") %>% 
  image_convert(matte=FALSE) %>% 
  image_negate() %>% 
  image_blur()

logo_mask %>% 
  image_ggplot()

image_composite(raw_logo, logo_mask, operator = "CopyOpacity") %>% 
  image_ggplot()



clean_logo_transparent <- function(img_url) {
  
  # find the name of the img and extract it
  img_name <- str_replace(img_url, ".*[/]([^.]+)[.].*", "\\1")
  
  raw_img <- img_url %>%
    image_read() %>% 
    image_convert("PNG")
  
  img_mask <- raw_img  %>% 
    image_fill("transparent", "+1+1", fuzz = 2, refcolor = "white") %>% 
    image_fill("transparent", "+1+99", fuzz = 2, refcolor = "white") %>% 
    image_fill("transparent", "+140+1", fuzz = 2, refcolor = "white") %>% 
    image_fill("transparent", "+140+99", fuzz = 2, refcolor = "white") %>% 
    image_channel("Opacity") %>%
    image_convert(matte=FALSE) %>%
    image_negate() %>%
    image_blur()
  
  
  image_composite(raw_img, img_mask, operator = "CopyOpacity") %>%
    image_write(glue('assets/team_images/{img_name}.png'))
}

team_include <- teamcolors::teamcolors %>% 
  filter(
    league == 'nhl'
  ) %>% 
  # slice(1:10) %>%
  select(img_url = logo) %>% 
  # mutate(trim = c(TRUE, rep(FALSE, 791))) %>% 
  identity()

team_include

team_include %>% 
  pwalk(clean_logo_transparent)
