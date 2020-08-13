# pbp_scrape <- readRDS("data/pbp_scrape3.rds")
# pbp_scrape$report$game_id %>% min()
# 
# f.scrape <- paste0("data/", list.files(path = "data/", pattern = "pbp_scrape"))
# 
# f.scrape[1:5]

fx.clean_scrape <- function(x) {
  y <- readRDS(x)
  y[grep("_df", names(y))]
  names(y)[1] <- y[1] %>% names() %>% substr(1, nchar(y[1] %>% names())-3)
  names(y)[6] <- y[6] %>% names() %>% substr(1, nchar(y[6] %>% names())-3)
  names(y)[7] <- y[7] %>% names() %>% substr(1, nchar(y[7] %>% names())-3)
  names(y)[8] <- y[8] %>% names() %>% substr(1, nchar(y[8] %>% names())-3)
  saveRDS(y, x)
  rm(y)
}

# Get list of files to clean
# f.scrape %>% lapply(fx.clean_scrape)

# Functions ---------------------------------------------------------------

fx.add_to_table <- function(fscrape) {
  scrape <- readRDS(fscrape)
  source(f.con)
  table_names <- dbListTables(con)
  if (length(names(scrape)) == 9) {
    print("Hello")
    tic("Total Upload")
    fx.scrape_upload <- function(x) {
      tic(
        scrape[
          grep(x,
               scrape %>%
                 names()
          )
        ] %>%
          names()
      )
      print(glue::glue("Uploading ", x))
      # print(glue::glue("Uploading ", x, "_test"))
      dbWriteTable(
        con,
        value = as.data.frame(scrape[grep(x, names(scrape))]),
        name = paste0(table_names[grep(x, table_names)]),
        # name = paste0(table_names[grep(x, table_names)], "_test"),
        # remove test once proven
        append = TRUE
      )
      print(glue::glue("Successfully added ", x))
      # print(glue::glue("Successfully added ", x, "_test"))
      toc()
    }
    names(scrape) %>%
      lapply(fx.scrape_upload)
  }
  if (length(names(scrape)) != 9) {
    print("Error, the list has a longer length than 9")
  }
  rm(scrape)
  dbDisconnect(con)
}