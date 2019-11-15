
library(ggplot2)


podlove_import_clean <- function(fn = "podlove-episode-downloads.csv", ref_fn = "ref_pit.csv") {
  
  # imports, cleans and tidies podlove analytics export data for later analysis
  
  # INPUT
  # fn: file name of the podlove csv data
  # ref_fn: file name of the reference file for points in time calculations
  
  # OUTPUT
  # podlove_tidy = a data frame containing clean & tidy podlove data
  
  ########################################################################
  
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(stringr)
  
  # get data
  
  podlove_raw <- read.csv(fn, encoding = "UTF-8", as.is = TRUE)
  ref_pit <- read.csv(ref_fn, as.is = TRUE)
  
  # clean data: dates, select vars
  
  podlove_clean <- podlove_raw %>% 
    mutate(post_date = ymd_hm(post_date)) %>% 
    select(id, title, post_date, days_since_release:X1d)  
  
  # tidy data: one entry per episode and point in time (pit)
  
  podlove_tidy <- podlove_clean %>% 
    gather(key = "point_in_time", value = "downloads_in_time", X3y:X1d) %>% 
    arrange(post_date) %>% 
    left_join(ref_pit, by="point_in_time") %>% 
    mutate(download_per_day_at_pit = downloads_in_time / pit_days) %>% 
    filter(!is.na(downloads_in_time))
  
  # get rid of X in point_in_time
  podlove_tidy$point_in_time <- str_replace(podlove_tidy$point_in_time, "X", "")

  podlove_tidy
  
}

most_downloaded <- function(df_tidy_data, top) {
  
  # creates a list of most downloaded podcast episodes
  
  # INPUT
  # df_tidy_data: a data frame containing tidy podlove data
  # top: length of the ranked list; negative numbers show least n downloads
  
  # OUTPUT
  # most_dls = a data frame containing a list of most/least downloaded episodes
  
  ########################################################################
  
 library(dplyr)

  most_dls <- df_tidy_data %>% 
    select(id, title, days_since_release, downloads) %>% 
    unique() %>% 
    arrange(desc(downloads)) %>% 
    top_n(n = top)
  
  most_dls
  
}

best_starts <- function(df_tidy_data, start_def = "1w", top) {
  
  # creates a list of of best episode starts, defined by a time period 
  
  # INPUT
  # df_tidy_data: a data frame containing tidy podlove data
  # start_def: definition of what is considered an episode start/launch duration
  #           available: 1d, 2d, 3d, 4d, 5d, 6d, 1w (default), 2w, 3w, 4w, 1q, 2q, 3q, 1y...
  # top: length of the ranked list; negative numbers show least n downloads
  
  # OUTPUT
  # most_dls = a data frame containing a list of episodes with the best launches
  
  ########################################################################
  
  library(dplyr)

  best_start <- df_tidy_data %>% 
    filter(point_in_time == start_def) %>% 
    arrange(desc(download_per_day_at_pit)) %>% 
    top_n(n = top)
  
  best_start
  
}
evergreens <- function(df_tidy_data, top, min_age = 180) {
  
  # creates a list of best episodes judged by their overall average downloads per day,
  # filtering out new episodes (which usually have a high average)
  
  # INPUT
  # df_tidy_data: a data frame containing tidy podlove data
  # min_age: filter threshold for what is considered an "aged" episode (defaults to 180 days)
  # top: length of the ranked list; negative numbers show least n downloads
  
  # OUTPUT
  # evergreens = a data frame containing a list most "evergreen" episodes
  
  ########################################################################
  
  library(dplyr)

  evergreens <- df_tidy_data %>% 
    select(id, title, days_since_release, downloads) %>% 
    unique() %>% 
    mutate(downloads_per_day_overall = downloads / days_since_release) %>% 
    filter(days_since_release > min_age) %>% 
    arrange(desc(downloads_per_day_overall)) %>% 
    top_n(n = top)
  
  evergreens
  
}

graph_average_downloads <- function(df_tidy_data) {
  
  # creates a line graph plotting out days since episode launch against,
  # average downloads at the point in time over all episodes
  
  # INPUT
  # df_tidy_data: a data frame containing tidy podlove data
  
  # OUTPUT
  # g_avg_dls = a line plot
  
  ########################################################################
  
  library(dplyr)
  library(ggplot2)
  
  # calculate average downloads at point in time over all episodes
  
  average_dls <- df_tidy_data %>% 
    
    group_by(pit_days) %>% 
    summarize(downloads_in_time_avg = mean(downloads_in_time))
  
  # create line plot
  
  g_avg_dls <- ggplot(average_dls,
                      aes(x = pit_days, y = downloads_in_time_avg)) +
    geom_line()
  
  # print 
  
  print(g_avg_dls)
  
  g_avg_dls
  
}

graph_download_curves <- function(df_tidy_data) {
  
  # creates a line graph plotting out days since episode launch against,
  # average downloads at the point in time, showing all episodes
  
  # INPUT
  # df_tidy_data: a data frame containing tidy podlove data
  
  # OUTPUT
  # g_dl_curves = a multiline plot
  
  ### TO DO: correct labeling of episodes, currently snipped off at the right end
  
  ########################################################################
  
  library(dplyr)
  library(ggplot2)
  library(directlabels)
  
  # create line plot
  
  g_dl_curves <- ggplot(df_tidy_data, 
                        aes(x = pit_days, y = downloads_in_time, color = title)) +
    geom_line(alpha = 0.5) +
    guides(color = FALSE) +
    geom_dl(aes(label = title), method = list("last.points", cex = 0.8))
  
  # print 
  
  print(g_dl_curves)  
  
  g_dl_curves
  
}


# EXAMPLE CALCULATIONS

sample_data <- podlove_import_clean(fn = "sample_podlove_data.csv")

top_5_downloaded_episodes <- most_downloaded(sample_data, 5)

worst_3_downloaded_episodes <- most_downloaded(sample_data, -3)

best_launches_after_4_days_top3 <- best_starts(sample_data, "4d", 3)

all_time_favorites_top5 <-evergreens(sample_data, 5)

my_overall_download_graph <- graph_average_downloads(sample_data)

my_episode_curves <- graph_download_curves(sample_data)
