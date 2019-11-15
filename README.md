# Podlove Analytics Functions

Contains a number of functions for data cleaning, analysis and plotting of Podlove podcast analytics data

##podlove_import_clean

imports, cleans and tidies podlove analytics export data for later analysis

## most_downloaded

creates a list of most downloaded podcast episodes

## best_starts

creates a list of of best episode starts, defined by a time period 

## evergreens
  
creates a list of best episodes judged by their overall average downloads per day, filtering out new episodes (which usually have a high average)

# graph_average_downloads <- function(df_tidy_data) 

creates a line graph plotting out days since episode launch against average downloads at the point in time over all episodes
  
# graph_download_curves
  
creates a line graph plotting out days since episode launch against average downloads at the point in time, showing all episodes
  