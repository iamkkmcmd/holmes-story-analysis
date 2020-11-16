# Clear environment
rm(list = ls())
graphics.off()

# set working directory
setwd("K:/Data Science/Sherlock Holmes/")

# To get list of all files
story_name <- list.files("K:/Data Science/Sherlock Holmes/sherlock_short_stories/", pattern = '.txt')

# To get list of all files with their existing location
story_name_wl <- paste('K:/Data Science/Sherlock Holmes/sherlock_short_stories/',story_name, sep = '')

# Common license text in all text files, we have to remove that
license <- paste("---------- This text is provided to you 'as-is' without any warranty.",
                 "No warranties of any kind, expressed or implied, are made to you as to the text or",
                 "any medium it may be on, including but not limited to warranties of merchantablity or",
                 "fitness for a particular purpose. This text was formatted from various free ASCII and HTML variants.",
                 "See http://sherlock-holm.es for an electronic form of this text and additional information about it.",
                 "This text comes from the collection's version 3.1.", sep = " ")

# Function of read the story 
read_story <- function(txtfile){
  story <- readLines(txtfile)
  story <- story[-(1:9)]
  story <- gsub(pattern = "\"", replacement = "'", story)
  story <- gsub(pattern = '"', replacement = "'", story)
  story <- paste(story, collapse = "")
  story <- tm::stripWhitespace(story)
  story <- gsub(pattern = license, replacement = "", story)
  return(story)
}

# Make a dataframe containing stories in a column
story_df_SA <- data.frame()

for(i in 1:56){
  story_df_SA[i,1] <- read_story(txtfile = story_name_wl[i])
}

# read the stories features dataset 
data <- read.csv('SHolmesFeatures.csv')

df <- data.frame(data[,(2:6)], story_df_SA$V1)
names(df) <- c('title', 'abbvr', 'rank', 'pub_date', 'collection', 'main_story')

# Save the dataset df in a csv file
write.csv(df, file = 'K:/Data Science/Sherlock Holmes/SHolmesStories.csv')
