# Clear everything form environment
rm(list = ls())

# clear plot window
graphics.off()

# Set working directory 
setwd("K:/Data Science/Sherlock Holmes/")

# List up all the csv file
list.files(pattern = '.csv')

# Load the necessary libraries (packages)
library(dplyr)      # data manipulation
library(tidytext)   # dealing with text data in tidy approach
library(ggplot2)    # data visualization
library(wordcloud2) # for word cloud

# Load the script containing custom created functions
source('our functions.R')

# Load the data file (SHolmesStories.csv)
sholmes <- read.csv('SHolmesStories.csv',
                    stringsAsFactors = FALSE)

full_word_count <- sholmes %>%
  unnest_tokens(word, main_story) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words)) +
  ylab("Story Count") + 
  xlab("Word Count per Story") +
  ggtitle("Word Count Distribution")+custom_theme()

# Checking missing values
length(which(!complete.cases(sholmes)))

# Remove the first column
sholmes <- sholmes[, -1]

# Take only publication year and transform into integer data type
sholmes$pub_date <- gsub(pattern = '\\D', replacement = '', sholmes$pub_date)
sholmes$pub_date <- as.integer(sholmes$pub_date)

# Transform collection type as factor variable
sholmes$collection <- as.factor(sholmes$collection)

# Create decade column
sholmes <- sholmes %>% 
  mutate(decade = ifelse(sholmes$pub_date %in% 1890:1899, '1890s',
                         ifelse(sholmes$pub_date %in% 1900:1909, '1900s',
                                ifelse(sholmes$pub_date %in% 1910:1919, '1910s',
                                       ifelse(sholmes$pub_date %in% 1920:1929, '1920s','NA')))))
# Looking for structure of dataset (aliter of str())
glimpse(sholmes)

# ------------------------- Descriptive Analysis ---------------------------------

# For simplicity we just remove column main_story from main data set
explore <- sholmes[, -6]
glimpse(explore)

# Plot of count of stories by year and collection
df1 <- explore %>% 
  group_by(pub_date, collection) %>% 
  summarise(story_count = n())

ggplot(data = df1,
       aes(x = pub_date, y = story_count, fill = collection)) +
  geom_bar(stat = 'identity') + 
  labs(x = 'Published Year', y = 'Number of Story', title = 'Story Published') +
  custom_theme()

# Plot of count of stories by decade and collection
df2 <- explore %>% 
  group_by(decade, collection) %>% 
  summarise(story_count = n())

ggplot(data = df2,
       aes(x = decade, y = story_count, fill = collection)) +
  geom_bar(stat = 'identity') + 
  labs(x = 'Deacde', y = 'Number of Story', title = 'Story Published') +
  custom_theme()

# Plot of count of stories by year and collection
df3 <- explore %>% 
  group_by(collection) %>% 
  summarise(story_count = n())

ggplot(data = df3,
       aes(x = collection, y = story_count, fill = collection)) +
  geom_bar(stat = 'identity') + guides(fill = FALSE) +  
  labs(x = 'Collection', y = 'Number of Story', title = 'Story Collection') +
  custom_theme()

# Plot of top 10 ranked story
top_10 <- explore %>% 
  mutate(Type = ifelse(explore$rank %in% 1:10, 'top_10', 'others')) %>% 
  group_by(decade, Type, collection) %>% 
  summarise(story_count = n())
ggplot(data = top_10,
       aes(x = decade, y = story_count, fill = Type)) +
  geom_bar(stat = 'identity') + labs(x = "Decade", y = "Number of Story", title = 'Top 10 Story According to Rank')+
  custom_theme()

# Word Count Distribution

