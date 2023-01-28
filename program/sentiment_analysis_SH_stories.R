# Here we will do sentiment analysis in tidy way (predefined lexicon based)

# clean up everything from environment
rm(list = ls())
graphics.off()

# load the necessary libraries
library(tidytext)   # for text mining
library(dplyr)      # data manipulation
library(tidyr)      # data wrangling 
library(widyr)      # pairwise correlation
library(circlize)   # for chord diagram
library(ggplot2)    # data visualization
library(igraph)     # network diagram
library(ggraph)     # network diagram
library(wordcloud2) # word cloud
library(extrafont)  # use for font style

# create our own theme and colour palette (using color picker)
custom_col_palette <- c('#FC1E05', '#FCE405', '#26FC05', '#05FCE4', '#0553FC', '#F405FC')
loadfonts(device = "win")
custom_theme <- theme_bw()+
  theme(text = element_text(family = 'Bookman Old Style'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 12),
        plot.subtitle = element_text(hjust = 0.5, face = 'italic', size = 10))

# set the working directory
setwd("K:/Data Science/Sherlock Holmes/")

# list up all the csv files
list.files(pattern = '.csv')

# read data
sholmes <- read.csv('SHolmesStories.csv', stringsAsFactors = FALSE, row.names = 1)

# Checking missing value
length(which(!complete.cases(sholmes)))

# some structural set up
sholmes$pub_date <- gsub(pattern = '\\D', replacement = '', sholmes$pub_date)
sholmes$collection <- as.factor(sholmes$collection)
sholmes <- sholmes %>% 
  mutate(decade = ifelse(sholmes$pub_date %in% 1890:1899, '1890s',
                         ifelse(sholmes$pub_date %in% 1900:1909, '1900s',
                                ifelse(sholmes$pub_date %in% 1910:1919, '1910s',
                                       ifelse(sholmes$pub_date %in% 1920:1929, '1920s','NA')))))

# alternative option for str()
glimpse(sholmes)

# set our own stop word list by looking at word cloud
our_stop_words <- c('holmes', 'upon','said', 'man', 'one', 'will','may',
                    'well', 'come', 'three', 'watson', 'shall', 'like', 
                    'now','never','must','see','can','two')

# Create tidy text format
sholmes_tidy <- sholmes %>% 
  unnest_tokens(word, main_story) %>%
  filter(!word %in% our_stop_words) %>% 
  filter(!nchar(word) < 3) %>% 
  anti_join(stop_words)

sholmes_tidy %>% 
  count(word, sort = TRUE) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot()+
  geom_col(aes(word, n))+coord_flip()+
  labs(x='', y = 'Word Count', title = 'Most Frequently Uses Words in Holmes Story')+
  custom_theme


sholmes_tidy %>% 
  dim()
sholmes_tidy %>% 
  head(n = 8)
sholmes_tidy %>% 
  glimpse()

word_summary <- sholmes_tidy %>% 
  group_by(decade, collection) %>% 
  mutate(word_count = n_distinct(word)) %>% 
  select(title, Decade = decade, Collection = collection, word_count) %>% 
  distinct() %>% 
  ungroup()
word_summary %>% 
  dim()
word_summary %>% 
  head(n = 8)

# descriptive analysis
decade_chart <- sholmes %>% 
  count(decade, collection)

circos.clear()
circos.par(gap.after = c(rep(5, length(unique(decade_chart[[1]])) - 1), 15,
                         rep(5, length(unique(decade_chart[[2]])) - 1), 15))
grid_col <- c('1890' = "#E69F00",
              '1900' = "#56B4E9",
              '1910' = "#009E73",
              '1920' = "#CC79A7")
chordDiagram(decade_chart, grid.col = grid_col, transparency = 0.4)
title('Relationship Between Decade and Collection of Story')

sholmes_bing <- sholmes_tidy %>% 
  inner_join(get_sentiments(lexicon = 'bing'))

sholmes_afinn <- sholmes_tidy %>% 
  inner_join(get_sentiments(lexicon = 'afinn'))

sholmes_nrc <- sholmes_tidy %>% 
  inner_join(get_sentiments(lexicon = 'nrc'))

# NRC plot
nrc_plot <- sholmes_nrc %>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count))

ggplot(data = nrc_plot,
       aes(x = sentiment, y = word_count, fill = sentiment)) +
  geom_col() + coord_flip() + guides(fill = FALSE) +
  labs(x = NULL, y = 'Word Count',
       title = 'NRC Sentiment') + custom_theme

# Bing plot
bing_plot <- sholmes_bing %>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count))

ggplot(data = bing_plot,
       aes(x = sentiment, y = word_count, fill = sentiment)) +
  geom_col() + coord_flip() + guides(fill = FALSE) +
  labs(x = NULL, y = 'Word Count',
       title = 'Bing Sentiment') + custom_theme

# Afinn Plot
afinn_plot <- sholmes_afinn %>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count))

ggplot(data = afinn_plot,
       aes(x = sentiment, y = word_count, fill = sentiment)) +
  geom_col() + coord_flip() + guides(fill = FALSE) +
  labs(x = NULL, y = 'Word Count',
       title = 'NRC Sentiment') + custom_theme

# Polarity over time
polarity_year <- sholmes_bing %>% 
  count(sentiment, pub_date) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive-negative,
         percent_positive = positive/(positive+negative)*100)

ggplot(data = polarity_year,
       aes(x = pub_date, y = polarity, color = ifelse(polarity >= 0, 'green', 'red'))) +
  geom_col() +
  geom_smooth(method = 'loess') +
  geom_smooth(method = 'lm', aes(color = 'blue')) +
  labs(x = 'Year', y = 'Polarity', title = "Polarity Over Time") +
  custom_theme

# Word Plot
word_plot <- sholmes_nrc %>% 
  group_by(sentiment) %>% 
  count(word, sort = TRUE) %>% 
  arrange(desc(n)) %>% 
  slice(seq_len(10)) %>% 
  ungroup()

ggplot(data = word_plot,
       aes(x = word, y = 1, label = word, fill = sentiment)) +
  geom_point(color = 'transparent') +
  ggrepel::geom_label_repel(force = 1,nudge_y = 0.8,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 4) +
  facet_grid(~sentiment) + coord_flip() + guides(fill = F) +
  labs(x = 'Sentiment', y = 'Word', title = '10 Most Written Word in Each Sentiment Category') +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 10),
        text = element_text(family = 'Bookman Old Style'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 12))
  
# Rader chart

sholmes_nrc_sub <- sholmes_tidy %>% 
  inner_join(get_sentiments('nrc')) %>% 
  filter(!sentiment %in% c('positive', 'negative'))

# word cloud
sholmes_tidy %>% 
  inner_join('bing') %>% 
  count(word, sentiment, sort = T) %>% 
  reshape2::acast(word~sentiment, value.var = 'n', fill = 0) %>% 
  wordcloud::comparison.cloud(colors = c('red', 'dark green'),
                              max.words = 100)

library(reshape2)
library(wordcloud)
sholmes_tidy %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)










