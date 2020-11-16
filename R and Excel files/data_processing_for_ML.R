# Clear the environment
rm(list = ls())
graphics.off()

# save the start time for calculating spending time for making this dataset
start_time <- Sys.time()

# set the working directory
setwd("K:/Data Science/Sherlock Holmes/")

# list of files
story_name <- list.files("K:/Data Science/Sherlock Holmes/sherlock_short_stories/", pattern = '.txt')

# list of files with their location
story_name_wl <- paste('K:/Data Science/Sherlock Holmes/sherlock_short_stories/',story_name, sep = '')

# Storing the common lisence text for removing 
license <- paste("---------- This text is provided to you 'as-is' without any warranty.",
"No warranties of any kind, expressed or implied, are made to you as to the text or",
"any medium it may be on, including but not limited to warranties of merchantablity or",
"fitness for a particular purpose. This text was formatted from various free ASCII and HTML variants.",
"See http://sherlock-holm.es for an electronic form of this text and additional information about it.",
"This text comes from the collection's version 3.1.", sep = " ")

# function for read only title of story
story_title <- function(txtfile){
  story <- readLines(txtfile)
  title <- tm::stripWhitespace(paste(story[3:6], collapse = ''))
  return(title)
}

# function for read the story
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

# taking a data frame
story_df <- data.frame()

# load the model for extracting Parts of Speech tag
ud_model <- udpipe::udpipe_load_model(file = 'K:/Data Science/Sherlock Holmes/english-ewt-ud-2.4-190531.udpipe')

# looping for run the above functions and model for all stories
for(i in 1:56){
  
  story_df[i,1] <- story_title(txtfile = story_name_wl[i])
  story_df[i,2] <- gsub(pattern = '.txt', replacement = '', x = story_name[i])
  
  actual_story <- read_story(txtfile = story_name_wl[i])
  
  story_df[i,3] <- tokenizers::count_sentences(actual_story)
  story_df[i,4] <- tokenizers::count_words(actual_story)
  story_df[i,5] <- tokenizers::count_characters(actual_story)
  story_df[i,6] <- story_df[i,4]/story_df[i,3]
  story_df[i,7] <- story_df[i,5]/story_df[i,4]
  story_df[i,8] <- stringr::str_count(actual_story, pattern = '[!]')
  story_df[i,9] <- stringr::str_count(actual_story, pattern = '[?]')
  
  annot <- udpipe::udpipe_annotate(object = ud_model, x = actual_story)
  annot <- as.data.frame(annot)
  
  story_df[i,10] <- as.integer(table(annot$upos)['PUNCT'])
  story_df[i,11] <- as.integer(table(annot$upos)['ADJ'])
  story_df[i,12] <- as.integer(table(annot$upos)['ADP'])
  story_df[i,13] <- as.integer(table(annot$upos)['ADV'])
  story_df[i,14] <- as.integer(table(annot$upos)['AUX'])
  story_df[i,15] <- as.integer(table(annot$upos)['DET'])
  story_df[i,16] <- as.integer(table(annot$upos)['CCONJ'])
  story_df[i,17] <- as.integer(table(annot$upos)['INTJ'])
  story_df[i,18] <- as.integer(table(annot$upos)['SCONJ'])
  story_df[i,19] <- as.integer(table(annot$upos)['NOUN'])
  story_df[i,20] <- as.integer(table(annot$upos)['PRON'])
  story_df[i,21] <- as.integer(table(annot$upos)['PROPN'])
  story_df[i,22] <- as.integer(table(annot$upos)['VERB'])
  story_df[i,23] <- as.integer(table(annot$upos)['NUM'])
}

# rank of 56 stories according to http://www.bestofsherlock.com/story/pt1ta2.htm
story_df$rank <- c(56,36,49,21,48,28,51,5,29,11,30,16,19,40,34,9,14,25,8,33,7,23,32,50,24,35,27,42,
               22,31,53,52,6,14,45,26,18,38,2,38,44,47,3,17,37,4,10,20,1,54,46,13,12,55,43,41)

# Publication date of all stories
story_df$pub_date <- c('1926-Sep','1924-Oct','1904-Jun','1904-Sep','1892-May','1904-Feb','1926-Oct','1892-Jan','1891-Oct','1908-Dec','1893-Jan',
                       '1904-Mar','1892-Jun','1923-Mar','1893-Jul','1903-Dec','1910-Dec','1913-Nov','1903-Sep','1892-Mar','1893-Dec','1891-Nov',
                       '1893-Apr','1904-Jul','1893-Sep','1891-Sep','1924-Nov','1911-Dec','1917-Sep','1926-Nov','1921-Oct','1904-Aug','1893-May',
                       '1893-Oct','1892-Apr','1903-Oct','1904-Jan','1911-Mar','1891-Aug','1893-Jun','1893-Aug','1926-Dec','1891-Jul','1904-Dec',
                       '1927-Mar','1892-Dec','1904-Apr','1903-Dec','1892-Feb','1893-Mar','1924-Jan','1922-Feb','1891-Dec','1927-Jan','1908-Aug','1893-Feb')

# Collection type of stories
story_df$collection <- c('casebook','casebook','return','return','adventures','return','casebook','adventures','adventures','bow','bow','return',
                     'adventures','casebook','memoirs','return','bow','bow','return','adventures','memoirs','adventures','memoirs','return',
                     'memoirs','adventures','casebook','bow','bow','casebook','casebook','return','memoirs','memoirs','adventures','return',
                     'return','bow','adventures','memoirs','memoirs','casebook','adventures','return','casebook','memoirs','return','return',
                     'adventures','memoirs','casebook','casebook','adventures','casebook','bow','memoirs')

# arranging columns
story_df_final <- data.frame(story_df[,(1:2)], story_df[,(24:26)], story_df[,(3:23)])

# give the appropriate column names
names(story_df_final) <- c('title','abbrv', 'rank', 'pub_date', 'collection', 'nos', 'now', 'noc', 'awps', 'acpw', 'em', 'qm',
                     'punct', 'adj', 'adp', 'adv', 'aux', 'det', 'cconj', 'intj', 'sconj', 'noun', 'pron', 'propn', 'verb', 'num')

# store the data frame into a csv file
write.csv(story_df_final, file = 'K:/Data Science/Sherlock Holmes/SHolmesFeatures.csv')

# store the data frame as R database
saveRDS(story_df_final, file = 'K:/Data Science/Sherlock Holmes/SHolmesFeatures.rds')

# calculate the spending time for make the dataframe
Sys.time()- start_time
# Time difference of 15.75856 mins
