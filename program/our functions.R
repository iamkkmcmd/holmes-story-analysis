# ------------------- Negation Contraction -------------------------------------
# In english when making a negation contraction, we omit the 'o' in 'not' 
# and replace it with an apostrophe (Some of these is given below)

negation_contraction <- function(document){
  document <- gsub(pattern = "don't    ", replacement = "do not", document)
  document <- gsub(pattern = "doesn't  ", replacement = "does not", document)
  document <- gsub(pattern = "aren't   ", replacement = "are not", document)
  document <- gsub(pattern = "isn't    ", replacement = "is not", document)
  document <- gsub(pattern = "didn't   ", replacement = "did not", document)
  document <- gsub(pattern = "haven't  ", replacement = "have not", document)
  document <- gsub(pattern = "hadn't   ", replacement = "had not", document)
  document <- gsub(pattern = "shouldn't", replacement = "should not", document)
  document <- gsub(pattern = "wouldn't ", replacement = "would not", document)
  document <- gsub(pattern = "won't    ", replacement = "will not", document)
  document <- gsub(pattern = "can't    ", replacement = "can not", document)
  document <- gsub(pattern = "weren't  ", replacement = "were not", document)
  document <- gsub(pattern = "wasn't   ", replacement = "was not", document)
  document <- gsub(pattern = "needn't  ", replacement = "need not", document)
  document <- gsub(pattern = "oughtn't ", replacement = "enough not", document)
  document <- gsub(pattern = "couldn't ", replacement = "could not", document)
  document <- gsub(pattern = "shan't   ", replacement = "shall not", document)
  document <- gsub(pattern = "mayn't   ", replacement = "may not", document)
  document <- gsub(pattern = "hasn't   ", replacement = "has not", document)
  document <- gsub(pattern = "ain't    ", replacement = "am not", document)
}

# ------------------- Custom Theme -------------------------------------
# Custom theme for plot with Bookman Old Style font
custom_theme <- function(){
loadfonts(device = "win")
theme_bw()+
  theme(text = element_text(family = 'Bookman Old Style'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 12),
        plot.subtitle = element_text(hjust = 0.5, face = 'italic', size = 10))
}


