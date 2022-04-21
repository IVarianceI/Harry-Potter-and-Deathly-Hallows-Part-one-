install.packages("tidytext")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("tidyr")
install.packages("qdap")
install.packages("reshape2")
install.packages("widyr")
install.packages("tm")
install.packages("textdata")
install.packages("textstem")

library(tidytext)
library(dplyr)
library(magrittr)
library(ggplot2)
library(wordcloud)
library(tidyr)
library(qdap)
library(reshape2)
library(widyr)
library(textdata)
library(tm)
library(textstem)

setwd("C:/Users/km889/Desktop/HP/SA")
scripts <- read.csv("hpdh_sent.csv", stringsAsFactors = FALSE)

str(scripts)

pre_processing_data_frame_fct <- function(scripts) {
  scripts <- tolower(scripts)
  scripts <- gsub('[[:digit:]]','',scripts)
  scripts <- gsub(paste(stopwords('en'), collapse = '\\b|\\b'),'',scripts)
  scripts <- gsub('[[:punct:]]','',scripts)
  scripts <- gsub('\\s+',' ',scripts)
  scripts <- lemmatize_strings(scripts)
  corp <- Corpus(VectorSource(scripts))
  return(corp)
}

my_data_clean <- pre_processing_data_frame_fct(scripts$Narrative)
my_data_clean

my_tdm <- TermDocumentMatrix(my_data_clean)

tidy_frame <- tidy(my_tdm)

#Unigram bar chart
tidy_frame %>%
  count(term, sort = TRUE) %>%
  filter(n > 1) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  geom_col(fill = "#9540FF") +
  ggtitle("Word Frequency - Scrimgeour") +
  geom_text(aes(x = term, label = n), vjust = 0, hjust = -0.3, size = 3)

sentiment_bing <- get_sentiments("bing")
sentiment_nrc <- get_sentiments("nrc")
sentiment_loughran <- get_sentiments("loughran")
sentiment_afinn <- get_sentiments("afinn")

#Positivity word cloud
positivity_wordcloud <- tidy_frame %>%
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  count(term, sentiment, sort = TRUE) %>%
  acast(term ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#177255", "#A32D39"),
                   max.words = 100)

sentiment_nrc <- get_sentiments("nrc")
