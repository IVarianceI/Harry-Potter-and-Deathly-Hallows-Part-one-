## Social network analysis with Harry Potter Script
## Jong Shin - MIS612

## Install packages
install.packages("dplyr")
install.packages("tm")
install.packages("textstem")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("textdata")
install.packages("tidyverse")
install.packages("ggplot2")


#load libraries
library(dplyr)
library(tm) #provide list of stop words
library(textstem) #helps with stemming and lemmatization
library(tidytext)
library(wordcloud)
library(textdata)
library(ggplot2)


#set the working directory
setwd("C:/Users/DevPro/Dropbox/Drexel/610/R scripts/612")
#read file
text_emotion <- read.csv("HarryPotter-team7-org.csv")

str(text_emotion)

#create a function that performs all the pre-processing
pre_processing_data_frame_fct <- function(text_column){
  
  text_column <- tolower(text_column)
  text_column <- gsub('[[:digit:]]','',text_column)
  text_column <- gsub(paste(stopwords('en'), collapse='\\b|\\b'),'',text_column)
  text_column <- gsub('[[:punct:]]','',text_column)
  text_column <- gsub('\\s+',' ',text_column)
  text_column <- lemmatize_strings(text_column)
  corp <- Corpus(VectorSource(text_column))
  
  return(corp)
  
}

##Apply the pre-processing function to clean Dialogue
my_data_clean <- pre_processing_data_frame_fct(text_emotion$Dialogue)
my_data_clean


#transform the clean data into a term document matrix
my_tdm <- TermDocumentMatrix(my_data_clean)

tidy_frame <- tidy(my_tdm)


#dictionary(lexicons)
#bing
sentiment_bing <- get_sentiments("bing")

#loughran
sentiment_loughran <- get_sentiments("loughran")

#afinn
sentiment_afinn <- get_sentiments("afinn")

#change sentiments to scores
sentiment_bing <- sentiment_bing %>% rename(score_bing = sentiment) %>% mutate(score_bing = ifelse(score_bing == "negative", -1, 1))
head(sentiment_bing)

unique(sentiment_loughran$sentiment)
sentiment_loughran <- sentiment_loughran %>% rename(score_loughran = sentiment) %>% filter(score_loughran %in% c("negative","positive")) %>% mutate(score_loughran = ifelse(score_loughran == "negative", -1, 1))
head(sentiment_loughran)

sentiment_afinn <- sentiment_afinn %>% rename(score_afinn = value)
head(sentiment_afinn)

##Join: put the scores from all 3 dictionaries into 1 data frame
sentiments <- full_join(sentiment_bing, sentiment_loughran, by=c("word"="word"))
sentiments <- full_join(sentiments, sentiment_afinn, by=c("word"="word"))
#sentiments <- sentiment_bing

#merge the tidy_frame and sentiment data sets
my_sentiments <- left_join(tidy_frame, sentiments, by=c("term"="word"))
my_sentiments <- my_sentiments %>% arrange(document, term)
head(my_sentiments)

#Change NA to 0
my_sentiments[is.na(my_sentiments)] <- 0


#calculate the lexicon scores per document per word
my_sentiments <- my_sentiments %>% mutate(score_bing = count * score_bing, score_loughran = count * score_loughran, score_afinn = count * score_afinn)
head(my_sentiments)

#sum lexicon scores by documents
my_sentiments_groupby <- my_sentiments %>% group_by(document) %>% summarise(sum_score_bing=sum(score_bing), sum_score_loughran=sum(score_loughran),sum_score_afinn=sum(score_afinn))
#head(my_sentiments_groupby)

#sum all lexicon score
my_sentiments_groupby2 <- my_sentiments_groupby %>% mutate(sum_score_bing, sum_score_loughran, sum_score_afinn, sum_all_score=sum_score_bing+sum_score_loughran+sum_score_afinn)
my_sentiments_groupby2$document = as.numeric(my_sentiments_groupby2$document)
my_sentiments_groupby2 <- my_sentiments_groupby2 %>% arrange(document, sum_all_score)
head(my_sentiments_groupby2)


#Plot sum of all lexicon scores - x axis is document progress
#Bar chart for overall story lines
ggplot(my_sentiments_groupby2, mapping=aes(document, sum_all_score, fill=sum_all_score)) +
  geom_bar(alpha = 10, stat = "identity", show.legend = FALSE) +
  labs(x = "Documents", y = "Average Sentiments", title="Sentiment Progress", subtitle = "Harry Potter and the Deathly Hallows Part One")

#Chart only of smooth() average line 
ggplot(my_sentiments_groupby2, mapping=aes(document, sum_all_score)) +
  geom_smooth() +
  labs(x = "Documents", y = "Average Sentiments", title="Sentiment Progress", subtitle = "Harry Potter and the Deathly Hallows Part One")

#Bar chart with smooth() average line 
ggplot(my_sentiments_groupby2, mapping=aes(document, sum_all_score)) +
  geom_bar(alpha = 10, stat = "identity", show.legend = FALSE) +
  geom_smooth() +
  labs(x = "Documents", y = "Average Sentiments", title="Sentiment Progress", subtitle = "Harry Potter and the Deathly Hallows Part One")






