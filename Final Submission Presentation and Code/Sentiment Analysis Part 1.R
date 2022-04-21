install.packages("tm")
install.packages("textdata")
install.packages("textstem")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("wordcloud2")

library(dplyr)
library(textdata)
library(tm)
library(textstem)
library(tidytext)
library(wordcloud)
library(wordcloud2)

setwd("C:/Users/km889/Desktop/HP/SA")
scripts <- read.csv("hpdh_sent.csv")

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
head(tidy_frame)
str(tidy_frame)

sentiment_bing <- get_sentiments("bing")
sentiment_bing

sentiment_loughran <- get_sentiments("loughran")
sentiment_loughran

sentiment_afinn <- get_sentiments("afinn")
sentiment_afinn

sentiment_nrc <- get_sentiments("nrc")
sentiment_nrc

sentiment_bing <- sentiment_bing %>% rename(score_bing = sentiment) %>%
  mutate(score_bing = ifelse(score_bing == "negative", -1, 1))

head(sentiment_bing)
tail(sentiment_bing)

unique(sentiment_loughran$sentiment)
sentiment_loughran <- sentiment_loughran %>% rename(score_loughran = sentiment) %>%
  filter(score_loughran %in% c("negative", "positive")) %>% 
  mutate(score_loughran = ifelse(score_loughran == "negative", -1, 1))

unique(sentiment_nrc$sentiment)
sentiment_nrc <- sentiment_nrc %>% rename(score_nrc = sentiment) %>%
  filter(score_nrc %in% c("negative", "positive")) %>% 
  mutate(score_nrc = ifelse(score_nrc == "negative", -1, 1))

head(sentiment_loughran)
tail(sentiment_loughran)

sentiment_afinn <- sentiment_afinn %>% rename(score_afinn = value)
head(sentiment_afinn)
tail(sentiment_afinn)

sentiments <- full_join(sentiment_bing, sentiment_loughran, by = c("word" = "word"))
sentiments <- full_join(sentiments, sentiment_nrc, by = c("word" = "word"))
head(sentiments)
tail(sentiments)

head(tidy_frame)
head(sentiments)

my_sentiments <- left_join(tidy_frame, sentiments, by = c("term" = "word"))
my_sentiments <- my_sentiments %>% arrange(document, term)
head(my_sentiments)
tail(my_sentiments)

scripts <- scripts %>% mutate(document = row_number())
my_sentiments <- my_sentiments %>% mutate(document = as.integer(document))
#my_sentiments <- full_join(my_sentiments, scripts %>% select(document, Sentiment),
by = c("document" = "document"))

#Replace all NAs with 0 

my_sentiments[is.na(my_sentiments)] <- 0 

write.csv(my_sentiments,"C:/Users/km889/Desktop/HP/SA/Approach1/my_sentiments_script.csv")

# Create a word cloud 

cloud_data <- my_sentiments %>% group_by(term) %>% summarise(counts = sum(count))

cloud_data2 <- read.csv("cloud_data2.csv")

head(cloud_data %>% arrange(-counts)) # these are the most common words 
wordcloud(words=cloud_data2$term, freq=cloud_data2$counts, random.order=FALSE, max.words = 100)

wordcloud2(data=cloud_data2, frequency(cloud_data2$counts))
wordcloud2(cloud_data, size=1.6, color='random-dark')
wordcloud2(cloud_data, size=1.6, color=rep_len( c("green","blue"), nrow(cloud_data) ) )

wordcloud(words = cloud_data2$term, freq = cloud_data2$counts, min.freq = 1,
         max.words=200, random.order=FALSE, rot.per=0.35, 
         colors=brewer.pal(8, "Dark2"))


# Calculate the lexicon scores per document per word. 

my_sentiments <- my_sentiments %>% mutate(score_bing=count*score_bing,
                                          score_loughran=count*score_loughran,
                                          score_nrc=count*score_nrc)

head(my_sentiments)

# sum up the scores per document 

my_sentiments <- my_sentiments %>% group_by(document) %>% 
  summarise(sum_score_bing=sum(score_bing),
            sum_score_loughram=sum(score_loughran),
            sum_score_afinn=sum(score_afinn))

head(my_sentiments)

# Simple Pie Chart
slices <- c(165, 156)
lbls <- c("Negative", "Positive")
pie(slices, labels = lbls, main="Sentiment Analysis - Harry")

#Pie Chart with Percentages
slices <- c(165, 156)
lbls <- c("Negative", "Positive")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Sentiment Analysis - {Character}")

