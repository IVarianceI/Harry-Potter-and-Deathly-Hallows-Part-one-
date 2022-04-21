install.packages("sentimentr")

library(sentimentr)

setwd("C:/Users/km889/Desktop/HP/SA")
scripts <- read.csv("hermione_ron.csv")

sentiment(scripts$Narrative)

scripts <- get_sentences(scripts$Narrative)
sentiment_by(scripts)

sentiment = sentiment(scripts)

summary(sentiment$sentiment)

install.packages("ggplot2")

library(ggplot2)
qplot(sentiment$sentiment,
      geom="histogram",binwidth=0.1,main="Review Sentiment Histogram")

write.csv(sentiment, "C:/Users/km889/Desktop/HP/sentiment2.csv")
