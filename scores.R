library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )

  free <- read.csv(choose.files(), header = TRUE)$Chatcontent
  
  ray <- as.character(free)
  
  mySentiment <- get_nrc_sentiment(ray)
  table <- head(mySentiment)
  table
  

  sentimentTotals <- data.frame(colSums(table[,c(1:6)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
    
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")