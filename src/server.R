library(shiny)
library(shinydashboard)
library(rworldmap)
library("shinythemes")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("plotrix")

shinyServer(function(input, output) {
  
  output$table1 <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
   free <- read.csv(inFile$datapath)
  docs <- Corpus(VectorSource(free))
  inspect(docs)
  #0onclick("btn1",)
  
  # perfom text transformation.
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  #Perfom  Data Cleaning in General
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  #Building a document term matrix.
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  output$table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
    
    docs <- Corpus(VectorSource(free))
    inspect(docs)
    
    # perfom text transformation.
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    #Perfom  Data Cleaning in General
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    #Building a document term matrix.
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 30)
  })
  
  output$Wordcloud <- renderPlot({
    
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  })
  output$freq <- renderPlot({
    
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
            col =topo.colors(10), main ="Most frequent words",
            ylab = "Word frequencies")
    
  })
  output$table1 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
  })
  
  output$summary <- renderPrint({
    summary(free)
  })
  
  sPDF <- joinCountryData2Map(free, joinCode='NAME', nameJoinColumn='Country')
  
  # Generate the Map 
  output$mPlot<- renderPlot({
    mapCountryData(sPDF, nameColumnToPlot = "Country"
                   ,catMethod = 'categorical',
                   mapTitle = 'CLIENT ORIGINS',
                   colourPalette='palette',
                   oceanCol = "light blue",
                   missingCountryCol = "white",
                   # addLegend=F
                   
                   identifyCountries(getMap()
                                     ,nameColumnToPlot="Country")
                   
    )
    
  })
  
  output$bar<-renderPlot({
    ace<- plot(free$Operator, main="MOST ACTIVE OPERATOR", ylab="Frequency", xlab="Operator Names", las=2, names.arg=d[0,]$Operator, col =topo.colors(10))
    ace
  })
  output$distPlot <- renderPlot({
    
    mytable <- table(free$Vote.status)
    lbls <- paste(names(mytable))
    pie(mytable, labels = lbls,
        main="VOTE STATUS OF CLIENTS", radius = 1)
  })
  output$yella<-renderPlot({
    ace<- plot(free$Country, main="CUSTOMER DISTRIBUTION", ylab="CUSTOMERS", xlab="COUNTRY", las=2, names.arg=d[0,]$Country, col =topo.colors(10))
    ace
  })
  output$WWT <- renderPrint({
    maxtime <- max(free $ Wait.time)
    print(maxtime)
    
  })
  output$DWWT <- renderTable({
    retvalmax <- subset(free, Wait.time == max(Wait.time))
    print(retvalmax) 
    
  })
  output$BWT <- renderPrint({
    minitime <- min(free $ Wait.time)
    print(minitime)
    
  })
  output$DBWT <- renderTable({
    retvalmini <- subset(free, Wait.time == min(Wait.time))
    print(retvalmini)
    
  })
  output$AWT <- renderPrint({
    average_waiting_time <- sum(free$Wait.time)/nrow(free)
    print(average_waiting_time)
    
  })
  
  output$table3 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
  
    
    
    ray <- as.character(free$Chatcontent)
    
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
  })

  })
})