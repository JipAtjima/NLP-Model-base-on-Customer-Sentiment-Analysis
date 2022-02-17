# NLP-Model-base-on-Customer-Sentiment-Analysis
Analysis customer feedback and create NPL model base on sentiment analysis.

#install.packages("lubridate")
install.packages("iconv")
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
install.packages("syuzhet")
library(syuzhet)
library(wordcloud)
install.packages("ggplot2")
library(ggplot2)



#import data into R
movie_review <- read.csv("C:/Users/atjim/Desktop/Portforio/R_project/movie_review.csv")

#check structure of file
str(book_review)

#Creating corpus
#This function uses the base package function iconv to translate value label into specfied,
corpus <- iconv(movie_review$title)
corpus <- Corpus(VectorSource(corpus))

#To see the corpus
inspect(corpus[1:5])
      
#Cleaning corpus
corpus <- tm_map(corpus, tolower)

corpus <-tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords, c("book", "read", "life"))

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

review_final <- corpus

#creat Term document
tdm <- TermDocumentMatrix(review_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]

#Bar plot of words
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w, las = 2, col = 'green')


#create word cloud
w <- sort(rowSums(tdm), decreasing = T)
set.seed(200)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25, "Dark2"),
          scale = c(3,0.3))


#obtain sentiment score
sentiment_data <- iconv(movie_review$title)
s <- get_nrc_sentiment(sentiment_data)

#create new column (neutral)
s$neutral <- ifelse(s$negative+s$positive ==0, 1, 1)
head(s)
sum(s)        
colSums(s)
100*colSums(s)/sum(s)    

#bar plot
barplot(100*colSums(s)/sum(s),
        las = 2,
        col = rainbow(11),
        ylab = "Percentage",
        main = "Centiment Score Demonstration ")






























