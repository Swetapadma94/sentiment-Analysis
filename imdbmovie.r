library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
# IMDB Reviews###
aurl<-"https://www.imdb.com/title/tt6751668/reviews?start="
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"parasite.txt",row.names = F)
parasite<- read.delim('parasite.txt')
str(parasite)
View(parasite)
# Build Corpus and DTM/TDM
library(tm)
corpus <- parasite[-1,]
head(corpus)
class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus)

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus)

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus)

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus)

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset)

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset)


cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))

# Removing the word movie and movies on similar grounds - as unnecessary.
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')


# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset)

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset)

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

# Sentiment Analysis #
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read File 
IMDB_reviews <- read.delim('parasite.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[6]
# on tweet 6, you have 3 for anger,8 for anticipation ,2 for disgust ,4 for fear 
#  4 for joy, each one for sadness and surprise, 8 for trust , 9 words for negative and 10 positive.
get_nrc_sentiment('ridiculous')
#ridiculous has 1 anger 1 disgust and 1 negative
# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for parasite')
