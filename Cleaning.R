#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("readr")
install.packages ("tm")
#install.packages ("wordcloud")
#install.packages ("RColorBrewer")

library(tidyr)     # contains tools to tidy data
library(ggplot2)   # for plotting
library(readr)     # a package for parsing data
library(dplyr)     # contains functions for data manipulation
library(tm)
library(wordcloud)
library(RColorBrewer)

#Import Data
music <- read.csv("https://raw.githubusercontent.com/stellasylee/Music-Wordcloud-r-Shiny-app/master/data/billboard_lyrics_1964-2015.csv")

#Filter the Years 
#music <- filter(music, "Year" < 2019)
#music <- filter(music, "Year" > 1967)

#Remove "Source" column:
music <- music[,-6]

#Rename "Song" column to more descriptive "Title"
names(music)[2]<-"Title"

dim(music)

#Experiment with colors
#Experiment with shapes/outlines
#Side-by-side

#Need a function to interactively create a dataframe (demoFreq) including word and freq in each column

makeCloud <- function(lyricsCol) {
text<-(paste(lyricsCol, collapse = ''))
docs <- Corpus(VectorSource(text))
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
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
}

newmusic<-filter(music, "Year" > 2012)

makeCloud(newmusic$Lyrics)

#will be applied inside the server
#Have the default one done beforehand (for all the data)
#Column->unlist? takes a vector and collapses into character string->tally the words in that string

#Move Lyrics into a separate vector

#Create Decades?
