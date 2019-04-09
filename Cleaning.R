#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages ("tm")
#install.packages ("wordcloud")

library(tidyr)     # contains tools to tidy data
library(ggplot2)   # for plotting
library(readr)     # a package for parsing data
library(dplyr)     # contains functions for data manipulation
library(tm)        # text mining library
library(wordcloud)

#Import Data
music <- read.csv("https://raw.githubusercontent.com/stellasylee/Music-Wordcloud-r-Shiny-app/master/data/billboard_lyrics_1964-2015.csv")

#Filter the Years (unnecessary for this dataset)
#music <- filter(music, "Year" < 2019)
#music <- filter(music, "Year" > 1967)

#Remove "Source" column:
music <- music[,-6]

#Rename "Song" column to more descriptive "Title"
names(music)[2]<-"Title"

#Create "Decade" column
music$Decade<-5
music$Decade[music$Year<2005]<-4
music$Decade[music$Year<1995]<-3
music$Decade[music$Year<1985]<-2
music$Decade[music$Year<1975]<-1

###CLEANING THE TEXT
text<-(paste(music$Lyrics, collapse = ''))
# Make into a corpus so we can use tm commands
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

##List of Artists
artists<-unique(music$Artist)

#getFreqMatrix: takes selections for artist, decade, and start and end ranks,
  #filters the music$Lyrics column accordingly,
  #and calls makeCloud to make a wordcloud from the filtered column
getFreqMatrix<-function(artist, decade, startRank, endRank){
  temp<-filter(music, Decade%in%decade) #keeps cases where the Decade is in the list decade
  temp<-filter(music, Artist%in%(grep(tolower(artist), music$Artist)))
  temp<-filter(music, Rank>=startRank)
  temp<-filter(music, Rank<=endRank)
  makeCloud(temp)
}

#makeCloud: takes a vector of character strings, 
  #concatenates them into one character string, 
  #and makes a wordcloud matrix from it.
makeCloud <- function(lyricsCol) {
text<-(paste(lyricsCol, collapse = ''))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
return(d)
}

newmusic<-filter(music, "Year" > 2012)

makeCloud(newmusic$Lyrics)
