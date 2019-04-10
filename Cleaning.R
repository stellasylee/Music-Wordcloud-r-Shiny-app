#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages ("tm")
#install.packages ("wordcloud")
#install.packages ("string")

library(stringr)
library(tidyr)     # contains tools to tidy data
library(ggplot2)   # for plotting
library(readr)     # a package for parsing data
library(dplyr)     # contains functions for data manipulation
library(tm)        # text mining library
library(wordcloud)

#Import Data
music <- read.csv("https://raw.githubusercontent.com/stellasylee/Music-Wordcloud-r-Shiny-app/master/data/billboard_lyrics_1964-2015.csv")

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

#Filter out instrumental songs
music<-filter(music, Lyrics!="instrumental")

#List of Artists used for searching valid artist input
artists<-unique(music$Artist)

# Cleaning the lyrics
music$Lyrics<- as.character(music$Lyrics)
for (i in 1:nrow(music)){
  # filter 
  # 1. non-alphabetic letter and whitespace for distinguishing the words
  # 2. numbers
  # 3. punctuations
  music$Lyrics[i]<- str_remove_all(music$Lyrics[i], "[^a-z ]")
}

#Remove English stopwords
stopwords<-list("it", "its", "itself", "what", "which", "who", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "to","then", "so", "than")

#getFreqMatrix: takes selections for artist, decade, and start and end ranks,
#filters the music$Lyrics column accordingly,
#isolates the column of lyrics and concatenates all the character strings into one,
#then creates a frequency table for the relevant words in it
getFreqMatrix<-function(artist, decade, startRank, endRank){
  temp<-filter(music, Decade%in%decade) #keeps cases where the Decade is in the list decade
  if(artist!="Artist") temp<-filter(temp, Artist%in%artists[grep(tolower(artist), music$Artist)])
  temp<-filter(temp, Rank>=startRank)
  temp<-filter(temp, Rank<=endRank)
  text<-(paste(temp$Lyrics, collapse = ''))
  docs <- Corpus(VectorSource(text))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}
