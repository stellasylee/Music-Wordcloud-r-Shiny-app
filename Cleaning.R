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

##List of Artists
artists<-unique(music$Artist)

#Remove non-letter characters which would cause an error in freqMatrix function
str_replace_all(music$Lyrics, "^[abcdefghijklmnopqrstuvwxyz ]", " ")


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
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}
