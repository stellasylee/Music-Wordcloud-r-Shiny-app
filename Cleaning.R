# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("wordcloud2") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes

library(dplyr)

#Import Data
music <- read.csv("https://raw.githubusercontent.com/stellasylee/rShinyProject/master/data/billboard_lyrics_1964-2015.csv")

#Filter the Years 
music <- filter(music, "Year" < 2019)
music <- filter(music, "Year" > 1967)

#Keep to variables we want
music <- slice(music, 1:5) #Year, Rank, Song, Artist, Lyrics
head(music)

#Need a function to interactively create a dataframe (demoFreq) including word and freq in each column
#will be applied inside the server
#Have the default one done beforehand (for all the data)
#Column->unlist? takes a vector and collapses into character string->tally the words in that string

#Move Lyrics into a separate vector

#Experiment with colors
#Experiment with shapes/outlines
#Side-by-side