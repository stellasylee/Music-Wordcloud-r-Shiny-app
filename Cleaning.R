#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("readr")
library(tidyr)     # contains tools to tidy data
library(ggplot2)   # for plotting
library(readr)     # a package for parsing data
library(dplyr)     # contains functions for data manipulation

#Import Data
music <- read.csv("https://raw.githubusercontent.com/stellasylee/Music-Wordcloud-r-Shiny-app/master/data/billboard_lyrics_1964-2015.csv")

#Filter the Years 
#music <- filter(music, "Year" < 2016)
#music <- filter(music, "Year" > 1964)

#Keep to variables we want
#music <- slice(music, 1:5) #Year, Rank, Song, Artist, Lyrics #not working to remove Source
head(music)

#Need a function to interactively create a dataframe (demoFreq) including word and freq in each column
#will be applied inside the server
#Have the default one done beforehand (for all the data)
#Column->unlist? takes a vector and collapses into character string->tally the words in that string

#Move Lyrics into a separate vector

#Create Decades?

#Experiment with colors
#Experiment with shapes/outlines
#Side-by-side
