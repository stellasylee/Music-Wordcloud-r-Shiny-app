#Citation: https://shiny.rstudio.com/gallery/word-cloud.html
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("readr")
#install.packages("RColorBrewer") 
#install.packages("shiny")
#install.packages("stringr")
#install.packages("tidygraph")
#install.packages("tidyr")
#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("tm")
#install.packages("visNetwork")
#install.packages("wordcloud")
#install.packages("wordcloud2")
library(dplyr)     
library(ggplot2)   
library(readr)     
library(RColorBrewer)
library(shiny)
library(stringr)
library(tidygraph)
library(tidyr)     
library(tidytext)
library(tidyverse)
library(tm)        
library(visNetwork)
library(wordcloud)
library(wordcloud2)

#-------------------------------------------------------------------------------------------------------------------#
#                                         DATA WRANGLING & CLEANING                                                 #
#-------------------------------------------------------------------------------------------------------------------#

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

#Save a list of unique artists and artist combos
artists<-unique(music$Artist)

# Cleaning the lyrics
music$Lyrics<- as.character(music$Lyrics)
for (i in 1:nrow(music)){
  # filter 
  # 1. non-alphabetic letter and whitespace for distinguishing the words
  # 2. numbers
  # 3. punctuations
  music$Lyrics[i]<- str_remove_all(music$Lyrics[i], "[^a-z ]")
  # filter: stop words
  music$Lyrics[i]<- str_replace_all(music$Lyrics[i], "(the )|( youre )|( when )|( your )|( it )|( its )|
                                                  ( itself )|( what )|( who )|( which )|( this )|( that )|
                                                   ( these )|( those )|( am )|( is )|( are )|( was )|( were )|
                                                      ( be )|( been )|( a )|( an )|( the )|( and )|( but )|
                                                      ( if )|( or )|( because )|( as )|( until )|( while )|
                                                      ( of )|( at )|( by )|( for )|( with )|( about )|( to )|
                                                    ( then )|( so )|( than )"," ")}

#getFreqMatrix: takes selections for artist, decade, and start and end ranks,
#filters the music$Lyrics column accordingly,
#isolates the column of lyrics and concatenates all the character strings into one,
#then creates a frequency table for the relevant words in it
#used to generate wordclouds
getFreqMatrix<-function(artist, decade, startRank, endRank){
  temp<-filter(music, Decade%in%decade) #keeps cases where the Decade is in the list decade
  if(artist!="Artist"){
    if(length(grep(tolower(artist), music$Artist))<1) #if the artist doesn't appear in our data, it ignores that input
  temp<-filter(temp, Artist%in%artists[grep(tolower(artist), music$Artist)])
  }
  temp<-filter(temp, Rank>=startRank)
  temp<-filter(temp, Rank<=endRank)
  text<-(paste(temp$Lyrics, collapse = ''))
  docs <- Corpus(VectorSource(text))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  sort(rowSums(m),decreasing=TRUE)
}

#getTop20CommonWords: takes in decade,
#creates frequency table for most popular words 
#used to generate barplots
getTop20CommonWords <- function (decade){
  temp <- filter (music, music$Decade == decade)
  text<-(paste(temp$Lyrics, collapse = ''))
  docs <- Corpus(VectorSource(text))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  head((sort(rowSums(m),decreasing=TRUE)), 20)
}

# Billboard Color Palette
pal <- c("black", "#02B845", "#f40506", "#ff9800", "#00b0f4","white")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#
# Define UI----
ui <- navbarPage(inverse = TRUE, "LyricsCloud",
                 # First Page - Intro        
                 tabPanel("Intro",
                          fluidPage(h1("Our Project"),
                                    br(),
                                    p(strong(em("\"A cool song quote...\""), "Source Song and Artist")),
                                    br(),
                                    p("A paragraph explanation"),
                                    p("Another paragraph explanation"),
                                    br(),
                                    br(),
                                    div(img(src="http://people.ischool.berkeley.edu/~kbloom/music/assets/images/bh100.png", height = 239, width = 260), style="text-align: center;"),
                                    br(),
                                    br(),
                                    div(p(strong("Built by"),  "LaAnna Farnelli and Stella Lee"), 
                                        p(strong("R Packages:"), "tidyverse, tidytext, wordcloud2, tidygraph, vizNetwork, glue."),
                                        p(strong("Data Sources:"), "INSERT LATER"),
                                        p("See", a("Our GitHub Repo", href = "https://github.com/stellasylee/Music-Wordcloud-r-Shiny-app"), "for more information")
                                    ))),
                 
                 # Second Page  - WordCloud Generator    
                 tabPanel("WordCloud Generator",
                          fluidPage(titlePanel("Wordcloud for Billboard Chart Top 100"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        textInput("artist", "Type an artist:",
                                                  value = "Artist"),
                                        actionButton("update", "Change"),
                                        checkboxGroupInput("year", h3("Select your Decade(s):"),
                                                           choices = list("1965 - 1975" = 1,
                                                                          "1976 - 1985" = 2,
                                                                          "1986 - 1995" = 3,
                                                                          "1996 - 2005" = 4,
                                                                          "2006 - 2015" = 5),
                                                           selected = c (1,2,3,4,5)),
                                        sliderInput("rank", h3("Rank selections:"),
                                                    min = 1, max = 100, value = c(1,100))
                                      ),
                                      mainPanel(
                                        p(strong(em("\"...another song quote.\""), "Reference song and artist")),
                                        p("Want to explore? Hover over the word cloud below...give directions here"),
                                        wordcloud2Output("wordcloud", width="100%", height = "565px"))))),
                 # Third Page  - Barplot Generator
                 tabPanel("Top 20 wordsfrom Different Decades",
                          fluidPage(titlePanel("Decades Comparison"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("histYear", h3("Select Decade:"), 
                                                    choices = list("1965 - 1975" = 1,
                                                                   "1976 - 1985" = 2,
                                                                   "1986 - 1995" = 3,
                                                                   "1996 - 2005" = 4,
                                                                   "2006 - 2015" = 5),
                                                    selected = 1)),
                                      mainPanel(
                                        p(strong(em("\"...another song quote.\""), "Reference song and artist")),
                                        plotOutput(outputId = "plot"))
                                    )
                          )
                 )
)
#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#
# Define server logic ----
server <- function (input,output){
  # Define a reactive expression for the document term matrix
  terms <- reactive ({
    #Change when the "update" button is pressed
    input$update
    # but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        getFreqMatrix(input$artist, input$year, input$rank[1], input$rank[2])
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  output$wordcloud <- renderWordcloud2({
    v <- terms()
    word_counts<-as.data.frame(as.table(v))
    wordcloud2(word_counts, size = 1.6, fontFamily = "Courier",
               color=rep_len(pal[2:4], nrow(word_counts())), backgroundColor = "black")
  })
  
  # Make histogram of top 20 frequent words throughout decades
  output$plot <- renderPlot({
    barplot(getTop20CommonWords(input$histYear))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
