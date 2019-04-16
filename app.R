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
  # 1. non-alphabetic characters
  # 2. numbers
  # 3. punctuation
  music$Lyrics[i]<- str_remove_all(music$Lyrics[i], "[^a-z ]")
  # filter: stop words
  music$Lyrics[i]<- str_replace_all(music$Lyrics[i], "(the )|(you )|( youre )|( you )|( when )|( your )|( it )|( its )|
                                    ( itself )|( what )|( who )|( which )|( this )|( that )|
                                    ( these )|( those )|( am )|( is )|( are )|( was )|( were )|
                                    ( be )|( been )|( a )|( an )|( the )|( and )|( but )|
                                    ( if )|( or )|( because )|( as )|( until )|( while )|
                                    ( of )|( at )|( by )|( for )|( with )|( about )|( to )|
                                    ( then )|( so )|( ill )|( can )|( will )|( get )|( than )"," ")}

#getFreqMatrix: takes selections for artist, decade, and start and end ranks,
#filters the music$Lyrics column accordingly,
#isolates the column of lyrics and concatenates all the character strings into one,
#then creates a frequency table for the relevant words in it
getFreqMatrix<-function(artist, decade, startRank, endRank){
  temp<-filter(music, Decade%in%decade) #keeps cases where the Decade is in the list decade
  if(artist!="Artist") temp<-filter(temp, Artist%in%artists[grep(tolower(artist), music$Artist)]) #checking if artist selection is relevant
  temp<-filter(temp, Rank>=startRank)
  temp<-filter(temp, Rank<=endRank)
  text<-(paste(temp$Lyrics, collapse = ''))
  docs <- Corpus(VectorSource(text))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  as.data.frame(as.table(sort(rowSums(m),decreasing=TRUE)))
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

# Billboard Color Palette (pulled from logo)
pal <- c("black", "#02B845", "#f40506", "#ff9800", "#00b0f4","white")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#

ui <- navbarPage(inverse = TRUE, "LyricsCloud",
                 # First Page - Intro        
                 tabPanel("About the Project",
                          fluidPage(h1("LyricsCloud"),
                                    br(),
                                    p(strong(em("\"I write the songs that make the whole world sing/I write the songs of love and special things
                                                \""))), 
                                    p(strong("Barry Manilow, I Write the Songs (1975)"))),
                          br(),
                          p("For this project, we wanted to provide an rShiny interface for visualizing trends in the lyrics of popular music over time. 
                            By exploring some of the patterns of language use in the music most celebrated and consumed by the US audience, the user can learn about cultural values and movements."),
                          p("Our data deals with songs which were included in the Billboard Hot 100 list for the years between 1965 and 2015. 
                            We organized these by artist, by the decade they were released, and by the rank they reached."),
                          br(),
                          br(),
                          div(img(src="http://people.ischool.berkeley.edu/~kbloom/music/assets/images/bh100.png", height = 239, width = 260), style="text-align: center;"),
                          br(),
                          br(),
                          div(p(strong("Built by"),  "LaAnna Farnelli and Stella Lee"), 
                              p(strong("R Packages:"), "diplyr, ggplot2, readr, RColorBrewer, shiny, stringr, tidygraph, tidyr, tidytext, tidyverse, tm, wordcloud2"),
                              p(strong("Data Sources:"), a("Kaggle.com: Billboard Top 100 1964-2015 Songs, Lyrics"), href = "https://www.kaggle.com/rakannimer/billboard-lyrics?fbclid=IwAR326qyrozIoyPpLPyBtp7sym04ohJXNJSWfoWoqSlyb3LAsFyzzRHfvgH0"),
                              p("See", a("Our GitHub Repository", href = "https://github.com/stellasylee/Music-Wordcloud-r-Shiny-app"), "for more information")
                          )),
                 # Second Page  - Barplot Generator
                 tabPanel("Top Words by Decade",
                          fluidPage(titlePanel("Top Words by Decade"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("histYear", h3("Select Decade:"), 
                                                    choices = list("1965 - 1975" = 1,
                                                                   "1976 - 1985" = 2,
                                                                   "1986 - 1995" = 3,
                                                                   "1996 - 2005" = 4,
                                                                   "2006 - 2015" = 5),
                                                    selected = 1)
                                      ),
                                      mainPanel(
                                        p(strong(em("\"The present now will later be past/The order is rapidly fadin'.
                                                    And the first one now will later be last/for the times they are a-changin'.
                                                    \""))), 
                                        p(strong(" - Bob Dylan, The Times They Are A-Changin' (1963)")),
                                        plotOutput(outputId = "plot"))))),
                 # Third Page  - WordCloud Generator    
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
                                        p(strong(em("\"Sing with me, sing for the year Sing for the laughter, sing for the tear
                                                    \""))), 
                                        p(strong(" - Aerosmith, Dream On (1973)")),
                                        p("Hover over the word cloud below to see their frequency in the Billboard Hot 100 Lyrics for your selection."),
                                        wordcloud2Output("wordcloud", width="100%", height = "565px"))))))


#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#

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
    wordcloud2(v, size = 1.6, fontFamily = "Courier",
               color=rep_len(pal[2:6], nrow(v)), backgroundColor = "black")
  })
  output$quote <-renderText({ 
    switch(as.numeric(input$histYear),
           "1965 - 1975",
           "1976 - 1985",
           "1986 - 1995", 
           "1996 - 2005",
           "2006 - 2015")
  })
  
  output$artist <-renderText({ 
    switch(as.numeric(input$histYear),
           "1965 - 1975",
           "1976 - 1985",
           "1986 - 1995", 
           "1996 - 2005",
           "2006 - 2015")
  }) #fails on more recent artists?
     #successful tests include: "Abba," "elton john," "beatles," "aerosmith," "johnny cash," 
  
  # Make histogram of top 20 frequent words throughout decades
  output$plot <- renderPlot({
    title<-switch(as.numeric(input$histYear),
                  "1965 - 1975", "1976 - 1985", "1986 - 1995", "1996 - 2005", "2006 - 2015")
    barplot(getTop20CommonWords(input$histYear),
            angle = 45, col = c(pal[2], pal[2], pal[2], pal[2], pal[2], pal[3], pal[3], pal[3], pal[3], pal[3], pal[4], pal[4], pal[4], pal[4], pal[4], pal[5], pal[5], pal[5], pal[5], pal[5]), 
            main=title,
            ylab="Frequency in Top 100 Songs",
            xlab="Word",las=2)
  }) 
}

# Run the app ----
shinyApp(ui = ui, server = server)
