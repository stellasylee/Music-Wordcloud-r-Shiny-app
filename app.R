#Citation: https://shiny.rstudio.com/gallery/word-cloud.html
#install.packages("wordcloud")
library(shiny)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(wordcloud2)
#library(glue)
library(visNetwork)

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
                  min = 1, max = 100, value = c(1,100)),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 150, value = 70),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1, max = 300, value = 100)
    ),
    mainPanel(
      p(strong(em("\"...another song quote.\""), "Reference song and artist")),
      p("Want to explore? Hover over the word cloud below...give directions here"),
      plotOutput("plot", width="100%", height = "565px"))))))
  


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
  #wordcloud_rep <- repeatable(wordcloud())
  output$plot <- renderPlot({
    v <- terms()
    wordcloud(names(v), v, scale = c(8, .2),
              min.freq = input$freq, max.words = input$max, rot.per = 0.35)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
