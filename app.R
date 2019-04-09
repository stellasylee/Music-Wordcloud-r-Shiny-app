#Citation: https://shiny.rstudio.com/gallery/word-cloud.html
library(shiny)
library (wordcloud)
install.packages("wordcloud")
#decade artist rank 
# Define UI----
ui <- fluidPage(
  titlePanel("Wordcloud for Billboard Chart Top 100"),
  sidebarLayout(
    sidebarPanel(
      textInput("artist", "Type an artist:",
                value = "Artist"),
      actionButton("update", "Change"),
      checkboxGroupInput("year", h3("Select decades:"),
                         choices = list("1965 - 1975" = 1,
                                        "1976 - 1985" = 2,
                                        "1986 - 1995" = 3,
                                        "1996 - 2005" = 4,
                                        "2006 - 2015" = 5),
                         selected = c (1,5)),
      sliderInput("rank", h3("Rank selections:"),
                  min = 1, max = 100, value = c(1,100)),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server <- function (input,output){
  # Define a reactive expression for the document term matrix
  terms <- reactive ({
    #Change when the "update" button is pressed
    input$update
    # but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$artist, input$year, input$rank)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud())
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale = c(4, 0.5),
               min.freq = input$freq)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
