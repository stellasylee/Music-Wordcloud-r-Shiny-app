library(shiny)
require(wordcloud2, geniusr, tidytext, tidyverse)

ui <- fluidPage(
  titlePanel("LyricsCloud"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose the artist/year/decade")
    ),
    mainPanel()
  )
)

server <- function(input, output){
    
    terms <- reactive({
      
      input$update
      
      isolate({
        withProgress({
          setProgress(message = "Processing...")
          input$selection
        })
      })
    })
    
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"))
    })
  
  shinyApp(ui, server)
  
}

shinyApp(ui=ui, server=server)


