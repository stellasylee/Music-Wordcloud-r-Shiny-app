library(shiny)
library (wordcloud2)
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
  
}

# Run the app ----
shinyApp(ui = ui, server = server)