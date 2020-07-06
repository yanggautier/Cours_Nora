library(shiny)

ecrin <- read.table("http://pbil.univ-lyon1.fr/R/donnees/ecrin.txt", h = TRUE)
data <- read.table(file="C:\\Users\\yangg\\Python\\Cours Nora\\R\\boulangerie.csv", sep =";", h = TRUE)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title
  titlePanel("Number of oberserved bird species"),
  
  # Sidebar layout with input for bumber of bins
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot")
      verbatimTextOutput('summary'),
      
      #Output: HTML table with requested number of oberservations --
      tableOutput('view')
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Return the requested dataset --
  datasetInput <- reactive({
    switch (input$dataset,
      "ecrin" = ecrin,
      "boulangerie" = data
    )
  })

  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  output$distplot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- ecrin$RIC
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    
    hist(x, breaks = bins, col = "#75AADB", border = "white"
         
         #,xlab = "Waiting time to next eruption (in mins)",
         #main = "Histogram of waiting times"
         )
  })
}

shinyApp(ui = ui, server = server)

