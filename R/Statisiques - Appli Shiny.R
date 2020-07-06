library(shiny)


# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("STATISTIQUES"),
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "Choose a dataset:",
                        choices = c("ecrin", "boulangerie")),
            
            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10),

            selectInput(inputId = "variable", label = "variable",choices=NULL),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
            
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            tableOutput("view"),
            
            plotOutput("distPlot"))
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({switch(input$dataset,
                                     "ecrin" = ecrin,
                                     "boulangerie" = data)
    })
    
    observe({
        choices1 = colnames(datasetInput())
        updateSelectInput(session, "variable", choices = choices1)
    })

    #datavar<-reactive({input$variable})
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the first "n" observations ----
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        dataset <- datasetInput()
        x <- dataset[input$variable]
        #print(x)
        x1 <-as.numeric(unlist(x))
    
        bins <- seq(min(x1,na.rm=TRUE), max(x1,na.rm=TRUE), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x1, main=paste("Histogramme de",input$variable),breaks = bins, col = 'darkgray', border = 'white')
    })
    
    
}

shinyApp(ui = ui, server = server)