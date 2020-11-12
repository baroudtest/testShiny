library(shiny)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput("var1", 
                   h3("Numeric input"), 
                   value = 4),
      numericInput("var2", 
                   h3("Numeric input"), 
                   value = 1)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      downloadButton("downloadData", "Download")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  x <-reactive({iris[,as.numeric(input$var1)]
  })
  y <-reactive({iris[,as.numeric(input$var2)]
  })
  grapho <- reactive({ plot(x,y)
  })
  
  
  output$distPlot <- renderPlot({
    plot(x(),y())
  })
  
  
  output$downloadData <- downloadHandler(
    
    
    filename = function() {paste("graph", '.png', sep='') },
    content = function(file) {
      
      png(file)
      plot(x(),y())
      dev.off() 
    }
    
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)