library(shiny)
library(tidyverse)
library(tidyquant)

ui <- fluidPage(

    # Application title
    titlePanel("Sales Estimation"),

     
    sidebarLayout(
        
        sidebarPanel(
            sliderInput("train",
                        "Select Data train %:",
                        min = .1,
                        max = .9,
                        step = .05,
                        value = .8),
            sliderInput("h",
                        "Select forecast Horizon",
                        min = 1,
                        max = 20,
                        step = 1,
                        value = 4)  
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
