

#

library(shiny)

# Define UI for application that draws a plot of the waning functions
ui <- fluidPage(
    # Application title
    titlePanel("Vaccine-derived Immunity Waning by dose timing"),
    
    # Sidebar with a slider input for time of peak first dose protection
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "FirstPeak",
                "Peak timing of response to First Dose",
                min = 0,
                max = 2,
                value = 1
            ),
            sliderInput(
                "FirstDuration",
                "Duration of Immunity from First Dose",
                min = 1,
                max = 4,
                value = 2
                
            ),
            sliderInput(
                "SecondPeak",
                "Peak timing of second dose response",
                min=1,
                max=4,
                value=1
            ),
            sliderInput(
                "MaxFirst",
                "Maximum immune response from first dose",
                min = 1,
                max = 2,
                value = 1
            ),
            sliderInput(
                "MaxSecond",
                "Maximum immune response from second dose",
                min = 2,
                max = 4,
                value = 2
            )
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("wanePlot"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$wanePlot <- renderPlot({
        t <- seq(0, input$FirstDuration, 0.001)
        Response1 <- -(t - input$FirstPeak) ^ 4 + input$MaxFirst
        Response2 <- -(t-input$SecondPeak) ^ 4 + input$MaxSecond
          wanePlot<-
    })
}

# Run the application
shinyApp(ui = ui, server = server)
