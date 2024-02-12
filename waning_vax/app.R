
#

library(shiny)

# Define UI for application that draws a plot of the waning functions
ui <- fluidPage(

    # Application title
    titlePanel("Vaccine-derived Immunity Waning by dose timing"),

    # Sidebar with a slider input for time of peak first dose protection
    sidebarLayout(
        sidebarPanel(
            sliderInput("peak",
                        "Peak first dose immune timing:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput(
               
            )
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
        bins <- seq(min(x), max(x), length.out = input$peak + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
