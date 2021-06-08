#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


likelihood_fun <- function(p, nhit, ntrial) {
    return(p^nhit*(1-p)^(ntrial-nhit)*choose(ntrial, nhit))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bayesian estimate of binary outcome probability"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "priorType",
                "Choose a prior: ",
                c("Uniform"="u", "Cauchy"="c", "Current posterior (update)"="cp")
            ), 
            numericInput(
                "ntrial", 
                "Number of total observations: ",
                5
            ),
            numericInput(
                "nhit", 
                "Number of target event occurrence: ",
                0
            ),
            actionButton("updatePosterior", "Compute/Update")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("priorPlot"),
           plotOutput("posteriorPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$priorPlot <- renderPlot({
        # make the prior plot
    })
    
    output$posteriorPlot <- renderPlot({
        # make the posterior plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
