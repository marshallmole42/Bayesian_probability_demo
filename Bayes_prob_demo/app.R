#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

likelihood_fun <- function(p, nhit, ntrial) {
    return(p^nhit*(1-p)^(ntrial-nhit)*choose(ntrial, nhit))
}


seq_length = 1001
xrange = seq(from=0, to=1, length.out=seq_length)

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
                c("Uniform"="u", "Parabolic"="p", "Current posterior (update)"="cp")
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
           #tableOutput("priordata"),
           plotOutput("likelihoodPlot"),
           plotOutput("posteriorPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #priordata = reactive({tibble(xx=seq(from=0, to=1, length.out=seq_length), prior=ifelse(input$priorType=="u", yes=seq(from=0, to=1, length.out=seq_length), no=seq(from=0, to=1, length.out=seq_length)))})
    
    #output$priordata <- renderDataTable({priordata()})
    
    output$priorPlot <- renderPlot({
        # make the prior plot
        priordata = tibble(xx=xrange, p=ifelse(c(rep(input$priorType=="u", seq_length)), yes=rep(1, length.out=seq_length), no=(1-4*(xrange-0.5)^2)))
        priordata = priordata %>% mutate(prior=p/sum(p)*seq_length)
        ggplot(data=priordata) + geom_line(aes(x=xx, y=prior)) + scale_y_continuous(name="probability density", limits=c(0,2)) + ggtitle("Prior")
    })
    
    output$likelihoodPlot <- renderPlot({
        likelidata = tibble(xx=xrange, l=likelihood_fun(xrange, input$nhit, input$ntrial))
        ggplot(data=likelidata) + geom_line(aes(x=xx, y=l)) + ggtitle("Likelihood")
    })
    
    output$posteriorPlot <- renderPlot({
        # make the posterior plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
