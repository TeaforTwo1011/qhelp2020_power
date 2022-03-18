library(shiny)

# source(helpers.r)
source("simpleReg.r")

server <- function(input, output){

    sim <- reactive(getPower(simpleReg_fit(n = input$n, alpha = input$alpha, beta = input$beta, err = input$err, x_mean = input$x_mean, x_sd = input$x_sd)))
    output$simpleReg_power <- renderText(paste0("The power is: ", sim()[[1]], " (sd = ", sim()[[2]], ")"))
    output$simpleReg_pplot <- renderPlot(plot(sim()[[3]], ylab = "p-values"))
}
