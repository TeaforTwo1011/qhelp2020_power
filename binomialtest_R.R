#Trying for one test: binomial
library(shiny)

ui <- fluidPage(
fluidRow(
column(10, offset=5,titlePanel("Binomial test")),
column(3, offset=2,wellPanel(numericInput("n", "Sample size",value=100,min=0, max=10000, step=10))),
column(3, offset=2, wellPanel(numericInput("k2", "Minimum relevant effect", value=0.1, min=0, max=1, step=0.01))),
column(2, offset=5, actionButton("goButton", "Go!")),
column(8, offset=5,textOutput("power")),
column(8, offset=2, plotOutput("distribution"))
)
)


#Binomial test
function1<-function(k2, k3){
  pval <- replicate(2000, { # replications of experiment
    x <- rbinom(1, size = k2, # data-generating model with
                prob = k3) # minimum relevant effect
    binom.test(x, n = k2, p = 0.5)$p.value # p-value of test against H0
  })
}




server <- function(input, output){
  rv<-eventReactive(input$goButton, function1(input$n,input$k2))
  output$power<-renderPrint({
    data2<-rv()<0.05
    input$goButton
    isolate(cat("The power of this test is",mean(data2)))
    
  })
  output$distribution<-renderPlot({
    data<-rv()
    input$goButton
    #stripchart(function2(input$n,input$k3,input$k4), pch=16, vertical=TRUE, method = "jitter", xlab="Index",ylab="p values", log="y")
    isolate(plot(main="p-values", data, xlab="Index", ylab="p-values"))
    abline(h=0.05, col="red", lty="dashed", lwd=4)
  })
}

shinyApp(ui, server)

shinyApp(ui, server)
