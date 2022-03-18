library(shiny)

ui <- fluidPage(
fluidRow(
column(10, offset=5,titlePanel("One sample z-test")),
column(3, offset=1, wellPanel(numericInput("n", "Sample size", 1,min=0, max=2000, step=10))),
  #numericInput("k2", "Minimum relevant effect", min=0, max=1, step=0.01),
column(3, offset=1, wellPanel(numericInput("k3", "Mean:", 0.1, min=0, max=100000, step=1))),
column(3, offset=1, wellPanel(numericInput("k4", "Standard deviation:", 0.2,min=0, max=50, step=0.1))),
column(2, offset=5, actionButton("goButton", "Go!")),
column(8, offset=5, textOutput("power")),
column(8, offset=2, plotOutput("distribution"))
)
)

#one Sample Z-test
function2<-function(k2,k3,k4){
  pval <- replicate(2000, {
    x <- rnorm(k2, mean =k3 , sd = k4)
    z <- (mean(x) - 1000)/7.5*sqrt(k2) # test statistic, variance known
    2*pnorm(-abs(z)) # p-value
  })
  
}

?stripchart
server <- function(input, output){
  #output$power<-reactive({
  #  mean(function2(input$n, input$k3,input$k4)<0.05)
  #})
  rv<-eventReactive(input$goButton, function2(input$n,input$k3,input$k4))
  
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
