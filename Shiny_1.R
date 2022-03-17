#Trying for one test: binomial
library(shiny)

ui <- fluidPage(
  titlePanel("Inferring the power of a test"),
  sliderInput("n", "Sample size", 0, 10000, 100),
  sliderInput("k1", "Number of replications", 0, 6000, 100),
  sliderInput("k2", "Minimum relevant effect", 0, 1, 0.1),
  textOutput("power")
)

function1<-function(k1, k2, k3){
  pval <- replicate(k1, { # replications of experiment
    x <- rbinom(1, size = k2, # data-generating model with
                prob = k3) # minimum relevant effect
    binom.test(x, n = k2, p = 0.5)$p.value # p-value of test against H0
  })
  mean(pval < 0.05)
}

server <- function(input, output){
  output$power<-reactive({
    function1(input$k1, input$n,input$k2)
  })
}

shinyApp(ui, server)
