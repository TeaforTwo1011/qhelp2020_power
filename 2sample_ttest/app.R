# shiny app for calculating the power for 2 sample t-test
library(shiny)

ui <- fluidPage( # create web page
  titlePanel("2-sample t-test for independent samples"),
  hr(),
#  withMathJax('Two-sample t-test tests the hypothesis $$H_0: \\mu_x-\\mu_y=\\delta=0$$ 
#              where \\(\\mu_x\\) and \\(\\mu_y\\) are the means of each sample'),
  withMathJax('Two-sample t-test tests the hypothesis \\(H_0: \\mu_x-\\mu_y=\\delta=0\\), 
              where \\(\\mu_x\\) and \\(\\mu_y\\) are the means of each sample.
              The present application generates 2000 times a pair of samples based on 
              the parameters that are entered below, and at each iteration performs a t-test and calculates the accompanying p-value. 
              Power is the proportion of times where the test actually gave a signicant result (p<0.05).'),
  hr(),
  h5("Please enter the parameters for your samples"),
  h4("First sample"),
  fluidRow(
    column(2, numericInput("n1", "sample size", "100", width = '100px')),
    column(2, numericInput("mean1", "sample mean", "0", width = '100px')),
    column(2, numericInput("sd1", "standard deviation", "1", width = '150px'))
  ),
  h4("Second sample"),
  fluidRow(
    column(2, numericInput("n2", "sample size", "100", width = '100px')),
    column(2, numericInput("mean2", "sample mean", "0", width = '100px')),
    column(2, numericInput("sd2", "standard deviation", "1", width = '150px'))
  ),
  actionButton("go","Calculate power"),
  h5(""),
  verbatimTextOutput("results"),
  plotOutput("plotResults")
)

#power <- function(sample_size1, sample_size2, mean1, mean2, sd1, sd2
calc_p_values <- function(sample_size1, sample_size2, mean1, mean2, sd1, sd2){
  pval <- replicate(2000, {
    x <- rnorm(sample_size1, mean1, sd1)
    y <- rnorm(sample_size2, mean2, sd2)
    t.test(x, y, mu = 0, var.equal = TRUE)$p.value # variances unknown
  })
# mean(pval < 0.05)
}

server <- function(input, output){
  rv <- eventReactive(
    input$go,
    calc_p_values(input$n1, input$n2, input$mean1, input$mean2, input$sd1, input$sd2)
    )
  output$results <- renderPrint({
    # input$go
    # pwr <- isolate(power(input$n1, input$n2, input$mean1, input$mean2, input$sd1, input$sd2))
    # p_values <- isolate(calc_p_values(input$n1, input$n2, input$mean1, input$mean2, input$sd1, input$sd2))
    p_values <- rv()
    cat("The power calculated based on your input parameters is", mean(p_values < 0.05))
    })

output$plotResults <- renderPlot({
#  plot(main = paste("P-VALUES. Power:", mean(rv() < 0.05)), rv(), ylab = "p-value")
  plot(main = "P-VALUES", rv(), xlab = "Number of iteration", ylab = "p-value")
  abline(h = 0.05, col = "red", lty = "73", lwd = 3)
  })

} # calculate output from user input

shinyApp(ui = ui, server = server)