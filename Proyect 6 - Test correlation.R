library(shiny)

ui <- fluidPage(
  titlePanel("Test of Correlation"),
  withMathJax(),
  "Power (also called true positive rate, hit rate, sensitivity, or recall) is defined as $$1 - \\beta$$
  It is the probability of a statistical test to detect an efect of a given size. Therefore, designing an
  experiment to have a good chance to find an efect means making sure its power is high enough.
  High power is a necessary condition for valid inference. A high power value can be
  considered when the proportion of significant results is about 0.8 or higher.",
  hr(),
  fluidRow(
  column(2, numericInput("n", "Number of observations", 60, min = 10, max = 500, step = 1)),
  column(2, offset = 1, numericInput("s", "Standard deviation:", 15, min = 1, max = 100, step = .1)),
  column(2, offset = 1, numericInput("r", "Effect size:", .3, min = 0, max = 1, step = .01)),
  column(2, offset = 1, numericInput("rho0", "Null Hypothesis (H0):", .6, min = 0, max = 1, step = .01)),
  column(2, offset = 5, actionButton("goButton", "Calculate power")),
  column(10, offset = 0, verbatimTextOutput("results")),
  plotOutput("plotResults")
  )
)

corr <- function(n, s, r, rho0){
  pval <- replicate(2000, {
    sxy <- r*s*s
    x <- MASS::mvrnorm(n, mu = c(100, 100),
                       Sigma = matrix(c(s^2, sxy, sxy, s^2), 2, 2))
    z <- 1/2 * (log((1 + cor(x[, 1], x[, 2])) / (1 - cor(x[, 1], x[, 2]))) -
                  log((1 + rho0) / (1 - rho0))) * sqrt(n - 3)
    2*pnorm(-abs(z))
  })
}

server <- function(input, output){
  
  rv <- eventReactive(input$goButton,
                      corr(input$n, input$s, input$r, input$rho0))
  
  output$results <- renderPrint({
    data2 <- rv() < 0.05
    input$goButton
    isolate(
      cat("The sample size of", input$n, "is suficient to detect a deviation of",
          input$r, "from de null hypothesis (H0 =", input$rho0, ") with a power of", mean(data2))
    )
  })
  
  output$plotResults <- renderPlot({
    input$goButton
    data <- rv()
    isolate(
      plot(main = paste("P-VALUES. Power:", mean(data < 0.05)), x = data,
           ylab = "p-value", xlab = "Number of repetitions"))
    abline(h = 0.05, col = "red", lty = "73", lwd = 3)
  })
}

shinyApp(ui = ui, server = server)