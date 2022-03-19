# setwd("C:\\Users\\Utilizador\\OneDrive\\Documentos\\qhelp2020_power")
# runApp("Analysis of Variance")

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel ("Analysis of Variance"),
  
  selectInput(inputId = "Type",
              label = "Select the Type of Test", 
              choices = c("One-way Analysis of Variance", 
                          "Two-way Analysis of Variance")),
  
  # if the person selected "One-Way Analysis of Variance"
  # Sample Sizes
  sidebarPanel(
  numericInput("NC", "Sample Size of the Control Group", value = 10, min = 0, max = 10000),
  numericInput("NT1", "Sample Size of Treatmeant Group 1", value = 10, min = 0, max = 10000),
  numericInput("NT2", "Sample Size of Treatment Group 2", value = 10, min = 0, max = 10000),
  
  # Parameter Estimates
  numericInput("Intercept", "Intercept", value = 10, min = 0, max = 10000),
  numericInput("Beta1", "Beta for Group 1", value = 2, min = 0, max = 10000),
  numericInput("Beta2", "Beta for Groups 2", value = -3, min = 0, max = 10000),
  actionButton("button", "Calculate Power")),
  
  mainPanel(
  textOutput("power"),
  textOutput("Results"),
  plotOutput("p.dist")
  )
)

FixedEffectsANOVA <- function(NC, NT1, NT2, Intercept, Beta1, Beta2) {
  grp <- factor(rep(1:3, c(NC, NT1, NT2)), labels = c("ctl", "t1", "t2"))
  beta <- c(mu = Intercept, a2 = Beta1, a3 = Beta2) # a1 := 0
  means <- model.matrix(~ grp) %*% beta
  pval <- replicate(2000, {
    y <- means + rnorm(NC + NT1 + NT2, sd = 5) # y = mu + a + e
    m <- aov(y ~ grp)
    summary(m)[[1]]$"Pr(>F)"[1]
  })
}

server <- function(input, output) {
  psimulation <- eventReactive(input$button ,FixedEffectsANOVA(input$NC , input$NT1, input$NT2, input$Intercept, input$Beta1, input$Beta2))
  output$power <- renderPrint({
    input$button
    isolate(cat( "The power of this test is:", mean (psimulation() < 0.05)) )
    })
  output$p.dist <- renderPlot ({
    #pval <- psimulation()$pval
    #isolate(plot(density(pval)))
    #abline(v=0.05, col = "#CC99CC")
    input$button
    isolate(plot(main = "p-values", psimulation(), xlab = "Index od Replication", ylab = "p-value"))
    abline(h=0.05, col = "#990000")
  })
  output$Results <- renderPrint({
    input$button
    isolate(cat("The sample size of", input$NC , "for the control group, of", input$NT1, "for treatment group 1 and of ", input$NT2, " for treatmeatn group 2 is sufficient to detect a deviation of", input$Beta1, "for treatmeant group 1 and of ", input$Beta2, "for treatment 2 group with a power of ",  mean (psimulation() < 0.05)))
  })
  
}

shinyApp(ui = ui, server = server)