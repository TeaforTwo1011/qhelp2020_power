library(shiny)

ui <- fluidPage(
  
  
  sidebarLayout(

    sidebarPanel(
      numericInput("n1",label="sample size n1",value=10,width="50%"),
      numericInput("n2",label="sample size n2",value=10,width="50%"),
      numericInput("n3",label="sample size n3",value=10,width="50%"),
      numericInput("mu",label="total mean(μ)",value=5,width="50%"),
      numericInput("a2",label="a2",value=1,width="50%"),
      numericInput("a3",label="a3",value=3,width="50%"),
      numericInput("sd",label="standard deviation",value=3,width="50%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("hist")
    )
  )
)

# Server logic
server <- function(input, output) {
  #output$distPlot <- renderPlot({
  #  hist(rnorm(input$obs))
  #})
  
  getdata <- reactive({
    grp <- factor(rep(1:3, c(input$n1, input$n2, input$n3)), labels = c("ctl", "t1", "t2"))
    beta <- c(input$mu, input$a2, input$a3) # a1 := 0
    means <- model.matrix(~ grp) %*% beta
    
    pval <- replicate(200, {
      y <- means + rnorm(input$n1 + input$n2 + input$n3, sd = input$sd) # y = mu + a + e
      m <- aov(y ~ grp)
      summary(m)[[1]]$"Pr(>F)"[1]
    })
    
    list(pval=pval)
  })
  
   output$hist <- renderPlot({
    pval<-getdata()$pval
    #pval <- out$pval
    plot(density(pval,bw=0.0001))
    abline(v = 0.05, col = "red")
    #poly_range <- density(pval)$pval < 0.05
    #polygon(c(0,density(pval)$,col = "darkgreen")
    #hist(pval)
    #pvald <- density(pval)
    #lines(density(pval), lwd = 2,col="red")
  })
}

# Complete app with UI and server components
shinyApp(ui, server)


