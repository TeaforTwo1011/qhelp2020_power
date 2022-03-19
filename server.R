library(shiny)

# source(helpers.r)
source("simpleReg.r")
source("binomialtest.r")
source("testofcorrelation.r")
source("t_test.r")

server <- function(input, output){

    # Simple Regression
    sim <- reactive(getPower(simpleReg_fit(n = input$n, alpha = input$alpha, beta = input$beta, err = input$err, x_mean = input$x_mean, x_sd = input$x_sd)))
    output$simpleReg_power <- renderText(paste0("The power is: ", sim()[[1]], " (sd = ", sim()[[2]], ")"))
    output$simpleReg_pplot <- renderPlot(plot(sim()[[3]], ylab = "p-values"))

    # binomial test
    observeEvent(input$goButton,
                 {rv <- function1(input$n_bin,input$k2)
                 output$power<-renderPrint({
                     data2<-rv < 0.05
                     input$goButton
                     cat("The power of this test is",mean(data2))

                 })
                 output$distribution<-renderPlot({
                     data <- rv
                     #stripchart(function2(input$n,input$k3,input$k4), pch=16, vertical=TRUE, method = "jitter", xlab="Index",ylab="p values", log="y")
                     plot(main="p-values", data, xlab="Index", ylab="p-values")
                     abline(h=0.05, col="red", lty="dashed", lwd=4)
                 })
                 })

    #anova
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
    output$anova_hist <- renderPlot({
        pval<-getdata()$pval
        plot(density(pval,bw=0.0001))
        abline(v = 0.05, col = "red")
    })

    # # Test of Correlation
    observeEvent(input$goButton_toc,
                 {rv_toc <- corr(input$n_toc, input$s, input$r, input$rho0)

                 output$results <- renderPrint({
                     data2_toc <- rv_toc < 0.05
                     cat("The sample size of", input$n_toc, "is suficient to detect a deviation of",
                         input$r, "from de null hypothesis (H0 =", input$rho0, ") with a power of", mean(data2_toc))
                 })

                 output$plotResults <- renderPlot({
                     data_toc <- rv_toc
                     plot(main = paste("P-VALUES. Power:", mean(data_toc < 0.05)), x = data_toc,
                          ylab = "p-value", xlab = "Number of repetitions")
                     abline(h = 0.05, col = "red", lty = "73", lwd = 3)
                 })})

    # # 2-Sample t-Test
    observeEvent(
        input$go,
        {t_rv <- calc_p_values(input$t_n1, input$t_n2, input$t_mean1, input$t_mean2, input$t_sd1, input$t_sd2)

        output$t_results <- renderPrint({
            p_values <- t_rv
            cat("The power calculated based on your input parameters is", mean(p_values < 0.05))
        })

        output$t_plotResults <- renderPlot({
            plot(main = "P-VALUES", t_rv, xlab = "Number of iteration", ylab = "p-value")
            abline(h = 0.05, col = "red", lty = "73", lwd = 3)
        })
        })
    # # t-Test
    observeEvent(input$t_goButton, {
        t_rv2 <- function2(input$t_n,input$k3,input$k4)

        output$t_power<-renderPrint({
            t_data2<-t_rv2 < 0.05
            cat("The power of this test is",mean(t_data2))

        })
        output$t_distribution<-renderPlot({
            t_data<-t_rv2
            #stripchart(function2(input$n,input$k3,input$k4), pch=16, vertical=TRUE, method = "jitter", xlab="Index",ylab="p values", log="y")
            plot(main="p-values", t_data, xlab="Index", ylab="p-values")
            abline(h=0.05, col="red", lty="dashed", lwd=4)
        })
    }
    )
}
