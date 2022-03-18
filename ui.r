ui <- fluidPage(
    tabsetPanel(
        tabPanel("Regression",
                 sidebarPanel(
                     h3("Simple Regression"),
                     h5("Sample"),
                     numericInput("n", label = "sample size",
                                  value = 30, min = 1, max = 1000),
                     numericInput("x_mean", label = "mean of x",
                                  value = 0, min = 1, max = 1000),
                     numericInput("x_sd", label = "standard deviation of x",
                                  value = 1, min = 1, max = 1000),
                     h5("Parameters"),
                     numericInput("alpha", label = "intercept",
                                  value = 0, min = -1000, max = 1000, step = 0.1),
                     numericInput("beta", label = "beta weight",
                                  value = 0, min = -1000, max = 1000, step = 0.1),
                     numericInput("err", label = "error term (deviation)",
                                  value = 1, min = 0, max = 1000, step = 0.1)
                 ),
                 mainPanel(
                     textOutput("simpleReg_power"),
                     plotOutput("simpleReg_pplot")
                 )
        ),
        tabPanel("tab2", ),
        tabPanel("tab3")
    )
)
