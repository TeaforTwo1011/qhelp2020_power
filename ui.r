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
        tabPanel("BinomialTest",
                 fluidRow(
                     column(10, offset=5,titlePanel("Binomial test")),
                     column(3, offset=2,wellPanel(numericInput("n_bin", "Sample size",value=100,min=0, max=10000, step=10))),
                     column(3, offset=2, wellPanel(numericInput("k2", "Minimum relevant effect", value=0.1, min=0, max=1, step=0.01))),
                     column(2, offset=5, actionButton("goButton", "Go!")),
                     column(8, offset=5,textOutput("power")),
                     column(8, offset=2, plotOutput("distribution"))
                 )
        ),
        tabPanel("ANOVA",
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
                         plotOutput("anova_hist")
                     )
                 )
                 ),
        tabPanel("Test of Correlation",
                 titlePanel("Test of Correlation"),
                 withMathJax("Power (also called true positive rate, hit rate, sensitivity, or recall) is defined as $$1 - \\beta$$
  It is the probability of a statistical test to detect an efect of a given size. Therefore, designing an
  experiment to have a good chance to find an efect means making sure its power is high enough.
  High power is a necessary condition for valid inference. A high power value can be
  considered when the proportion of significant results is about 0.8 or higher."),
                 hr(),
                 fluidRow(
                     column(2, numericInput("n_toc", "Number of observations", 60, min = 10, max = 500, step = 1)),
                     column(2, offset = 1, numericInput("s", "Standard deviation:", 15, min = 1, max = 100, step = .1)),
                     column(2, offset = 1, numericInput("r", "Effect size:", .3, min = 0, max = 1, step = .01)),
                     column(2, offset = 1, numericInput("rho0", "Null Hypothesis (H0):", .6, min = 0, max = 1, step = .01)),
                     column(2, offset = 5, actionButton("goButton_toc", "Calculate power")),
                     column(10, offset = 0, verbatimTextOutput("results")),
                     plotOutput("plotResults")
                 )
        ),
        tabPanel("Two-Sample t-Test",
                 titlePanel("2-sample t-test for independent samples"),
                 hr(),
                 withMathJax('Two-sample t-test tests the hypothesis \\(H_0: \\mu_x-\\mu_y=\\delta=0\\),
              where \\(\\mu_x\\) and \\(\\mu_y\\) are the means of each sample.
              The present application generates 2000 times a pair of samples based on
              the parameters that are entered below and at each iteration performs a t-test and calculates the accompanying p-value.
              Power is the proportion of times where the test actually gave a signicant result (p<0.05).'),
                 hr(),
                 h5("Please enter the parameters for your samples"),
                 h4("First sample"),
                 fluidRow(
                     column(2, numericInput("t_n1", "sample size", "100", width = '100px')),
                     column(2, numericInput("t_mean1", "sample mean", "0", width = '100px')),
                     column(2, numericInput("t_sd1", "standard deviation", "1", width = '150px'))
                 ),
                 h4("Second sample"),
                 fluidRow(
                     column(2, numericInput("t_n2", "sample size", "100", width = '100px')),
                     column(2, numericInput("t_mean2", "sample mean", "0", width = '100px')),
                     column(2, numericInput("t_sd2", "standard deviation", "1", width = '150px'))
                 ),
                 actionButton("go","Calculate power"),
                 h5(""),
                 verbatimTextOutput("t_results"),
                 plotOutput("t_plotResults")
            ),
        tabPanel("One-Sample t-Test",
                 fluidRow(
                     column(10, offset=5,titlePanel("One sample z-test")),
                     column(3, offset=1, wellPanel(numericInput("t_n", "Sample size", 1,min=0, max=2000, step=10))),
                     #numericInput("k2", "Minimum relevant effect", min=0, max=1, step=0.01),
                     column(3, offset=1, wellPanel(numericInput("k3", "Mean:", 0.1, min=0, max=100000, step=1))),
                     column(3, offset=1, wellPanel(numericInput("k4", "Standard deviation:", 0.2,min=0, max=50, step=0.1))),
                     column(2, offset=5, actionButton("t_goButton", "Go!")),
                     column(8, offset=5, textOutput("t_power")),
                     column(8, offset=2, plotOutput("t_distribution"))
                 )
        )
    )
)
