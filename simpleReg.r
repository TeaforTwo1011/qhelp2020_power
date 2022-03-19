simpleReg_generate <- function(n = 30, alpha = 0, beta = 0, err = 1,
                               x_mean = 0, x_sd = 1){
    x <- rnorm(n, x_mean, x_sd)
    y <- alpha + beta * x + rnorm(30, sd = err)
    data.frame(x, y)
}

simpleReg_fit <- function(rep = 1000, n = 30, alpha = 0, beta = 0, err = 1,
                          x_mean = 0, x_sd = 1){
    replicate(rep, {
    dat <- simpleReg_generate(n = n, alpha = alpha, beta = beta, err = err,
                              x_mean = x_mean, x_sd = x_sd)
    mod <- lm(y ~ x, data = dat)
    mod
    }, simplify = FALSE)
}

getSD <- function(x) apply(sapply(x, coef), 1, sd)

getPower <- function(y) {
    pvals <- sapply(y, function(x)
        coef(summary(x))["x", "Pr(>|t|)"])
    list(mean(pvals < .05), sd(pvals < .05), pvals)
}
