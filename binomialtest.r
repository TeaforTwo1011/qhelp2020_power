function1 <- function(k2, k3){
    pval <- replicate(1000, { # replications of experiment
        x <- rbinom(1, size = k2, # data-generating model with
                    prob = k3) # minimum relevant effect
        binom.test(x, n = k2, p = 0.5)$p.value # p-value of test against H0
    })
}
