function2<-function(k2,k3,k4){
    pval <- replicate(1000, {
        x <- rnorm(k2, mean =k3 , sd = k4)
        z <- (mean(x) - 1000)/7.5*sqrt(k2) # test statistic, variance known
        2*pnorm(-abs(z)) # p-value
    })
}

calc_p_values <- function(sample_size1, sample_size2, mean1, mean2, sd1, sd2){
    pval <- replicate(2000, {
        x <- rnorm(sample_size1, mean1, sd1)
        y <- rnorm(sample_size2, mean2, sd2)
        t.test(x, y, mu = 0, var.equal = TRUE)$p.value # variances unknown
    })
}
