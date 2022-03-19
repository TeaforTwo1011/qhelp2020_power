corr <- function(n, s, r, rho0){
    pval <- replicate(1000, {
        sxy <- r*s*s
        x <- MASS::mvrnorm(n, mu = c(100, 100),
                           Sigma = matrix(c(s^2, sxy, sxy, s^2), 2, 2))
        z <- 1/2 * (log((1 + cor(x[, 1], x[, 2])) / (1 - cor(x[, 1], x[, 2]))) -
                        log((1 + rho0) / (1 - rho0))) * sqrt(n - 3)
        2*pnorm(-abs(z))
    })
}
