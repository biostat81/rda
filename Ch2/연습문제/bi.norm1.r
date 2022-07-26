bi.norm1 <- function(rho=0.5, size=100) { # bi.norm1.r
    x <- rnorm(size)
    y <- rep(0, size) # 초기화
    for (i in 1:size) y[i] <- rnorm(1, rho*x[i], sqrt(1-rho^2))
    plot(x, y)
}
