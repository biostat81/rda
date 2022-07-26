bi.norm2 <- function(rho=0.5, size=1000) { # bi.norm2.r 
    x <- rnorm(size)
    y <- rep(0, size) # 초기화
    for (i in 1:size) y[i] <- rnorm(1, rho*x[i], sqrt(1-rho^2))
    hist(y, prob=T)
    xa <- seq(-3, 3, length=size)
    lines(xa, dnorm(xa), col=rgb(1,0,0))
 }

