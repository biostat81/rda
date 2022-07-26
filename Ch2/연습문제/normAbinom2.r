normAbinom2 <- function(nn=1000, n=20, p=0.5) { # normAbinom2.r
  x <- rbinom(nn, n, p)
  hist(x, prob=T)

  mx <- n*p;  sdx <- sqrt(n*p*(1-p))
  xa <- seq(mx-3*sdx, mx+3*sdx, length=200)
  y <- dnorm(xa, mx, sdx)
  lines(xa, y, col=rgb(1,0,0))
} # end function

