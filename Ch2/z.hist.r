z.hist <- function(nn=1000) { # z.hist.r
 par(mfrow=c(1,2))
 hist(rnorm(1000))
 hist(rnorm(1000), freq=F)
 lines(x<-seq(-3,3, length=100), dnorm(x))
} # end function

z.hist()
