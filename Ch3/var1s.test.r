 # var1s.r
 
 nn <- 100
 x <- rnorm(nn, sd =2); vx <- var(x)
 chi0 <- (nn-1)*vx / 2^2
 chi0
 p.val <- 1-pchisq(chi0, nn-1)
 p.val
 ci <- c( (nn-1)*vx /qchisq(0.975, nn-1), (nn-1)*vx /qchisq(0.025, nn-1) )
 ci
