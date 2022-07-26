nbinom.dist <-  function(p=0.1, r = 3, n=1000) { # nbinom.dist.r
 x <- rnbinom(n, r, p)
 meanx <- mean(x)
 varx <- var(x)
 list(meanx = meanx, varx= varx)
}

