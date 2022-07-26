geom.dist <-  function(p=0.1, n=1000) { # geom.dist.r
 x <- rgeom(n, p)
 meanx <- mean(x)
 varx <- var(x)
 list(meanx = meanx, varx= varx)
}
