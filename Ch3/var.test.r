var.test <- function() { # var.test.r
 x <- c(21.6,20.8,17.6,20.1,20.1,21.9,20.6,19.4,21.5,26.1)
 y <- c(20.6,20.4,20.2,20.2,18.0,19.8,20.9,19.7,20.3,19.7,22.7)
 mm <- length(x);  nn <- length(y)

 F0 <- sd(x)^2/sd(y)^2
 F0
 if (F0 >= 1)  p.val <- 2*(1- pf(F0, mm-1, nn-1)) 
 else  p.val <- 2*pf(F0, mm-1, nn-1)
 p.val

 lbd <- 1/qf(0.975, mm-1, nn-1)*var(x)/var(y)
 ubd <- 1/qf(0.025, mm-1, nn-1)*var(x)/var(y)
 ci <-  c(lbd, ubd)
 list(F0=F0, p.val=p.val, ci=ci)
}
