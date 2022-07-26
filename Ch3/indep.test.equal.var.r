# independent.test.equal.var.r
  x <- c(21.6,20.8,17.6,20.1,20.1,21.9,20.6,19.4,21.5,26.1) 
  y <- c(20.6,20.4,20.2,20.2,18.0,19.8,20.9,19.7,20.3,19.7,22.7)
  mx <- mean(x); my <- mean(y)
  sdx <- sd(x); sdy <- sd(y)
  sp <- sqrt( (9*sdx^2+10*sdy^2)/(10+11-2) )
  t0 <- (mx-my)/(sp*sqrt(1/10+1/11))
  t0
  1-pt(t0, 19)
  lbd <- (mx-my) - qt(0.995, 19)*sp*sqrt(1/10+1/11)
  lbd
  ubd <- (mx-my) + qt(0.995, 19)*sp*sqrt(1/10+1/11)
  ubd

