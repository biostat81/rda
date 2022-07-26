# lm.test.r
 age <-     c( 54,  69,  43,  39,  64,  52,  47,  34,  73,  37,  45)
 c.level <- c(181, 235, 193, 177, 197, 191, 213, 167, 212, 183, 190)
 cdata <- data.frame(age, c.level)

 nn <- length(age) ; my <- mean(c.level); mx <- mean(age)
 Sxx <- sum(age^2) - nn*mx^2
 Sxy <- sum(age*c.level) - nn*mx*my
 SST <- sum(c.level^2) - nn*my^2
 b1 <- Sxy/Sxx; b0 <- my -b1*mx
 coef <- c(b0, b1)
 yhat <- b0 + b1* age
 SSE <- sum( (c.level - yhat)^2 )
 SSR <- b1^2*Sxx ; MSR <- SSR
 MSE <- SSE / (nn-2)
 cat("SST = ", SST, "SSR = ", SSR, "SSE = ", SSE, "\n")
 R.square <- SSR/SST
 cat("R^2 = ", R.square, "\n")
 F0 <- MSR/MSE ; p.val <- 1-pf(F0, 1, nn-2)
 cat("F = ", F0, "P-value = ", p.val, "\n")

 se.b1 <- sqrt(MSE / Sxx) ; se.b0 <- sqrt(MSE*(1/nn + mx^2/Sxx))
 cat("SE b1 = ", se.b1, "SE b0 = ", se.b0, "\n")
 
 # beta0 =0, beta1 =0에 대한 가설검정
 t0.b1 <- b1/se.b1; t0.b0 <- b0/se.b0
 p.b1 <-  2*(1-pt(t0.b1, nn-2)); p.b0 <- 2*(1-pt(t0.b0, nn-2))
 cat("t for b1 = ", t0.b1, "p.val = ", p.b1, "\n" )
 cat("t for b0 = ", t0.b0, "p.val = ", p.b0, "\n" )
 
 # beta0, beta1의 신뢰구간
 ci.b0 <- c(b0-qt(.975,nn-2)*se.b0, b0+qt(.975,nn-2)*se.b0)
 cat("beta0의 신뢰구간: ", ci.b0, "\n")
 ci.b1 <- c(b1-qt(.975,nn-2)*se.b1, b1+qt(.975,nn-2)*se.b1)
 cat("beta1의 신뢰구간: ", ci.b1, "\n")
 
 # 주어진 x 값에서 y의 예측구간, mu의 신뢰구간 그림 그리기
 xx <- seq(min(age), max(age), length = 100)
 lbd.mu <- b0+b1*xx - qt(.975,nn-2)*sqrt( MSE*(1/nn+(xx-mx)^2/Sxx) )
 ubd.mu <- b0+b1*xx + qt(.975,nn-2)*sqrt(MSE*(1/nn+(xx-mx)^2/Sxx))
 lbd.yi <- b0+b1*xx - qt(.975,nn-2)*sqrt(MSE*(1+1/nn+(xx-mx)^2/Sxx))
 ubd.yi <- b0+b1*xx + qt(.975,nn-2)*sqrt(MSE*(1+1/nn+(xx-mx)^2/Sxx))
 plot(c.level ~ age, data = cdata)
 abline(b0, b1)
 lines(xx, lbd.mu, col=rgb(1,0,0), lty=2)
 lines(xx, ubd.mu, col=rgb(1,0,0), lty=2)
 lines(xx, lbd.yi, col=rgb(0,0,1), lty=3)
 lines(xx, ubd.yi, col=rgb(0,0,1), lty=3)
 
lsfit(age, c.level)

summary(lm(c.level ~ age, data=cdata))
plot(c.level ~ age, data = cdata)
