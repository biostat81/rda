# onesample.t.r
x <-c(63, 72, 73, 70, 77, 72, 74, 73, 69, 79)  # score1.r
mx <- mean(x)  # 72.2
sx <- sd(x)     # 4.391912
lbd <- mx - qt(0.975, 9)*sx/sqrt(10)
ubd <- mx + qt(0.975, 9)*sx/sqrt(10)
lbd
ubd
t0 <- (mx-70)/(sx/sqrt(10))
t0
1-pt(t0, 9)

