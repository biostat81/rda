# paired.t.r
x <- c(106,107,105,113,107,112,109,112,111,114)
y <- c(100, 92, 91, 82, 87, 96,101, 96, 79, 96)
dd <- x-y
nn <- length(dd) ; md <- mean(dd) ; sdd <- sd(dd)
t0 <- md / ( sdd/sqrt(nn) )
t0
1-pt(t0, nn-1)

lbd <- md - qt(.975, nn-1)* sdd/sqrt(nn)
ubd <- md + qt(.975, nn-1)* sdd/sqrt(nn)
list(lbd, ubd)
