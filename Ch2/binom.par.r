binom.par <- function(nrep=100, n=5, p=1/6) { # binom.par.r
   x <- rbinom(nrep, n, p)     # B(n,p)에서 nrep 개의 난수 생성
   meanx <- mean(x)            # 이들 난수의 평균
   varx <- var(x)              # 이들 난수의 분산
   list(meanx = meanx, varx = varx)
} # end function

binom.par()
