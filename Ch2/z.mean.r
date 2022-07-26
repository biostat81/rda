z.mean <- function(nn=10, nrep=1000) { #z.mean.r
  xbar <- rep(0, nrep)    # nrep 개의 평균을 저장할 배열
  stdev <- sqrt(5)        # 표준편차 
  for (i in 1:nrep) {     # nn 개의 평균을 nrep번 계산
    xbar[i] <- mean(rnorm(nn, 0, stdev))
  } # nrep 개의 평균을 계산
  list(meanxbar= mean(xbar), varxbar=var(xbar))
} # end function

