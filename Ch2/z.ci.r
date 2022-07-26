z.ci <- function(alpha = 0.05, nrep = 1000) { #z.ci.r
  ndata <- 10                 # 신뢰구간을 계산할 자료의 수 
  qz <-  qnorm(1-alpha/2)     # 
  se <- 1/sqrt(ndata)         # 
  ncover <- 0                 # 신뢰구간이 0을 포함하는 회수
  for (i in 1:nrep) {         # nrep 번 (기본값 1000번) 반복
    x <- rnorm(ndata)         # ndata 개(기본값 10개)의 난수 생성
    meanx <- mean(x)          # ndata 개의 평균
    ubound <- meanx + qz*se   # 신뢰상한
    lbound <- meanx - qz*se   # 신뢰하한
    if ( ubound > 0 & lbound < 0) ncover = ncover + 1 # 신뢰구간에 포함되는 개수
  } # end for
  list(ncover=ncover)  # 출력
} # end function

z.ci()

