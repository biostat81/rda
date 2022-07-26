draw.chisq <- function() { # draw.chisq.r
  from <-0    # x축 하한
  to <- 20    # x축 상한
  x <- seq(from, to, length=100)  # x축의 값
  plot(x, dchisq(x, 3), type="l", col="red", 
     main=expression(chi[nu]^2), 
     ylab=expression(f(x)) )   # 자유도 3인 카이제곱
  curve(dchisq(x, 5), from=from, to=to, lty=2, add=T, 
     col="blue")               # 자유도 5인 카이제곱 분포
  curve(dchisq(x, 10), from=from, to=to, lty=3, add=T, 
     col="magenta")            # 자유도 10인 카이제곱 분포
  abline(h=0)                  # x축 그리기
  legend("topright", lty=1:3, col=c("red", "blue", "magenta"),
     legend=c(expression(nu == 3),expression(nu == 5),
     expression(nu == 10)) )   # 범례만들기
} # end function

