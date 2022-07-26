lotto3 <- function(nn = 1000000) { # lotto.r
  luckyNo <- c(1,2,3,4,5,6)   # 당첨번호
  fourNo <- 0
  for (i in 1: nn) {          # nn 번 모의실험
     x <- sort(sample.int(45, size=6))  # 1에서 45사이의 난수 6개 생성하여 오름차순
     if (sum(luckyNo %in% x) == 4) fourNo = fourNo + 1 
  } # end for i
  list(fourNo = fourNo)
}  # end function


