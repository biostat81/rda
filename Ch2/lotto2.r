lotto2 <- function(nn = 10000) { # lotto.r
  luckyNo <- c(1,2,3,4,5,6)   # 당첨번호
  threeNo2 <- 0
  for (i in 1: nn) {          # nn 번 모의실험
     x <- sort(sample.int(45, size=6))  # 1에서 45사이의 난수 6개 생성하여 오름차순
     if (sum(luckyNo %in% x) == 3) threeNo2 = threeNo2 + 1 
  } # end for i
  list(threeNo2 = threeNo2)
}  # end function


