lotto <- function(nn = 10000) { # lotto.r
  luckyNo <- c(1,2,3,4,5,6)   # 당첨번호
  threeNo <- 0                # 번호 세 개의 맞는 회수
  for (i in 1: nn) {          # nn 번 모의실험
     x <- sort(sample.int(45, size=6))  # 1에서 45사이의 난수 6개 생성하여 오름차순
     nMatch = 0
     for (j in 1:6) {  # 임의 로또번호 x 값에 대해
       for(k in j:6) { # 당첨번호 LuckNo와 같은지 비교
         if (x[j] == luckyNo[k]) nMatch = nMatch +1 # 각 번호를 당첨번호와 비교
     }} # end for j & k
     if (nMatch == 3) threeNo = threeNo + 1 # 세 개의 번호가 일치한 횟수
  } # end for i
list(threeNo = threeNo)
}  # end function
