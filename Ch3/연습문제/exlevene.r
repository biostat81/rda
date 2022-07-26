x <- c(69, 74, 66, 66, 78, 68, 62, 63, 69, 69)
 y <- c(55, 54, 56, 58, 48, 52, 58, 51, 53, 56)

mx <- mean(x)
my <- mean(y)

 
devx <- abs(x-mean(x))
devy <- abs(y-mean(y))

t.test(devx, devy, var.equal=T)
t.test(x-10, y, var.equal=T)


