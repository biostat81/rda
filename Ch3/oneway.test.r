# oneway.test.r
y <- c( 11, 11, 10, 10, 10, 11, 10,  8, 10,  9,
        12, 11, 11, 13, 12, 11, 11, 11, 12,  9,
        12, 13, 13, 11, 10, 13, 11, 12, 13, 11,
         9, 10,  8, 10, 13, 10, 10, 10, 10,  8)
my.i <- c(mean(y[1:10]), mean(y[11:20]), mean(y[21:30]),mean(y[31:40]))
n.i <- c(10,10,10,10)
CT <-  length(y)*mean(y)^2
SST <- sum (y^2) - CT
SSTrt <- sum(n.i*my.i^2) - CT
SSE <- SST - SSTrt
MSTrt <- SSTrt /(4-1)
MSE <- SSE / (40-4)
F0 <- MSTrt / MSE
F0
1-pf(F0, 3, 36)

#====================

y <- c( 11, 11, 10, 10, 10, 11, 10,  8, 10,  9,
        12, 11, 11, 13, 12, 11, 11, 11, 12,  9,
        12, 13, 13, 11, 10, 13, 11, 12, 13, 11,
         9, 10,  8, 10, 13, 10, 10, 10, 10,  8)
x <- c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
fert <- data.frame(y,x)
oneway.test(y ~x, var.equal=T)

