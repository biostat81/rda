m.i <-c(10.1, 10.7, 11.7, 10.2)
SST <- 4619 - 40* 10.675^2
SSTrt <- sum(10*m.i^2) - 40*10.675^2
SSE <- SST - SSTrt
MSTrt <- SSTrt/3
MSE <- SSE/36
F0 <- MSTrt/MSE
F0
1-pf(F0, 3, 36)

