#### This file contains R codes used in Lecture 2.
#### For the most part it was copied from the textbook.

library(dplyr)

x<-1:10
sum(x)
x %>% sum()

sum(round(log(x)))
x %>% log() %>% round() %>% sum() 

sum(round(log(1:10), digits=1))
x %>% log() %>% round(digits=1) %>% sum() 

setwd("C:\\Users\\SYP\\Dropbox\\KNOU_강의개편\\고급R활용\\R_exercise")

library(readxl)
dat1<-read_excel("patients_2sheets.xlsx", sheet = "whole")


####
dat.base1<-dat1
colnames(dat1)
colnames(dat.base1)[4]<-"TX"
colnames(dat.base1)[5]<-"CA19.9"

dat.base1.1<-dat1
colnames(dat.base1.1)[colnames(dat.base1.1)=="treatment"]<-"TX"
colnames(dat.base1.1)[colnames(dat.base1.1)=="CA19-9"]<-"CA19.9"

dat.dplyr1 <- dat1 %>% rename(TX=treatment, 
                              CA19.9='CA19-9')



dat.base2<-dat.base1[, c(2:5, 7)]
dat.base2.1<-dat.base1[, -c(1, 6, 8)]
dat.base2.2<-dat.base1[, c("age", "sex", "TX", "CA19.9", "Stage")]

dat.dplyr2<-dat.dplyr1 %>% select(2:5, 7)
dat.dplyr2.1<-dat.dplyr1 %>% select(-1, -6, -8)
dat.dplyr2.2<-dat.dplyr1 %>% select(age, sex, TX, CA19.9, Stage)
dat.dplyr2.3<-dat.dplyr1 %>% select(-id, -CRP, -complication)


dat.base3<-dat.base2[dat.base2$age>=40, ]
dat.base3.1<-dat.base2[dat.base2$Stage==3, ]
dat.base3.2<-dat.base2[dat.base2$age>=40 & dat.base2$Stage==3, ]
dat.base3.3<-dat.base2[dat.base2$age>=40 | dat.base2$Stage==3, ]

dat.dplyr3<-dat.dplyr2 %>% filter(age>=40)
dat.dplyr3.1<-dat.dplyr2 %>% filter(Stage==3)
dat.dplyr3.2<-dat.dplyr2 %>% filter(age>=40 & Stage==3)
dat.dplyr3.3<-dat.dplyr2 %>% filter(age>=40 | Stage==3)


dat.base4<-dat.base3
dat.base4$CA19.9[dat.base4$CA19.9=="<1.0"]<-"1"

dat.base4.1<-dat.base3
dat.base4.1$CA19.9<-replace(dat.base4.1$CA19.9, 
                            dat.base4.1$CA19.9=="<1.0", "1")


dat.dplyr4<-dat.dplyr3 %>% mutate(
                CA19.9=replace(CA19.9, CA19.9=="<1.0", "1"))



