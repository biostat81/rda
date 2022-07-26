### This file contains R codes included in Chapter 1 of the textbook (R Data Analysis).

setwd("C:/Users/KNOU_stat/R_codes")  ### Change this to your own working directory

install.packages("dplyr")
library(dplyr)


2*3+1
a<-2
b<-3
c<-1
a*b+c
d<-a*b+c
d

typeof(d)
x<-"KNOU"
x
typeof(x)
y<-TRUE
y
typeof(y)
yy<-2>3
yy
typeof(yy)
z<-as.factor(1)
z
typeof(z)
is.factor(z)

e<-c(1, 10, 100)
e
f<-1:5
f
g<-seq(1, 9, 2)
g
h<-rep(1, 5)
h
i<-c(e, f, g)
i

j<-matrix(1:8, ncol=2)
j
k<-matrix(1:8, nrow=2)
k
l<-cbind(g, h)
l
m<-rbind(g, h)
m
n<-as.character(h)
n
cbind(g, n)
typeof(cbind(g, n))


name<-c("Kim", "Lee", "Park")
age<-c(35, 42, 27)
sex<-as.factor(c("Female", "Male", "Female"))
dat<-data.frame(name, age, sex)
dat


v<-1:4
v
v[2]
v[2]<-100
v

m<-matrix(1:8, ncol=2)
m
m[1,2]
m[1,]
m[,2]

df<-as.data.frame(m)
df
df[1,2]
df[1,]
df[,2]
df$V2

v0<-1:4
is.na(v0)
is.na(v0[2])
v0[2]<-NA
v0
is.na(v0)
is.na(v0[1])
is.na(v0[2])


s1<-2
v1<-1:3
m1<-matrix(1:6, ncol=2)
v1
v1+s1
v1*s1
v1/s1
v1^s1
m1+s1
m1*s1

v1<-1:3
v2<-rep(10, 3)
v3<-1:5
v4<-1:6
v1+v2
v1*v2
v1+v3
v1+v4




library(readxl)
dat0<-read_excel("patients_2sheets.xlsx")
dat1<-read_excel("patients_2sheets.xlsx", sheet = "whole")
dat2<-read_excel("patients_2sheets.xlsx", sheet = "subset", col_names=FALSE)
dat3<-read_excel("patients_2sheets.xlsx", sheet = "subset", 
                 col_names=c("id", "age", "sex", "treatment", "CA19.9", "CRP", "Stage", "complication"))

dat4<-read.csv("patients.csv")
dat5<-read.table("patients_tab.txt", header=TRUE)

library(foreign)

write.csv(dat3, "subset_data.csv")
write.table(dat3, "subset_data.txt")
save(dat3, file="subset_data.RData")
load("subset_data.RData")




x<-1:10
sum(x)

library(dplyr)
x %>% sum()

sum(round(log(x)))
x %>% log() %>% round() %>% sum() 

sum(round(log(1:10), digits=1))
x %>% log() %>% round(digits=1) %>% sum() 




dat.base1<-dat1
colnames(dat1)
colnames(dat.base1)[4]<-"TX"
colnames(dat.base1)[5]<-"CA19.9"

dat.base1.1<-dat1
colnames(dat.base1.1)[colnames(dat.base1.1)=="treatment"]<-"TX"
colnames(dat.base1.1)[colnames(dat.base1.1)=="CA19-9"]<-"CA19.9"

dat.dplyr1 <- dat1 %>% rename(TX=treatment, CA19.9='CA19-9')

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
dat.base4.1$CA19.9<-replace(dat.base4.1$CA19.9, dat.base4.1$CA19.9=="<1.0", "1")

dat.dplyr4<-dat.dplyr3 %>% mutate(CA19.9=replace(CA19.9, CA19.9=="<1.0", "1"))

dat.base5<-dat.base4
dat.base5$age50<-ifelse(dat.base5$age>=50, 1, 0)

dat.dplyr5<-dat.dplyr4 %>% mutate(age50=ifelse(age>=50, 1, 0))

dat.base5$age.grp<-cut(dat.base5$age, breaks=c(0, 50, 60, 70, Inf))

dat.dplyr6<-dat.dplyr5 %>% mutate(age.grp=cut(age, breaks=c(0, 50, 60, 70, Inf)))

summary(dat.base5)
summary(dat.dplyr6)

dat.base5$CA19.9<-as.double(dat.base5$CA19.9)

dat.dplyr7<-dat.dplyr6 %>% mutate(CA19.9=as.double(CA19.9))

dat.base5$sex<-as.factor(dat.base5$sex)
dat.base5$TX<-as.factor(dat.base5$TX)
dat.base5$Stage<-as.factor(dat.base5$Stage)
dat.base5$age50<-as.factor(dat.base5$age50)

dat.dplyr8<-dat.dplyr7 %>% mutate_at(vars(sex, TX, Stage, age50), as.factor)

summary(dat.base5)
summary(dat.dplyr8)


#### multiple tasks in a row

dat.base1<-dat1
colnames(dat.base1)[4]<-"TX"
colnames(dat.base1)[5]<-"CA19.9"
dat.base2<-dat.base1[, c("age", "sex", "TX", "CA19.9", "Stage")]
dat.base<-dat.base2[dat.base2$age>=40, ]
dat.base$CA19.9[dat.base$CA19.9=="<1.0"]<-"1"
dat.base$age50<-ifelse(dat.base$age>=50, 1, 0)
dat.base$age.grp<-cut(dat.base$age, breaks=c(0, 50, 60, 70, Inf))
dat.base$CA19.9<-as.double(dat.base$CA19.9)
dat.base$sex<-as.factor(dat.base$sex)
dat.base$TX<-as.factor(dat.base$TX)
dat.base$Stage<-as.factor(dat.base$Stage)
dat.base$age50<-as.factor(dat.base$age50)


dat.dplyr<-dat1 %>% rename(TX=treatment, 
                           CA19.9='CA19-9') %>%
                    select(age, sex, TX, CA19.9, Stage) %>%
                    filter(age>=40) %>%
                    mutate(CA19.9=replace(CA19.9, CA19.9=="<1.0", "1"),
                           age50=ifelse(age>=50, 1, 0),
                           age.grp=cut(age, breaks=c(0, 50, 60, 70, Inf)),
                           CA19.9=as.double(CA19.9)) %>%
                    mutate_at(vars(sex, TX, Stage, age50), as.factor)





