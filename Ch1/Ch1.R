setwd("C:/Users/KNOU_stat/R_codes")


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
                 col_names=c("id", "age", "sex", "treatment", "CA19-9", "CRP", "Stage", "complication"))

dat4<-read.csv("patients.csv")
dat5<-read.table("patients_tab.txt", header=TRUE)

library(foreign)

write.csv(dat3, "subset_data.csv")
write.table(dat3, "subset_data.txt")
save(dat3, file="subset_data.RData")
load("subset_data.RData")



x<-1:10
sum(x)
x %>% sum()

sum(round(log(1:10)))
x %>% log() %>% round() %>% sum() 

sum(round(log(1:10), digits=1))
x %>% log() %>% round(digits=1) %>% sum() 




####
dat.base1<-dat1
colnames(dat1)
colnames(dat.base1)[4]<-"TX"
colnames(dat.base1)[8]<-"CX"

dat.base2<-dat1
colnames(dat.base2)[which(colnames(dat.base2)=="treatment")]<-"TX"
colnames(dat.base2)[which(colnames(dat.base2)=="complication")]<-"CX"

dat.dplyr1 <- dat1 %>% rename(TX=treatment,
                           CX=complication)




age.v<-dat1$age

dat.base3<-dat1[, c(2:5, 7)]
dat.base4<-dat1[, -c(1, 6, 8)]
dat.base5<-dat1[, c("age", "sex", "treatment", "CA19.9", "Stage")]

dat.dplyr2<-dat1 %>% select(2:5, 7)
dat.dplyr3<-dat1 %>% select(-1, -6, -8)
dat.dplyr4<-dat1 %>% select(age, sex, treatment, CA19.9, Stage)
dat.dplyr5<-dat1 %>% select(-id, -CRP, -complication)


dat.base6<-dat1[dat1$age>=40, ]
dat.base7<-dat1[dat1$Stage==3, ]
dat.base8<-dat1[dat1$age>=40 & dat1$Stage==3, ]
dat.base9<-dat1[dat1$age>=40 | dat1$Stage==3, ]

dat.dplyr6<-dat1 %>% filter(age>=40)
dat.dplyr7<-dat1 %>% filter(Stage==3)
dat.dplyr8<-dat1 %>% filter(age>=40 & Stage==3)
dat.dplyr9<-dat1 %>% filter(age>=40 | Stage==3)



dat.base10<-dat1
dat.base10$CA19.9[dat.base10$CA19.9=="<1.0"]<-"1"

dat.base11<-dat1
dat.base11$CA19.9<-replace(dat.base11$CA19.9, dat.base11$CA19.9=="<1.0", "1")


dat.dplyr10<-dat1 %>% mutate(CA19.9=replace(CA19.9, CA19.9=="<1.0", "1"))





