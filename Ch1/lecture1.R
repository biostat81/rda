#### This file contains R codes used in Lecture 1.
#### For the most part it was copied from the textbook.

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


setwd("C:\\Users\\SYP\\Dropbox\\KNOU_강의개편\\고급R활용\\R_exercise")

library(readxl)
dat0<-read_excel("patients_2sheets.xlsx")
dat1<-read_excel("patients_2sheets.xlsx", sheet = "whole")
dat2<-read_excel("patients_2sheets.xlsx", sheet = "subset", col_names=FALSE)
dat3<-read_excel("patients_2sheets.xlsx", sheet = "subset", 
                 col_names=c("id", "age", "sex", "treatment", "CA19.9", "CRP", "Stage", "complication"))

dat4<-read.csv("patients.csv")
dat5<-read.table("patients_tab.txt", header=TRUE)

write.csv(dat3, "subset_data.csv")
write.table(dat3, "subset_data.txt")
save(dat3, file="subset_data.RData")
load("subset_data.RData")

