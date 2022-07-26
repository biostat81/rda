#### This file contains R codes used in Lecture 3, part 1 & 2
#### For the most part it was copied from the textbook.

setwd("C:\\Users\\SYP\\R_exercise")

library(readxl)
dat1<-read_excel("patients_2sheets.xlsx", sheet = "whole")


####
dat.base1<-dat1
colnames(dat1)
colnames(dat.base1)[4]<-"TX"
colnames(dat.base1)[5]<-"CA19.9"
dat.base2<-dat.base1[, c("age", "sex", "TX", "CA19.9", "Stage")]
dat.base3<-dat.base2[dat.base2$age>=40, ]
dat.base4<-dat.base3
dat.base4$CA19.9[dat.base4$CA19.9=="<1.0"]<-"1"

library(dplyr)

dat.dplyr1 <- dat1 %>% rename(TX=treatment, CA19.9='CA19-9')
dat.dplyr2<-dat.dplyr1 %>% select(age, sex, TX, CA19.9, Stage)
dat.dplyr3<-dat.dplyr2 %>% filter(age>=40)
dat.dplyr4<-dat.dplyr3 %>% mutate(CA19.9=replace(CA19.9, CA19.9=="<1.0", "1"))

###################

dat.base5<-dat.base4
dat.base5$age50<-ifelse(dat.base5$age>=50, 1, 0)
dat.base5$age.grp<-cut(dat.base5$age, breaks=c(0, 50, 60, 70, Inf))

dat.dplyr5<-dat.dplyr4 %>% mutate(age50=ifelse(age>=50, 1, 0))
dat.dplyr6<-dat.dplyr5 %>% mutate(age.grp=cut(age, 
                                  breaks=c(0, 50, 60, 70, Inf)))


summary(dat.base5)
dat.base5$CA19.9<-as.double(dat.base5$CA19.9)

dat.base5$sex<-as.factor(dat.base5$sex)
dat.base5$TX<-as.factor(dat.base5$TX)
dat.base5$Stage<-as.factor(dat.base5$Stage)
dat.base5$age50<-as.factor(dat.base5$age50)

summary(dat.base5)


summary(dat.dplyr6)
dat.dplyr7<-dat.dplyr6 %>% mutate(CA19.9=as.double(CA19.9))
dat.dplyr8<-dat.dplyr7 %>% mutate_at(vars(sex, TX, Stage, age50), 
                                     as.factor)

summary(dat.dplyr8)

########## multiple tasks in a row

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

