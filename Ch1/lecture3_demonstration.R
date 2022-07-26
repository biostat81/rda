#### This file contains R codes used in Lecture 3, part 3

setwd("C:\\Users\\SYP\\R_exercise")

dat0<-read.csv("example_data.csv")
dat0<-read.csv("example_data.csv", nrow=52)

library(dplyr)

unique(c(1, 2, 3, 5, 5, 5, 5))
length(unique(dat0$id))
dim(dat0)

which(duplicated(dat0$id))
dat0[32, ]

dat1<-dat0 %>% distinct()
length(unique(dat1$id))
which(duplicated(dat1$id))
dat1$id[which(duplicated(dat1$id))]
dat1[dat1$id==41, ]

dat2<-dat1[-41, ]
length(unique(dat2$id))

summary(dat2)

dat3<-dat2 %>% rename(Recur=Recur..1..Recur..0..Censored.) %>%
              mutate(CEA=as.double(replace(CEA, 
                                           CEA=="na"|CEA==".", NA)), 
                     abnormal=ifelse(CEA>=100, 1, 0), 
                     CEA.group=cut(CEA, breaks=c(0, 5, 10, 100, Inf)))%>%
              mutate_at(vars(sex, Recur, local_6m, TNM, abnormal),
                        as.factor)
summary(dat3)
