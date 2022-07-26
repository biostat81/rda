---
title: "R 데이터분석 7장"
author: "YDL"
date: "3/13/2022"
output: html_document
---


<br><br>

# 한국방송대, R 데이터분석, 제7장 R 실행 코드


<br><br>
Import utility functions
<br>
```{r}
source("util_functions.R")
```

<br> <br>

<br> <br>

## 7. 분류


<br><br>

### 7.1 LRA

<br><br>

#### R 7.1

<br>

```{r}

library(nnet)

names(iris) <- c('sl','sw','pl','pw','sp')
levels(iris$sp)<-c('st','vc','vg')

r4_LRA <- multinom(sp~.,iris, maxit=300)
r3_LRA <- step(r4_LRA,  trace=0)

AIC(r4_LRA)
AIC(r3_LRA)

round(coef(r4_LRA),3)
round(coef(r3_LRA),3)

```

<br>



<br>



<br><br>

#### 그림 7.1 /  R 7.2

<br>

```{r}
library(nnet)

r_LRA <- multinom(sp ~ sw+sl, data=iris, maxit=300)

summary(r_LRA)

# 'Draw' is in appendix

p1 <- Draw(r_LRA)+labs(title="LRA")

cm <- matrix(0,3,3)
cm[1:2,] <- coef(r_LRA)
cm[3,]   <- cm[2,] - cm[1,]
cm[,3]   <- -1*cm[,3]

p2 <- Draw(r_LRA)+labs(title="LRA")
colx <- c("red","blue","green")

for(k in 1:3){
   p2 <- p2 + 
         geom_abline(intercept=cm[k,1]/cm[k,3],
         slope=cm[k,2]/cm[k,3],col=colx[k],size=1) 
}

p1+p2+plot_layout(guides="collect")

round(coef(r_LRA),3)
```



<br><br>

#### R 7.3  / 표 7.1

<br>

```{r}
library(MASS)
library(nnet)

# buid crabx data

names(crabs)<- tolower(names(crabs))
crabx <- crabs
crabx$spsex <- as.factor(paste0(crabs$sp, crabs$sex))
crabx <- crabx[,-3]
head(crabx,2)
table(crabx$spsex)

# spsex~ fl+rw+cl+cw

rx_LRA <- multinom( spsex~ fl+rw+cl+cw, crabx, maxit=500)
px_LRA <- predict(rx_LRA,type="class")

# Model selection by AIC; # sp  ~ fl+cw  # sex ~ fl+cl+rw
# We will use, sex ~ cl+rw, for comparison

rsp0_LRA <- multinom(sp~fl+rw+cl+cw,crabs,maxit=30000,trace=FALSE)
rsp_LRA  <- step(rsp0_LRA, trace=FALSE)
rsx_LRA  <- multinom(sex~cl+rw, crabs,trace=FALSE)

psp_LRA <- predict(rsp_LRA,type="class")
psx_LRA <- predict(rsx_LRA,type="class")

# combined predictor

ps_LRA <- as.factor(paste0(psp_LRA,psx_LRA))

# coefficients

round(coef(rx_LRA),2)
round(coef(rsp_LRA),2)
round(coef(rsx_LRA),2)

# confusion matrix

table(crabx$spsex, px_LRA)

table(crabs$sp,  psp_LRA)
table(crabs$sex, psx_LRA)
table(crabx$spsex, ps_LRA)

# AIC comparison

round(c(AIC(rx_LRA), AIC(rsp_LRA), AIC(rsx_LRA)),2)
```

<br><br>

### 7.2 LDA QDA

<br><br>

#### R 7.4 / 그림 7.2

<br>

```{r}
library(MASS)

( r4_LDA <- lda(sp~.,iris) )
( r4_QDA <- qda(sp~.,iris) )

dsc <- as.matrix(iris[,1:4]) %*% coef(r4_LDA) 
LD1 <- dsc[,"LD1"]
sp  <- iris$sp

ggplot()+
  geom_boxplot(aes(x=LD1,y=sp, col=sp))+
  geom_jitter(aes(x=LD1,y=sp, col=sp), 
              size=0.6, height = 0.1, alpha=0.6)+
  labs(x="LD1",fill="sp")

```

<br><br>

#### R 7.5 / 표 7.2

<br>

```{r}
p4_LDA <- predict(r4_LDA)$class
p4_QDA <- predict(r4_QDA)$class
p4_LRA <- predict(r4_LRA,type="class")

sp_list <- list(p4_LDA, p4_QDA, p4_LRA)
lapply( sp_list, function(x) table(iris$sp,x))
```



<br><br>

#### 그림 7.3 / 표 7.3 / R 7.6

<br>

```{r}
library(MASS)

r_LDA <- lda(sp~sw+sl,iris)
r_QDA <- qda(sp~sw+sl,iris) 

# 'Draw' is in appendix

p1 <- Draw(r_LDA)+labs(title="LDA")  
p2 <- Draw(r_QDA)+labs(title="QDA")  

p1 + p2 + plot_layout(guides="collect")

p_LDA <- predict(r_LDA)$class
p_QDA <- predict(r_QDA)$class
p_LRA <- predict(r_LRA, type="class")

sp_list <- list( p_LDA, p_QDA, p_LRA)
lapply( sp_list, function(x) table(iris$sp,x))

```



<br><br>

#### R 7.7 / 표 7.4 / 그림 7.4  

<br>

```{r}
library(scales)
library(patchwork)

# spsex ~ fl+rw+cl+cw

( rx_LDA <- lda(spsex~fl+rw+cl+cw, crabx) )
px_LDA <- predict(rx_LDA)$class 

# spsex ~ LD1 + LD2

dsc <- as.matrix(crabx[,3:6]) %*% coef(rx_LDA) 
LD1  <- dsc[,1]
LD2  <- dsc[,2]

crabz <- data.frame(LD1,LD2,crabx)

( rz_LDA <- lda(spsex~LD1+LD2,crabz) )
pz_LDA <- predict(rz_LDA)$class 

# sp ~ fl + cw

( rsp_LDA <- lda(sp ~ cw+fl, crabs) )
psp_LDA <- predict(rsp_LDA)$class 

# sex ~ cl + rw

( rsx_LDA <- lda(sex ~ rw+cl, crabs) )
psx_LDA <- predict(rsx_LDA)$class 

# Combining, (sp ~ fl +cw ) & (sex ~ cl + rw )

ps_LDA <- paste0(psp_LDA,psx_LDA)


# confusion matrix

table(crabx$spsex,px_LDA)
table(crabz$spsex,pz_LDA)
table(crabs$sp,psp_LDA)
table(crabs$sex,psx_LDA)
table(crabx$spsex,ps_LDA)

# plots

p1 <- Draw(rz_LDA)+labs(title="spsex")+
      scale_color_manual(values=hue_pal()(4)[c(3,2,1,4)])
p2 <- Draw(rsp_LDA)+labs(title="sp")+
      scale_color_manual(values=hue_pal()(2)[c(2,1)])
p3 <- Draw(rsx_LDA)+labs(title="sex")+
      scale_color_manual(values=hue_pal()(4)[c(4,2)])

p1+p2+p3+plot_layout(guides="collect")
```


<br> <br>



### 7.3 NNET


<br><br>

#### 그림 7.5

<br>


![신경망](ref/nnet_iris.png)


<br><br>

#### R 7.8

<br>

```{r}
library(nnet)
 
r_NNET <- nnet(sp~.,iris,size=2,
               decay=0.01, maxit=300)
p_NNET <- predict(r_NNET, type="class")

table(iris$sp, p_NNET)

rx_NNET <- nnet(spsex~fl+rw+cl+cw,crabx, size=2,
               decay=0.01, maxit=300)
px_NNET <- predict(rx_NNET, type="class")

table(crabx$spsex, px_NNET)
```

<br> <br>

#### R 7.9 / 그림 7.6

<br>

```{r}
library(nnet)

rA_NNET <- nnet(sp~sw+sl, iris, 
               size=2, decay=0.01, maxit=300)

rB_NNET <- nnet(sp~sw+sl, iris, 
               size=8, decay=0, maxit=300)

rC_NNET <- nnet(sp~sw+sl, iris, 
               size=8, decay=0.01, maxit=300)

# 'Draw' is in appendix

p1 <- Draw(rA_NNET)+
      labs(title="NNET, (2, 0.01)")  

p2 <- Draw(rB_NNET)+
      labs(title="NNET, (8, 0.00)")  

p3 <- Draw(rC_NNET)+
      labs(title="NNET, (8, 0.01)")  

p1+p2+p3+plot_layout(guides="collect")

```


<br><br>

#### R 7.10

<br>

```{r}
library(GGally)

# flea data

dim(flea)
names(flea)
table(flea$species)
names(flea) <- c('sp','t1','t2','hd','a1','a2','a3')
levels(flea$sp) <- c('cn','hk','hp')
table(flea$sp)
```


<br><br>

#### R 7.11 / 표 7.5 / 그림 7.7

<br>

```{r}
library(nnet)

# model fitting

rf_LRA  <- multinom(sp ~ hd+a2, flea, maxit=300)
rf_QDA  <- qda(sp ~ hd+a2,flea)
rf_NNET <- nnet(sp ~ hd+a2, flea, 
                size=2, decay=0.001, maxit=300)

# prediction

pf_LRA <-  predict(rf_LRA, type="class")
pf_QDA <-  predict(rf_QDA)$class
pf_NNET <- predict(rf_LRA, type="class")

# confusion matrix

table(flea$sp, pf_LRA)
table(flea$sp, pf_QDA)
table(flea$sp, pf_NNET)

# plots

# 'Draw' is in appendix

p1<- Draw(rf_LRA)+labs(title="LRA")
p2<- Draw(rf_QDA)+labs(title="QDA")
p3<- Draw(rf_NNET)+labs(title="NNET")

p1+p2+p3+plot_layout(guides="collect")
```


<br> <br>

### 7.4 TREE

<br><br>

#### R 7.12 / 그림 7.8  위 그림, 아래 그림

<br>

```{r}
library(rpart)
library(partykit)


# cp = 0.01

rkt <- rpart( Kyphosis ~ Age+Number+Start,
              data = kyphosis)  

( rk1_TREE <- as.party(rkt) )

pk1_TREE <- predict(rk1_TREE)


# cp = 0.05

rkt <- rpart( Kyphosis ~ Age+Number+Start,
              data = kyphosis, cp=0.05 ) 

( rk5_TREE <- as.party( rkt ) )

pk5_TREE <- predict(rk5_TREE)

# confusion matrix

table(kyphosis$Kyphosis, pk1_TREE)
table(kyphosis$Kyphosis, pk5_TREE)

# tree plot

plot(rk1_TREE)
plot(rk5_TREE)

```


<br><br>

#### R 7.13 / 그림 7.9

<br>

```{r}
library(rpart)
library(partykit)

( r4_TREE <- as.party(rpart(sp~ sl+sw+pl+pw, iris)) )
p4_TREE <- predict(r4_TREE)

table(iris$sp, p4_TREE)

plot(r4_TREE)
```

<br> <br>

####  R 7.14 / 그림 7.10

<br>

```{r}

( r1_TREE <- as.party(rpart(sp~ sw+sl, iris)) )
p1_TREE <- predict(r1_TREE)

table(iris$sp, p1_TREE)

# 'Draw' is in appendix

p1 <- Draw(r1_TREE)+labs(title ="TREE with sw, sl")

r2_TREE <- as.party(rpart(sp~ pl+pw, iris))
p2_TREE <- predict(r2_TREE)

p2 <- Draw(r2_TREE,axis=0)+
      labs(title ="TREE with pw, pl")

r3_TREE <- as.party(rpart(sp~ pw+pl, iris))
p3_TREE <- predict(r3_TREE)

p3 <- Draw(r3_TREE)+
      labs(title ="TREE with pw, pl")

p1+p2+p3+plot_layout(guides="collect")

```

<br><br>

#### R 7.15 / 표 7.6

<br>

```{r}
library(rpart)
library(partykit)

# cp = 0.01

rxt <- rpart(spsex~ fl+rw+cl+cw, crabx)
rx1_TREE <- as.party(rxt)

px1_TREE <- predict(rx1_TREE)

# cp = 0.05

rxt <- rpart(spsex~ fl+rw+cl+cw, crabx,cp=0.05)
( rx5_TREE <- as.party(rxt)  ) 

px5_TREE <- predict(rx5_TREE)

# confusion matrix

x1 <- table(crabx$spsex, px1_TREE); x1
x5 <- table(crabx$spsex, px5_TREE); x5

# accuracy

sum(diag(x1))/sum(x1)
sum(diag(x5))/sum(x5)

# plots

plot(rx1_TREE)
plot(rx5_TREE)

```

<br> <br>

##### 참고그림

<br> 

```{r}
library(rpart)
library(partykit)

# cp = 0.01

rxt <- rpart(sp~ fl+cw, crabx)
rsp_TREE <- as.party(rxt)

psp_TREE <- predict(rsp_TREE)

p1<- Draw(rsp_TREE)+labs(title="TREE sp")

rxt <- rpart(sex~ cl+rw, crabx)
rsx_TREE <- as.party(rxt)

psx_TREE <- predict(rsx_TREE)

p2<- Draw(rsx_TREE)+labs(title="TREE sex")


table(crabx$sp, psp_TREE)
table(crabx$sex, psx_TREE)

p1+p2+plot_layout(guides="collect")

```


<br> <br>

#### R 7.16

<br>

```{r}
library(palmerpenguins)

dim(penguins)
names(penguins)<-c('sp','is','bl','bd','fl','ms','sx','yr')

pengs<- na.omit(penguins)

dim(pengs)

with(pengs, table(sp,is))
```

<br> <br>

#### R 7.17 / 그림 7.11

<br>


```{r}
rp_TREE <- as.party(rpart(sp~bl+bd+fl,pengs))
pp_TREE <- predict(rp_TREE)

table(pengs$sp,pp_TREE)

plot(rp_TREE)

( rp_LRA <- multinom(sp~bl+bd+fl,pengs,maxit=300) )

pp_LRA <- predict(rp_LRA)

table(pengs$sp,pp_LRA)

```


<br> <br>

### 7.5 SVM

<br> <br>

#### 그림 7.12

<br>

```{r}
x  <- seq(-3,5,by=0.01)
y1 <- log(1+exp(x))
y2 <- pmax(0,1+x)

ggplot()+
  geom_hline(yintercept=0)+ 
  geom_vline(xintercept=0)+
  geom_line(aes(x,y1),col="blue", size=0.8)+
  geom_line(aes(x,y2),col="brown", size=0.8)+
  annotate("text", x=3, y=2, label="SoftPlus") +
  annotate("text", x=2, y=4, label="max(0,1+x)")
```

<br> <br>

#### R 7.18 / 표 5.7 /그림 7.13

<br> 

```{r}
library(e1071)

r_tune <- tune(svm, sp~sw+sl, data=iris,    
              range=list(gamma=2^(-10:10),cost=2^(-10:10)),
              tunecontrol=tune.control(sampling="bootstrap"))

( pm <- as.vector(t(r_tune$best.parameters)) )

r1_SVM <- svm(sp~sw+sl, iris, gamma=pm[1],  cost=pm[2])
r2_SVM <- svm(sp~sw+sl, iris, gamma=pm[1]*100,cost=pm[2])
r3_SVM <- svm(sp~sw+sl, iris, gamma=pm[1],cost=pm[2]*5000)

p1_SVM <- predict(r1_SVM)
p2_SVM <- predict(r2_SVM)
p3_SVM <- predict(r3_SVM)

table(iris$sp, p1_SVM)
table(iris$sp, p2_SVM)
table(iris$sp, p3_SVM)

# 'Draw' is in appendix

pp1<- Draw(r1_SVM)+labs(title="Case I")
pp2<- Draw(r2_SVM)+labs(title="Case II")
pp3<- Draw(r3_SVM)+labs(title="Case III")

(pp1+pp2+pp3)+
  plot_layout(guides="collect")
```


<br> <br>


#### R 7.19

<br> 

* e1071 이용

<br> 

```{r}

r_tune <- tune(svm, spsex~fl+rw+cl+cw, data=crabx,    
              range=list(gamma=2^(-10:10),cost=2^(-10:10)),
              tunecontrol=tune.control(sampling="bootstrap"))

( pm <- as.vector(t(r_tune$best.parameters)) )

r_SVM <- svm(spsex~fl+rw+cl+cw, crabx, gamma=pm[1], cost=pm[2])
p_SVM <- predict(r_SVM)

table(crabx$spsex, p_SVM)
```

<br> <br> 


#### R 7.20 / 그림 7.14

<br> 

```{r}

# 'gen_dnut' is in appendix

dnut <- gen_dnut()

head(dnut)
table(dnut$sp)

ggplot(dnut)+geom_point(aes(x,y,col=sp))

```

<br> <br>

#### R 7.21 / 표 7.8 / 그림 7.15

<br> 

```{r}
rd_LRA  <- multinom(sp~x+y,dnut,maxit=300)
rd_NNET <- nnet(sp~x+y, dnut,size=2,decay=0.01)
rd_TREE <- as.party(rpart(sp~x+y, dnut))

r_tune <- tune(svm, sp~x+y, data=dnut,    
              range=list(gamma=2^(-10:10),cost=2^(-10:10)),
              tunecontrol=tune.control(sampling="bootstrap"))

( pm <- as.vector(t(r_tune$best.parameters)) )

rd_SVM <- svm(sp~x+y, dnut, gamma=pm[1], cost=pm[2])
```

<br> <br>


```{r,warning=FALSE}

# prediction

pd_LRA  <-predict(rd_LRA)
pd_NNET <-predict(rd_NNET,type="class")
pd_TREE <-predict(rd_TREE)
pd_SVM  <-predict(rd_SVM)

# confusion matrix

table(dnut$sp, pd_LRA)
table(dnut$sp, pd_NNET)
table(dnut$sp, pd_TREE)
table(dnut$sp, pd_SVM)

# 'Draw' is in appendix

p1 <- Draw(rd_LRA)+labs(title="LRA")
p2 <- Draw(rd_NNET)+labs(title="NNET")
p3 <- Draw(rd_TREE)+labs(title="TREE")
p4 <- Draw(rd_SVM)+labs(title="SVM")

(p1+p2)/(p3+p4)+plot_layout(guides="collect")

```

<br> <br>

##### 참고 그림

<br>

```{r}
rd_LDA  <- lda(sp~x+y,dnut)
pd_LDA  <- predict(rd_LRA)
table(dnut$sp, pd_LDA)
Draw(rd_LRA,np=160)+labs(tile="LDA")
```

<br> <br>

### 7.6 parameter tuning

<br> <br>


#### olivex 자료

<br>

```
{r}
install.packages("cepp")
```

<br><br>

#### R 7.22

<br>

```{r}
library(cepp)
library(tidyverse)

# preparing olive data

olive1 <- set_olive(1)
olive2 <- set_olive(2)

with(olive1,table(rg))
with(olive2,table(ar))
with(set_olive(),table(rg,ar))
```


<br> <br> 

#### R 7.23 /  표 7.9 / 그림 7.16

<br> 

* caret / kernlab 이용

<br>

```{r}
library(kernlab)
library(caret)

# fitting models
# rg~po+lo ;  palmitoleic (po) + linoleic(lo)

roc_LDA <- train(rg~po+lo, olive1, method="lda")
roc_NNET <- train(rg~po+lo, olive1, method="nnet",
                  maxit=300, trace=F)
roc_TREE <- train(rg~po+lo, olive1, method="rpart")
roc_SVM <- train(rg~po+lo, olive1, method="svmRadial")

( pm <- t(roc_TREE$bestTune) )

# confusion matrix

( x1<- table(olive1$rg, predict(roc_LDA)))
( x2<- table(olive1$rg, predict(roc_NNET)))
( x3<- table(olive1$rg, predict(roc_TREE)))
( x4<- table(olive1$rg, predict(roc_SVM)))

# accuracy

sum(diag(x1))/sum(x1)
sum(diag(x2))/sum(x2)
sum(diag(x3))/sum(x3)
sum(diag(x4))/sum(x4)

# plots

p1<- Draw(roc_LDA)+labs(title="LDA")
p2<- Draw(roc_NNET)+labs(title="NNET")
p3<- Draw(roc_TREE)+labs(title="TREE")
p4<- Draw(roc_SVM)+labs(title="SVM")

(p1+p2)/(p3+p4)+plot_layout(guides="collect")
```

<br> <br>

#### 표 7.10 up / 그림 7.17

<br>

```{r}
library(kernlab)
library(caret)

# fitting model:  
# ar ~ lo + es ; linoleic(lo), eicosenoic(es)

roc_LDA  <- train(ar~lo+es, olive2, method="lda")
roc_NNET <- train(ar~lo+es, olive2, method="nnet",
                  maxit=300, trace=F)
roc_TREE <- train(ar~lo+es, olive2, method="rpart")
roc_SVM  <- train(ar~lo+es, olive2, method="svmRadial")

( pm <- t(roc_TREE$bestTune) )


# confusion matrix

x1<- table(olive2$ar, predict(roc_LDA))
x2<- table(olive2$ar, predict(roc_NNET))
x3<- table(olive2$ar, predict(roc_TREE))
x4<- table(olive2$ar, predict(roc_SVM))


# accuracy

sum(diag(x1))/sum(x1)
sum(diag(x2))/sum(x2)
sum(diag(x3))/sum(x3)
sum(diag(x4))/sum(x4)


# accuracy

p1<- Draw(roc_LDA)+labs(title="LDA")
p2<- Draw(roc_NNET)+labs(title="NNET")
p3<- Draw(roc_TREE)+labs(title="TREE")
p4<- Draw(roc_SVM)+labs(title="SVM")


(p1+p2)/(p3+p4)+plot_layout(guides="collect")
```

<br> <br>

#### 표 7.10 down

<br>


```{r}
library(kernlab)
library(caret)

# fitting model:  ar ~ . 

roc_LDA  <- train(ar~., olive2, method="lda")
roc_NNET <- train(ar~., olive2, method="nnet",
                  maxit=300, trace=F)
roc_TREE <- train(ar~., olive2, method="rpart")
roc_SVM  <- train(ar~., olive2, method="svmRadial")

( pm <- t(roc_TREE$bestTune) )

# confusion matrix

x1<- table(olive2$ar, predict(roc_LDA))
x2<- table(olive2$ar, predict(roc_NNET))
x3<- table(olive2$ar, predict(roc_TREE))
x4<- table(olive2$ar, predict(roc_SVM))

# accuracy

sum(diag(x1))/sum(x1)
sum(diag(x2))/sum(x2)
sum(diag(x3))/sum(x3)
sum(diag(x4))/sum(x4)
```

<br> <br>

#### R 7.24 / 그림 7.18

<br> 

```{r}
library(rpart)
library(partykit)

# fitting model:  
# ar ~ lo + es ; linoleic(lo), eicosenoic(es)
# ar ~ .  ; 8 fatty acid

# TREE with cp=0.01

roc01a_TREE <- as.party(rpart(ar~lo+es, olive2, cp=0.01))
roc01b_TREE <- as.party(rpart(ar~., olive2, cp=0.01))

# confusion matrix

( x5a <- table(olive2$ar, predict(roc01a_TREE)))
( x5b <- table(olive2$ar, predict(roc01b_TREE)))

# accuracy

sum(diag(x5a))/sum(x5a)
sum(diag(x5b))/sum(x5b)

# plots

Draw(roc01a_TREE,np=160)+labs(title="TREE, cp=0.01")
```

<br> <br>

##### TREE 잘 나온 것으로 대체한 그림

<br>
```{r}

p5a<-Draw(roc01a_TREE,np=160)+labs(title="TREE")
(p1+p2)/(p5a+p4)+plot_layout(guides="collect")

```


<br> <br>
<br> <br>

