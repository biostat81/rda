---
title: "R 데이터분석 6장"
author: "YDL"
date: "3/13/2022"
output: html_document
---

<br><br>

# 한국방송대, R 데이터분석, 제6장 R 실행 코드


<br><br>

### loading basic libraries

<br>

다음 패키지들은 6장 내용에서 자주 사용되는 패키지들로 미리 설치하고, library 함수로 search list 에 올려 주는 것이 좋습니다.

<br>
 
```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(patchwork)
library(GGally)
library(scales)
library(MASS)
```


<br><br>



## 6. 일반화선형모형

<br><br>

####  trees 자료

<br>

```{r}
dim(trees)
names(trees)
```

```{r}
# View(trees)
```

```{r}
ggpairs(trees)
```

<br><br>

#### 그림 6.1

<br>

```{r}
pp_601a <- ggplot(trees)+geom_point(aes(Girth,Volume))
pp_601b <- ggplot(trees)+geom_point(aes(Height,Volume))

pp_601  <- pp_601a + pp_601b
```

```{r}
pp_601
```

<br><br>

####  R 6.1

<br>

```{r}
lm(log(Volume)~log(Girth)+log(Height),trees)
```

<br>

```{r}
options()$contrasts
default <- options()$contrasts

options(contrasts=c("contr.sum","contr.poly"))
options()$contrasts

options(contrasts=default)
options()$contrasts
```

<br><br>

#### R 6.2

<br>

```{r}
contr.treatment(3)
contr.treatment(3,2)
contr.SAS(3)
contr.sum(3)
contr.helmert(3)
```


<br><br>

#### 참고 그림

<br>

```{r}
pima <- MASS::Pima.tr 
dim(pima)
names(pima)

ggplot(pima)+
  geom_boxplot(aes(glu,type,col=type))+
  geom_point(aes(glu,type,col=type), size=1, alpha=0.4)
```

<br><br>

#### 그림 6.2 / R 6.3

<br>

```{r}
library(MASS)
pima <- Pima.tr 

( r_pima <- glm(type~glu, binomial, pima) )

gsx    <- with(pima, seq( min(glu), max(glu),l= 200))
df_glu <- data.frame(glu=gsx)
prd    <- predict(r_pima,newdata=df_glu,type="response")

ggplot()+
  geom_point(data=pima,aes(glu,type,col=type),alpha=0.4)+
  geom_line(aes(gsx,prd+1),col="blue")+
  theme(legend.position='none')

```


<br><br>

#### 그림 6.3 / R 6.4

<br>

```{r}
x <- 1:14
y <- c(0,1,2,3,1,4,9,18,23,31,20,25,37,45)

aids<- data.frame(x=x,y=y)

r_aids <- glm(y~x,family=poisson,data=aids)

ggplot(aids)+geom_point(aes(x,y))+
    geom_smooth( aes(x,y), formula="y~x", method=glm, 
         method.args = list(family = "poisson"), col="red", se=F)
```


<br>

위 내용을 간단하게 그리기

<br>

```{r}
x <- 1:14
y <- c(0,1,2,3,1,4,9,18,23,31,20,25,37,45)

aids<- data.frame(x=x,y=y)

( r_aids <- glm(y~x,family=poisson,data=aids) )
yhat<- predict(r_aids, type="response")

ggplot()+geom_point(aes(x,y))+
     geom_line(aes(x,yhat), col="red")
```

<br> <br>

#### 그림 6.4

<br>

```{r}
x <- (-500:500)/100
y <-exp(x)/(1+exp(x))

ggplot()+geom_line(aes(x,y))
```

<br><br>

#### R 6.5

<br>

```{r}
pima <- MASS::Pima.tr 
r_pima_3 <- glm(type~glu+npreg+age, binomial, pima)
anova(r_pima_3)
```

<br><br>

#### 표 6.4 / R 6.6

<br>

```{r}
deviance(r_aids)      # deviance
( dfr <- r_aids$df.residual )    # resid df
deviance(r_aids) / dfr
rs1 <- resid(r_aids)  # deviance residuals
sum(rs1*rs1)          # deviance
rs2 <- resid(r_aids, "pearson")  # pearson resid
sum(rs2*rs2)/ dfr
```

<br><br>

#### R 6.7 /  R 6.8  /  R 6.9

<br>

```
{r}
lm(formula = “y ~ x+v”)
lm(formula = y ~ x+v)
lm(y ~ x+v)
```
<br>

```{r}
x <-  1:10
y <-  1+2*x+rnorm(10)
# (A, B) and (C, D) show the same results
lm(y ~ x)    	#  A 
lm(y ~ 1+x)  	#  B
lm(y ~ 0+x)  	#  C
lm(y ~-1+x) 	#  D
```

<br>

```{r}
sex<-factor(rep(c("M","F"), e=8))
type<-factor(rep(c("A","B","O","AB"), 4))
score<- 10*c(rnorm(8,0.5), rnorm(8,1))
lm(score ~ sex + type )               	# 교호작용이 없는 모형 
lm(score ~ sex + type + sex:type )    	# 교호작용이 있는 모형
lm(score ~ sex*type )                 	# 교호작용이 있는 모형
```

<br>

```{r}
lm( y ~ x + I(1/x) )  
lm( y ~ x + offset(2/x) )  
```

<br>

```{r}
lm(Volume ~ .^2, data = trees)
```

<br><br>

#### R 6.10

<br>

```{r}
names(iris) <- c('sl','sw','pl','pw','sp')
levels(iris$sp)<-c("st","vc","vg")
iris[c(49:50, 99:100, 149:150),]
```


<br><br>

#### R 6.11 / 표 6.5 / R 6.12

<br>

```{r}
M1  <- lm(sl~sp+sw-1, iris)
M1a <- lm(sl~sp+sw, iris)

summary(M1)
summary(M1a) 
```


```{r}
anova(M1)
anova(M1a) 
```



<br>

#### R 6.13 / 표 6.6 

<br>

```{r}
( M2  <- lm(sl~sp/sw-1, iris) ) 
( M2a <- lm(sl~sp/sw, iris)   )  # sl ~ sp+sp/sw
( M2b <- lm(sl~sp*sw, iris)   )
```


```{r}
coef(M2)
coef(M2b) 
```


<br>

#### R 6.14

<br>

```{r}
anova(M2)
anova(M2b) 
```


<br>

#### R 6.15

<br>

```{r}
c(AIC(M1), AIC(M2))
c(BIC(M1), BIC(M2))
```


```{r}
rs <- resid(M1)
n  <- length(rs)
sig2 <- sum(rs*rs)/n
ss   <- n*(1+log(2*pi*sig2))
( aic_M1 <- ss + 2*(4+1) )
( bic_M1 <- ss + log(n)*(4+1) )
```


<br>

#### 그림 6.5  / R 6.16

<br>

```{r}
library(tidyverse)
library(patchwork)

sw_range = with(iris, do.call( rbind, by(sw, sp, range)))
sw_range

prd= data.frame(sw=c(t(sw_range))) 
prd$sp = rep(levels(iris$sp),e=2)
prd$sl = predict(M1,newdata=prd)

prd


pp <- ggplot(iris,aes(sw,sl,col=sp))+geom_point()

pp_M1 <- pp+geom_line(data=prd, size=1)+ 
         labs(title="M1")+
         theme(legend.position='none')

pp_M2 <- pp+geom_smooth(method=lm,formula=y~x, se=F)+
         labs(title="M2")

pp_M1 + pp_M2 

```



<br>

#### R 6.17 / 표 6.8

<br>

```{r}
ldose <- rep(0:5, 2)
ndead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M","F"),e=6))
nda<-cbind(ndead, alive=20-ndead)
xd<-data.frame(ldose, ldose2=ldose-2, sex)

( M3 <- glm( nda~sex*ldose, data=xd, family = binomial) )
( M4 <- glm( nda~sex+ldose, data=xd, family = binomial) )
( M5 <- glm( nda~sex*ldose2, data=xd, family = binomial))
```



<br><br>

#### 그림 6.6 / R 6.18

<br>

```{r}
           
lds0<- rep(seq(min(ldose),max(ldose),l=200),2)
sx0 <- factor(rep(c("M","F"),e=200))
xd0 <- data.frame(ldose=lds0, ldose2=lds0-2, sex=sx0) 
         
prd_M3<- predict(M3,newdata=xd0,type="response")         
prd_M4<- predict(M4,newdata=xd0,type="response")         


pp <- ggplot()+
  geom_point(aes(ldose,ndead/20,col=sex))+
  labs(y="Probability")
  
pp_M3_0 <- pp + geom_line(data=xd0,
              aes(ldose,prd_M3,col=sex))+
          theme(legend.position='none')+
          labs(title="M3")

pt0 <- data.frame(x=c(0,0),y=c(prd_M3[201], prd_M3[2]))
pt2 <- data.frame(x=c(2,2),y=c(prd_M3[281], prd_M3[80]))

pp_M3 <- pp_M3_0 +
         geom_line(data=pt2,aes(x,y),col="orange",size=2)+
         geom_line(data=pt0,aes(x,y),col="orange",size=3)

pp_M4 <- pp + geom_line(data=xd0,
              aes(ldose,prd_M4,col=sex))+
        labs(title="M4")
  
pp_M3 + pp_M4 + plot_layout(guides = 'collect')

```


<br>

#### R 6.19

<br>

```{r}
summary(M3)
summary(M5)
```

<br><br>


#### R 6.20

<br>

```{r}
library(MASS)

names(crabs) <- tolower(names(crabs))
levels(crabs$sp)
( r_crabs <- glm(sp~cw+cl, binomial, crabs)  )
```

<br><br>

#### 그림 6.7 / R 6.21

<br>

```{r}
library(tidyverse)
library(patchwork)
library(scales)

y    <- as.numeric(crabs$sp)-1
eta  <- predict(r_crabs,type="link")
x    <- seq(min(eta),max(eta),l=400)
prob <- exp(x)/(1+exp(x))

pp1 <- ggplot(crabs,aes(cw,cl,col=sp))+
       geom_point(size=1)+
       scale_color_manual(values=hue_pal()(2)[c(2,1)])

pp2 <- ggplot()+
       geom_point(aes(eta,y,col=factor(y)),alpha=0.2)+
       geom_line(aes(x,prob))+
       labs(x="linear predictor",y="probability")+
       scale_color_manual(values=hue_pal()(2)[c(2,1)])+
       theme(legend.position='none')
  
pp1 + pp2 + plot_layout(guides = 'collect')

```



<br>

#### R 6.22

<br>

```{r}
library(MASS)

table(snails$Deaths)

r_snails <- glm(
              cbind(Deaths,N-Deaths) ~., 
              quasibinomial,
              snails
              )

summary(r_snails)

deviance(r_snails)
deviance(r_snails)/r_snails$df.residual
```


<br><br>

#### R 6.23

<br>

```{r}
library(tidyverse)
library(MASS)

pima <- Pima.tr

r_pima_0 <- glm(type~glu+bp+skin, binomial,pima)
r_pima_a <- step(r_pima_0) 
r_pima_b <- step(r_pima_0,k=log(200))

c(AIC(r_pima_0), BIC(r_pima_0))
c(AIC(r_pima_a), BIC(r_pima_a))
c(AIC(r_pima_b), BIC(r_pima_b))
```


<br> <br>

<br> <br>

