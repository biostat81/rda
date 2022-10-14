---
title: "5. R을 이용한 고급 그래픽 기법 "
output: word_document
---

# 5.1 빅데이터

```{r}

abalone<-read.table("./data/abalone.data",sep=",")
colnames(abalone) = c("sex","length","diameter","height",
                      "wholeW","shuckedW","visceraW","shellW","rings")
dim(abalone)
head(abalone)
```

## R 5.1  
```{r}
#library(devtools)
#install_github("mtennekes/tabplot")
```


# R 5.2
```{r}
library(ggplot2)
head(diamonds)
dim(diamonds)
```

# R 5.3  - 그림 5.1
```{r}
library(tabplot)
tableplot(diamonds)
```

# R 5.4  - 그림 5.2
```{r}
tableplot(diamonds,select=c(carat,cut,color,clarity,price),sortCol=price)
```

# R 5.5 - 그림 5.3
```{r}
library(ash)
test1<-ash1(bin1(diamonds$price,nbin=50),5)
plot(test1,type='l')
```

# R 5.6 - 그림 5.4
```{r}
library(hexbin)
library(grid)
x <- diamonds$carat
y <- diamonds$price
bin <- hexbin(x,y)
plot(bin,xlab="carat",ylab="price")
```

# R 5.7 - 그림 5.5
```{r}
smbin <- smooth.hexbin(bin)
plot(smbin,xlab="carat",ylab="price")
```

# R 5.8 - 그림 5.6
```{r}
pairs(abalone[,-1])
```

# R 5.9
```{r}
library(scagnostics)
scag.abalone<-scagnostics(abalone[,-1])
round(t(scag.abalone),2)
```

# R 5.10 - 그림 5.7
```{r}
pairs(t(scag.abalone))
```

# R 5.11
```{r}
Notnormal.plot<-scagnosticsOutliers(scag.abalone)
Notnormal.plot[Notnormal.plot]

round(scag.abalone[,Notnormal.plot],4)
```

# 그림 5.8
```{r}
library(ggplot2)
ggplot(abalone,aes(length, diameter))+geom_point()
ggplot(abalone,aes(height, rings))+geom_point()
```
