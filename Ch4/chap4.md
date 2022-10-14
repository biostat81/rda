---
title: '4. 통계 그래픽스 '
output: word_document
editor_options: 
  chunk_output_type: console
---

## R 4.1

```{r}
autompg <- read.table("./data/auto-mpg.data",na.strings="?")
colnames(autompg) = c("mpg","cylinder","displacement","horsepower",
                      "weight", "acceleration","year", "origin", "name")
dim(autompg)
head(autompg)
```

## R 4.2

```{r}
library(reshape2)
data(tips)
tips$tiprate <- tips$tip/tips$total_bill * 100
dim(tips)
head(tips)
```

## R 4.3

```{r}
abalone<-read.table("./data/abalone.data",sep=",")
colnames(abalone) = c("sex","length","diameter","height",
                      "wholeW","shuckedW","visceraW","shellW","rings")
dim(abalone)
head(abalone)
```

## R 4.4

```{r}
Pconsump<-read.csv("./data/power_consumption.csv") 
dim(Pconsump)
head(Pconsump)
```

## R 4.5  - 그림 4.1
```{r}
library(lattice)
barchart(as.factor(autompg$cylinder))
```

## R 4.6  - 그림 4.2
```{r}
barchart(as.factor(autompg$year),horizontal=FALSE)
```

## R 4.7  - 그림 4.3
```{r}
histogram(tips$tip)
histogram(tips$tip,nint=10)
```

## R 4.8  - 그림 4.4
```{r}
histogram(tips$tip, breaks = seq(0,11,0.5), main = "binwidth = 50 cent")
histogram(tips$tip, breaks = seq(0,11,0.25), main = "binwidth = 25 cent")
histogram(tips$tip, breaks = seq(0,11,0.1), main = "binwidth = 10 cent")
histogram(tips$tip, breaks = seq(0,11,0.05), main = "binwidth = 5 cent")
```

## R 4.9  - 그림 4.5
```{r}
densityplot(autompg$mpg, xlab="mpg")
```

## R 4.10  - 그림 4.6
```{r}
xyplot(tip ~ total_bill, pch=16, data = tips)
```

## R 4.11  - 그림 4.7
```{r}
xyplot(tip~total_bill | sex, pch=16, data = tips)
```

## R 4.12  - 그림 4.8
```{r}
xyplot(tip~total_bill | sex, group = smoker, pch = c(16,2), data = tips)
```

## R 4.13  - 그림 4.9
```{r}
xyplot(tip ~ total_bill | sex + smoker, data = tips,
        panel = function(x, y) {
            panel.grid(h = -1, v = 2)
            panel.xyplot(x, y, pch=16)
            panel.lmline(x, y)
        })
```


## R 4.14  - 그림 4.10
```{r}
bwplot(cylinder ~ mpg, data = autompg)
```

## R 4.15  - 그림 4.11
```{r}
bwplot(weight ~ cylinder, data = autompg, horizontal = FALSE)
```

## R 4.16 - 그림 4.12
```{r}
dotplot(cylinder ~ mpg, data = autompg)
```

## R 4.17 - 그림 4.13
```{r}
library(vcd)
mosaic(~ sex + size, data = tips)
```

## R 4.18 - 그림 4.14
```{r}
mosaic(~ size + sex, data = tips)
```

## R 4.19 - 그림 4.15
```{r}
cotabplot(~ size+sex | smoker, data = tips, panel = cotab_mosaic)
```


## R 4.20 - 그림 4.16
```{r}
splom(~autompg[,c(1,3:6)], data = autompg)
```


## R 4.21 - 그림 4.17
```{r}
splom(~autompg[,c(1,3:6)], groups = cylinder, data = autompg,
       col=c("red","orange","blue","green","grey50"),
       pch=c(16,2,15,3,1),cex=0.7,
       key = list(title = "Various cylinders in autompg",
                  columns = 5, 
                  points = list(pch =c(16,2,15,3,1),
                  col = c("red","orange","blue","green","grey50")),
                  text = list(c("3","4","5","6","8"))))
```

## R 4.22 - 그림 4.18
```{r}
parallelplot(~ autompg[,c(1,3:6)] ,data = autompg, horizontal=FALSE) 
```


## R 4.23 - 그림 4.19
```{r}
parallelplot(~ autompg[,c(1,3:6)] | as.factor(cylinder), data = autompg) 
```


## R 4.24 - 그림 4.20
```{r}
cloud(mpg ~ horsepower*displacement, data = autompg, 
                                      screen=list(x=-80,y=70))
```


## R 4.25 - 그림 4.21
```{r}
cloud(mpg ~ horsepower*displacement|as.factor(cylinder), data = autompg,
                      screen=list(x=-80,y=70))
```

## R 4.26 - 그림 4.22
```{r}
contour(volcano)
```

## R 4.27 - 그림 4.23
```{r}
wireframe(volcano)
```

## R 4.28 - 그림 4.24
```{r}
wireframe(volcano, shade = TRUE, light.source = c(10,0,10))
```

## R 4.29 
```{r}
#install.packages("ggplot2")
```

## R 4.30 
```{r}
library(ggplot2)
ggplot(data = abalone, aes(x=length,y=wholeW)) 

```

## R 4.31 - 그림 4.25
```{r}
ggplot(data = abalone, aes(x=length,y=wholeW)) + geom_point()

```

## R 4.32 - 그림 4.26
```{r}
ggplot(data = abalone, aes(x=length,y=wholeW)) + 
    geom_point(aes(color=sex,shape=sex))
```

## R 4.33 - 그림 4.27
```{r}
ggplot(data = abalone, aes(x=length,y=wholeW)) + 
    geom_point(alpha=0.1)

```

## R 4.34 - 그림 4.28
```{r}
ggplot(data = abalone, aes(x=sex,y=wholeW)) + 
    geom_boxplot()
```

## R 4.35 - 그림 4.29
```{r}
ggplot(data = abalone, aes(x=sex,y=wholeW)) + 
    geom_jitter()
```

## R 4.36 - 그림 4.30
```{r}
ggplot(data = abalone, aes(x=sex,y=wholeW)) + 
    geom_jitter(alpha=1/3)
```

## R 4.37 - 그림 4.31
```{r}
ggplot(data = abalone, aes(x=factor(rings),y=wholeW)) + 
    geom_boxplot()
```

## R 4.38 - 그림 4.32
```{r}
ggplot(data = abalone, aes(x=wholeW)) + 
    geom_histogram(binwidth=0.1)

```

## R 4.39 - 그림 4.33
```{r}
ggplot(data = abalone, aes(x=wholeW)) + 
    geom_histogram(binwidth=0.05) +
    facet_grid(~sex)

```

## R 4.40 - 그림 4.34
```{r}
ggplot(data = abalone, aes(x=wholeW)) + 
    geom_density(aes(color=sex,linetype=sex)) 
```

## R 4.41 
```{r}
class(Pconsump$Date)
class(Pconsump$Time)
Pconsump$newDate <- as.POSIXct(Pconsump$Date, format="%d/%m/%Y")
Pconsump$newTime <- as.POSIXct(Pconsump$Time,format="%H:%M:%S") 
Pconsump$year <-  format(Pconsump$newDate,"%Y")
```

## R 4.42 - 그림 4.35
```{r}
Pconsump.2006.12.17 <- Pconsump[Pconsump$Date == "17/12/2006",]
ggplot(Pconsump.2006.12.17,aes(newTime,X3))+
    geom_line()
```

## R 4.43 - 그림 4.36
```{r}
Pconsump.12.17<- Pconsump[(Pconsump$Date == "17/12/2006" |
                                Pconsump$Date == "17/12/2007"), ]
ggplot(Pconsump.12.17,aes(newTime,X3))+
    geom_line(aes(color=year,linetype=year))

```

## R 4.44 - 그림 4.37
```{r}
Pconsump.12<- Pconsump[(Pconsump$Date == "17/12/2006" |
                          Pconsump$Date == "18/12/2006" |
                          Pconsump$Date == "17/12/2007" |
                                Pconsump$Date == "18/12/2007"), ]
ggplot(Pconsump.12,aes(newTime,X3))+
    geom_line() +
    facet_wrap(~Date,ncol=1)

```


## R 4.45 - 그림 4.38

```{r}
ggplot(tips,aes(x=total_bill,y=tip,color=sex)) +
         geom_point()+
         geom_smooth()
```


## R 4.46 - 그림 4.39

```{r}
ggplot(tips,aes(x=total_bill,y=tip)) +
         geom_point(aes(color=sex))+
         geom_smooth()
```

## R 4.47 - 그림 4.40

```{r}
plot.stat<-ggplot(abalone,aes(x=rings,y=log(wholeW)))
plot.stat + geom_point(shape=1) + 
             stat_summary(size=3,shape=15,color="red",
                          fun = "mean", geom = "point") 
```

## R 4.48 - 그림 4.41

```{r}
plot.stat + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar")
```

## R 4.49 - 그림 4.42

```{r}
q1<-function(x) quantile(x,p=0.25)
q3<-function(x) quantile(x,p=0.75)
plot.stat + stat_summary(aes(color="Q1",shape="Q1"),fun=q1,geom="point") +
    stat_summary(aes(color="median",shape="median"),fun=median,geom="point") +
    stat_summary(aes(color="Q3",shape="Q3"),fun=q3,geom="point") +
    stat_summary(aes(color="min",shape="min"),fun=min,geom="point") +
    stat_summary(aes(color="max",shape="max"),fun=max,geom="point") +
    scale_color_hue("Quartile")+scale_shape("Quartile")
```

## R 4.50 - 그림 4.43

```{r}
plot.pos <- ggplot(tips,aes(x = day, fill = sex, shape = sex))
plot.pos + geom_bar(position = "stack") + ggtitle("stack") + 
            theme(legend.position="none")
plot.pos + geom_bar(position = "dodge") + ggtitle("dodge") + 
            theme(legend.position="none")
plot.pos + geom_bar(position = "fill") + ggtitle("fill") + 
          theme(legend.position="none")
plot.pos + geom_jitter(aes(y=total_bill,color=sex,shape=sex),
                       width=0.2,height=0) +
          ggtitle("jitter") + theme(legend.position="none")
plot.pos + geom_bar(position = "identity") + ggtitle("identity") + 
          theme(legend.position="none")
plot.pos + geom_bar(position = "identity", alpha = I(0.5)) + 
          ggtitle("identity with alpha") + 
          theme(legend.position="none")
```

## R 4.51 - 그림 4.44

```{r}
plot.scale1 <- ggplot(tips, aes(x=total_bill, y = tip, 
                                color = sex, shape = sex, size = size)) +
             geom_point()
plot.scale1 + scale_color_hue("Gender", labels = c("여자", "남자"))
```

## R 4.52 - 그림 4.45

```{r}
plot.scale1 + scale_color_brewer(palette="Set1")
```

## R 4.53 - 그림 4.46

```{r}
plot.scale2 <- ggplot(tips,aes(x=total_bill,y=tip))+geom_point()
plot.scale2 + scale_x_continuous(breaks=c(20,40)) +
             scale_y_continuous(breaks=1:10) 
```

## R 4.54 - 그림 4.47

```{r}
plot.facet <- ggplot(tips, aes(x = total_bill, y = tip)) + geom_point()
plot.facet + facet_grid(sex ~ smoker, margins = TRUE)
```

## R 4.55 - 그림 4.48

```{r}
plot.facet + facet_wrap(~ size, ncol = 6)
```

## R 4.56 - 그림 4.49

```{r}
tips$tipgroup1 <- cut_interval(tips$tiprate,n=3)
tips$tipgroup2 <- cut_number(tips$tiprate,n=3)
plot.newfacet<-ggplot(tips,aes(x=total_bill,y=tip))+geom_point()
plot.newfacet + facet_wrap(~tipgroup1)
```

## R 4.57 - 그림 4.50

```{r}
plot.newfacet + facet_wrap(~tipgroup2)

```

## R 4.58 - 그림 4.51

```{r}
plot.theme <- ggplot(tips, aes(x = total_bill, y = tip))
plot.theme + geom_point() + xlab("Total Bill") + 
            ylab("팁") + ggtitle("전제 금액과 팁")
```

## R 4.59 - 그림 4.52

```{r}
plot.theme + geom_point() + 
       labs(xlab="Total Bill",ylab="팁",title="전제 금액과 팁")+
       theme(plot.title = element_text(size = 10, 
                                  color = "red", face = "bold", hjust = 0.5))
 
```

## R 4.60 - 그림 4.53

```{r}
last_plot()+theme_bw()+
       theme(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             panel.border=element_blank(),
             axis.line = element_line())
```

## R 4.61 - 그림 4.54

```{r}
plot.theme + geom_point(aes(color = sex, shape = sex)) +
              theme(legend.position = "none")
```

## R 4.62

```{r}
ggsave("sample-plot.png")
```


