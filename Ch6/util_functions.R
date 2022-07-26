

install_packages_7 <- function(){
  
  installed_pkgs <- .packages(all = TRUE)
  
  pks <- c("tidyverse", "patchwork", "GGally")
  pks <- c(pks, "scales", "MASS", "nnet", "rpart")
  pks <- c(pks,"partykit", "e1071", "caret")
  pks <- c(pks, "kernlab", "cepp")
  
  install.packages(pks[!(pks %in% installed_pkgs)]) 
}



library_7 <- function(){
  
  require(tidyverse,quietly=T)
  require(patchwork,quietly=T)
  require(GGally,quietly=T)
  require(scales,quietly=T)
  require(MASS,quietly=T)
  
  require(nnet,quietly=T)
  require(rpart,quietly=T)
  require(partykit,quietly=T)
  require(e1071,quietly=T)
  require(caret,quietly=T)
  require(kernlab,quietly=T)
}


install_packages_7()  
library_7()  


type_fix <- function(res){
  type=class(res)
  if('multinom' %in% type) type='multinom' # nnet
  if('lda' %in% type) type='lda'     # MASS
  if('qda' %in% type) type='qda'     # MASS
  if('nnet' %in% type) type='nnet'   # nnet
  if('rpart' %in% type) type='rpart' # rpart
  if('party' %in% type) type='party' # partikit
  if('svm' %in% type) type='svm'     # e1071
  if('train' %in% type) type='train' # caret
  type
}


predict_cases<-function(res,new){
  
  type = type_fix(res)
  
  p_sp =rep( '0', nrow(new) )
  
  if(type %in% c('lda','qda')) p_sp  = 
    predict(res, newdata=new)$class  
  if(type %in% c('multinom','rpart','nnet')) p_sp  =  
    predict(res, newdata=new, type="class") 
  if(type %in% c('party','svm')) p_sp  =  
    as.vector(predict(res, newdata=new))
  if(type %in% c('train')) p_sp  =  
    predict(res, newdata=new, type="raw")
  as.character(p_sp)
}



is_a_leaf <-function(fm){
  
  out = TRUE
  if( is.call(fm) ) 
    if( length(fm)>2 )
      if( (fm[[1]]=='+')|(fm[[1]]=='-') )  out=FALSE
  out      
}  


leafs <- function(res){
  v = NULL
  fx = res
  while(!is_a_leaf(fx)){
    v = c(fx[[3]],v)
    fx = fx[[2]]
  }
  c(fx,v)
}



Draw <- function(res, border=1, axis=1, np=100){
  
  type = type_fix(res)
  
  callx  =  res$call
  df     =  callx[[3]]
  fm     =  callx[[2]]  
  
  if( type %in% c('party') ){
    df0    =  res$data
    df     =  as.name('df0')
    fm     =  res$terms       }
  
  vy     =  fm[[2]]
  terms  =  leafs(fm[[3]])
  cterms =  as.character(terms)
  pos    =  which(sapply(terms, is.name))
  
  q1     =  as.name(cterms[pos][1])
  q2     =  as.name(cterms[pos][2]) 
  
  if(axis==0){
    q1     =  as.name(cterms[pos][2])
    q2     =  as.name(cterms[pos][1])  }
  
  x1     =  eval(q1, eval(df))  
  x2     =  eval(q2, eval(df))
  
  x1q    =  seq( min(x1), max(x1), l=np)
  x2q    =  seq( min(x2), max(x2), l=np)
  
  new    =  expand.grid( x=x1q,  y=x2q )
  names(new) = as.character(c(q1,q2))
  
  cp_0   =  predict_cases(res,eval(df))
  cp_1   =  as.character(eval(vy, eval(df))) 
  p_1    =  factor(cp_1)
  dfz    =  eval(df)[cp_0!=cp_1,]
  dfz$p  =  p_1[cp_0!=cp_1]
  
  cp_sp  =  predict_cases(res,new)
  p_sp   =  factor(cp_sp, levels=levels(p_1))
  new_p  =  data.frame( new, p_sp )
  
  p<- eval( bquote( 
    ggplot(.(df)) + 
      geom_point( aes(.(q1),.(q2), col=.(vy)))+
      geom_point(data= new_p, 
                 aes(.(q1),.(q2), col=p_sp),size=0.1,alpha=0.2) +
      geom_point(data=dfz,
                 aes(.(q1),.(q2),col=p ),shape=1,size=4)+
      geom_point( aes(.(q1),.(q2), col=.(vy)), size=1.4)
    #+theme_light()
  ) )
  if(border==1){
    z_sp = factor(cp_sp)
    K = length(levels(z_sp))
    if(K>1){
      zzz<-diag(K)[z_sp,]
      for(k in 1:K){
        new_z <- new_p
        new_z$z <- zzz[,k]
        p<-p + eval( bquote( geom_contour(data=new_z,
                                          aes(.(q1),.(q2),z=z), 
                                          breaks=0.5, size=0.1, col="black") 
        ))
      }
    }
  }
  p
}


gen_doughnut <-function(n=10000, seed=777){
  set.seed(seed)
  s  <- 10
  xr <- 3;    yr <- 2
  sx <- xr*s; sy <- yr*s
  x <- sx*stats::runif(n)
  y <- sy*stats::runif(n)
  sp <- rep('green',n)
  sp[ (x-sx/2)^2/xr +(y-sy/2)^2/yr <= (1.0*s)^2/xr ] <- 'red' 
  sp[ (x-sx/2)^2/xr +(y-sy/2)^2/yr <= (0.6*s)^2/xr ] <- 'blue'
  sp <- factor(sp, levels=c('red','green','blue'))
  data.frame( sp, x, y )
}


gen_dnut <- function(ns=300){ 
  tmp <- gen_doughnut()
  tdf <- tmp[sample(1:nrow(tmp),ns),]
  
  # add noise
  u <- sample(1:ns, trunc(0.20*ns))
  tdf$sp[sort(u)] <- tdf$sp[u] 
  row.names(tdf)<- 1:nrow(tdf)
  tdf
}



set_olive <- function(inc=c(1,2)){
  
  require(tidyverse,quietly = T)
  
  if(!("cepp" %in% .packages(all = TRUE))) install.packages("cepp") 
  
  require(cepp,quietly = T)
  data(olive)
  
  olivex <- olive
  names(olivex)<-c("rg","ar","pt","po","st","ol","lo","ln","ac","es")
  
  rg          <- factor(olivex$rg)
  levels(rg)  <- c("S","R","N")
  olivex$rg   <- factor(rg,levels=c("S","R","N"))
  
  ar  <- factor(olivex$ar)
  arf <- factor(ar,levels=levels(ar)[c(6,1,7,5,2,4,8,3,9)])
  
  x  <- levels(arf) %>% 
    tolower() %>% 
    str_replace("east","e") %>% 
    str_replace("west","w") %>%
    str_replace("south","s") %>%
    str_replace("north","n") %>%
    str_replace("coast","c") %>%
    str_replace("inland","i") 
  
  y  <- str_split(x,"-",simplify=T)
  y1 <- str_sub(y[,1],1,4)
  y2 <- str_sub(y[,2],1,4)
  
  short <- paste0(c("S","S","S","S","R","R","N","N","N"),
                  "_", y1,"_", y2) %>% str_replace_all("_$","")
  
  levels(arf) <- short
  olivex$ar <- arf
  
  indx = c( intersect((1:2),inc), 3:10)
  olivex[,indx]
}
