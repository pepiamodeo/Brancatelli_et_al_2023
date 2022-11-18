# library
library(popdemo)
source("./R/sup_correction.R") 

# create function for a truncated normal distribution constrained between 0 and 1

rnormt <- function(n, mean, sd) {
  
  range<-c(0,1)
  F.a <- pnorm(min(range), mean = mean, sd = sd)
  F.b <- pnorm(max(range), mean = mean, sd = sd)
  
  u <- runif(n, min = F.a, max = F.b)
  
  qnorm(u, mean = mean, sd = sd)
  
}


# matrix loading

matrix_load <- function(name,path){
  parameters<-read.csv(path,sep=";") 
  
  stg.ID<- c("SC","J1","J2","J3","J4","J5","J6","J7","A1","A2","A3","A4") 
  
  # parameters
  
  parameters[,2] 
  
  mu.r<-parameters[1,2]
  mu.g<-parameters[2,2]
  mu.e<-parameters[3,2]
  mu.s1<-parameters[4,2]
  mu.s2<-parameters[5,2]
  mu.s3<-parameters[6,2]
  mu.s4<-parameters[7,2]
  mu.s5<-parameters[8,2]
  mu.s6<-parameters[9,2]
  mu.s7<-parameters[10,2]
  
  #Adults (clases stage-based)  
  
  # adult annual survival (same in all classes)  
  mu.sa<-parameters[11,2] 
  
  d.sa1<-8 
  d.sa2<-9 
  d.sa3<-21 
  d.sa4<-45 
  
  # permanence in each class
  mu.sa1 <- sup_correction(sup=mu.sa,d=d.sa1)[[1]]
  mu.sa2 <- sup_correction(sup=mu.sa,d=d.sa2)[[1]]
  mu.sa3 <- sup_correction(sup=mu.sa,d=d.sa3)[[1]]
  mu.sa4 <- sup_correction(sup=mu.sa,d=d.sa4)[[1]]
  
  # growth in each class
  mu.sa1.2 <- sup_correction(sup=mu.sa,d=d.sa1)[[2]]
  mu.sa2.3 <- sup_correction(sup=mu.sa,d=d.sa2)[[2]]
  mu.sa3.4 <- sup_correction(sup=mu.sa,d=d.sa3)[[2]]
  
  # fertility
  
  mu.f1<-parameters[12,2]
  mu.f2<-parameters[13,2]
  mu.f3<-parameters[14,2]
  mu.f4<-parameters[15,2]
  
  #standard deviation for each parameter
  
  parameters[,3] 
  
  s.r<-parameters[1,3]
  s.g<-parameters[2,3]
  s.e<-parameters[3,3]
  s.s1<-parameters[4,3]
  s.s2<-parameters[5,3]
  s.s3<-parameters[6,3]
  s.s4<-parameters[7,3]
  s.s5<-parameters[8,3]
  s.s6<-parameters[9,3]
  s.s7<-parameters[10,3]
  
  s.sa<-parameters[11,3] # adult annual survival sd
  
  s.f1<-parameters[12,3]  
  s.f2<-parameters[13,3]
  s.f3<-parameters[14,3]
  s.f4<-parameters[15,3]
  
  # DETERMINISTIC MATRIX ####
  
  
  mat.m<-matrix(0,12,12,dimnames=list(stg.ID,stg.ID)) 
  
  mat.m[1,1]<-1-mu.r
  mat.m[2,1]<-mu.r * mu.g * mu.e
  mat.m[3,2]<-mu.s1
  mat.m[4,3]<-mu.s2
  mat.m[5,4]<-mu.s3
  mat.m[6,5]<-mu.s4
  mat.m[7,6]<-mu.s5
  mat.m[8,7]<-mu.s6
  mat.m[9,8]<-mu.s7
  
  mat.m[9,9] <- mu.sa1
  mat.m[10,10] <- mu.sa2
  mat.m[11,11] <- mu.sa3
  mat.m[12,12] <- mu.sa4
  
  mat.m[10,9] <- mu.sa1.2
  mat.m[11,10] <-mu.sa2.3
  mat.m[12,11] <-mu.sa3.4
  
  mat.m[1,9]<-mu.f1
  mat.m[1,10]<-mu.f2
  mat.m[1,11]<-mu.f3
  mat.m[1,12]<-mu.f4
  
  
  # STOCHASTIC MATRIX ####
  
  # function to build many probable matrices based on distributions  
  
  matrix.build<-function(){
    r<-rnormt(1,mean=mu.r,sd=s.r) 
    g<-rnormt(1,mean=mu.g,sd=s.g)
    e<-rnormt(1,mean=mu.e,sd=s.e)
    r.g.e <- r*g*e 
    
    #juveniles
    s1<-rnormt(1,mean=mu.s1,sd=s.s1)
    s2<-rnormt(1,mean=mu.s2,sd=s.s2)
    s3<-rnormt(1,mean=mu.s3,sd=s.s3)
    s4<-rnormt(1,mean=mu.s4,sd=s.s4)
    s5<-rnormt(1,mean=mu.s5,sd=s.s5)
    s6<-rnormt(1,mean=mu.s6,sd=s.s6)
    s7<-rnormt(1,mean=mu.s7,sd=s.s7)
    
    #adults (annual)
    sa1.a<-rnormt(1,mean=mu.sa,sd=s.sa) 
    sa2.a<-rnormt(1,mean=mu.sa,sd=s.sa)
    sa3.a<-rnormt(1,mean=mu.sa,sd=s.sa)
    sa4.a<-rnormt(1,mean=mu.sa,sd=s.sa)
    
    # transform from annual to stage (permanence)
    
    sa1 <- sup_correction(sup=sa1.a,d=d.sa1)[[1]]
    sa2 <- sup_correction(sup=sa2.a,d=d.sa2)[[1]]
    sa3 <- sup_correction(sup=sa3.a,d=d.sa3)[[1]]
    sa4 <- sup_correction(sup=sa4.a,d=d.sa4)[[1]]
    
    ######
    
    sa1.2 <- sup_correction(sup=sa1.a,d=d.sa1)[[2]]
    sa2.3 <- sup_correction(sup=sa2.a,d=d.sa2)[[2]]
    sa3.4 <- sup_correction(sup=sa3.a,d=d.sa3)[[2]]
    
    f1<-rnorm(1,mean=mu.f1,sd=s.f1)
    f2<-rnorm(1,mean=mu.f2,sd=s.f2)
    f3<-rnorm(1,mean=mu.f3,sd=s.f3)
    f4<-rnorm(1,mean=mu.f4,sd=s.f4)
    
    mat<-matrix(0,12,12,dimnames=list(stg.ID,stg.ID)) # creo una matriz en blanco
    
    mat[1,1]<-1-r
    mat[2,1]<-r.g.e
    mat[3,2]<-s1
    mat[4,3]<-s2
    mat[5,4]<-s3
    mat[6,5]<-s4
    mat[7,6]<-s5
    mat[8,7]<-s6
    mat[9,8]<-s7
    
    mat[9,9] <- sa1
    mat[10,10] <- sa2
    mat[11,11] <- sa3
    mat[12,12] <- sa4
    
    mat[10,9] <- sa1.2
    mat[11,10] <-sa2.3
    mat[12,11] <-sa3.4
    
    mat[1,9]<-f1
    mat[1,10]<-f2
    mat[1,11]<-f3
    mat[1,12]<-f4
    
    
    return(mat)
  }
  
  # matrix construction 
  
  matrix.build() 
  
  # matrix list
  
  mat.s<-list() 
  
  for (i in 1:150){
    mat.s[[i]]<-matrix.build()
  } 
  
  # those that do not comply with irreducibility are discarded  
  discard<-which(sapply(X=mat.s, FUN=function(x){isIrreducible(A=x)})=="FALSE")
  mat.s<-mat.s[-discard]
  
  mat.s <- mat.s[1:100] # select first hundred
  
  assign(x=paste("mat.m",name,sep="."),value=mat.m,envir = .GlobalEnv)
  assign(x=paste("mat.s",name,sep="."),value=mat.s,envir = .GlobalEnv)
  
}
