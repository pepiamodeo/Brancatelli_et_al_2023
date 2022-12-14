
# library
library(popdemo)

# function to create stochastic projection and obtain the outcomes

proj.stoch<- function(list.mat, ini.vec,Aseq="unif",iterations){
  
  #Check primitivity, irreducibility and ergodicity
  
  print(which(sapply(X=list.mat, FUN=function(x){isPrimitive(A=x)})=="FALSE"))
  print(which(sapply(X=list.mat, FUN=function(x){isIrreducible(A=x)})=="FALSE"))
  print(which(sapply(X=list.mat, FUN=function(x){isErgodic(A=x)})=="FALSE"))
  
  # Stochastic projection
  
  #stochastic growth rate
  growth.rate<-stoch(list.mat, c("lambda", "var"), vector = ini.vec,
                     iterations = iterations, discard = 100, Aseq = Aseq)
  
  #loop number of individuals at 50 and 100 years, total and adults (average of 2000 iterations)
  n.ad.50<-NA
  n.ad.100<-NA
  
  n.pl.100<-NA
  
  n.tot.50<-NA
  n.tot.100<-NA
  
  time.extinction.ad<-NA
  
  for(i in 1:2000){
    proj<-project(list.mat, ini.vec,time=100,Aseq=Aseq) 
    n.tot.100[[i]]<-sum(vec(proj)[100,]) 
    n.tot.50[[i]]<-sum(vec(proj)[50,])
    n.pl.100[[i]]<-sum(vec(proj)[100,2:12])
    n.ad.50[[i]]<-sum(vec(proj)[50,9:12])
    n.ad.100[[i]]<-sum(vec(proj)[100,9:12]) 
    time.extinction.ad[i]<-min(which(rowSums(vec(proj)[,9:12])<ex.ad))
  }
  
  df.outcome<- data.frame(growth.rate,
                          summary.ntot.100=array(summary(n.tot.100)),# 100 years of the projection
                          summary.nad.100=array(summary(n.ad.100)),# adults at 100 years of the projection
                          summary.nad.50=array(summary(n.ad.50)),# adults at 50 years of the projection
                          prop.total.extinction=sum(n.tot.100<1)/2000,#proportion of iterations with total extinction at 100 years
                          prop.adult.extinction=sum(n.ad.100<ex.ad)/2000,#proportion of iterations with adult extinction at 100 years
                          summary.time.ad.ext=array(summary(time.extinction.ad)))# time to adult extinction
  
  #base projection
  proj <- project(list.mat, ini.vec, time = 100,Aseq=Aseq) 
  
  assign(x=paste("proj",name,sep=""),value=proj,envir = .GlobalEnv)
  assign(x=paste("df_outcome_",name,sep=""),value=df.outcome,envir = .GlobalEnv)
  
}
