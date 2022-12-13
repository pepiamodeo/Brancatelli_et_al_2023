
# library
library(popdemo)
source("./R/projection_control_fire.R")

# function to create stochastic projection and obtain the outcomes
# removal = fixed number of individuals of class adult 1 removed every year

proj.stoch.control.fire<- function(list.mat, ini.vec,iterations,Aseq,removal){
  
  #Check primitivity, irreducibility and ergodicity
  
  print(which(sapply(X=list.mat, FUN=function(x){isPrimitive(A=x)})=="FALSE"))
  print(which(sapply(X=list.mat, FUN=function(x){isIrreducible(A=x)})=="FALSE"))
  print(which(sapply(X=list.mat, FUN=function(x){isErgodic(A=x)})=="FALSE"))
  
#stochastic growth rate
  
  discard<-100
  
  proj2000<-project.control.fire(list.mat=list.mat,
                            ini.vec = ini.vec,
                            removal=removal,
                            time=iterations)

  #work out per-timestep growth
  discard<-100
  gr <- colSums(proj2000)[(discard:iterations) + 1] / colSums(proj2000)[discard:iterations]
  
  #find the per-timestep mean growth (stochastic growth)
  growth.rate <- data.frame(lambda=mean(gr),var=var(gr))

  #loop number of individuals at 50 and 100 years, total and adults (average of 2000 iterations)
  n.ad.50<-NA
  n.ad.100<-NA
  
  n.pl.100<-NA
  
  n.tot.50<-NA
  n.tot.100<-NA
  
  time.extinction.ad<-NA
  
  for(i in 1:2000){
    proj<-project.control.fire(list.mat, ini.vec,removal=removal) 
    n.tot.100[[i]]<-sum(proj[,100]) 
    n.tot.50[[i]]<-sum(proj[,50])
    n.pl.100[[i]]<-sum(proj[2:12,100])
    n.ad.50[[i]]<-sum(proj[9:12,50])
    n.ad.100[[i]]<-sum(proj[9:12,100]) 
    time.extinction.ad[i]<-min(which(rowSums(proj[9:12,])<ex.ad))
  }
  
  df.outcome<- data.frame(growth.rate,
                          summary.ntot.100=array(summary(n.tot.100)),# 100 years of the projection
                          summary.nad.100=array(summary(n.ad.100)),# adults at 100 years of the projection
                          summary.nad.50=array(summary(n.ad.50)),# adults at 50 years of the projection
                          prop.total.extinction=sum(n.tot.100<1)/2000,#proportion of iterations with total extinction at 100 years
                          prop.adult.extinction=sum(n.ad.100<ex.ad)/2000,#proportion of iterations with adult extinction at 100 years
                          summary.time.ad.ext=array(summary(time.extinction.ad)))# time to adult extinction
  
  #base projection
  proj <- project.control.fire(list.mat, ini.vec, removal=removal) 
  
  assign(x=paste("proj",name,sep=""),value=proj,envir = .GlobalEnv)
  assign(x=paste("df_outcome_",name,sep=""),value=df.outcome,envir = .GlobalEnv)
  
}
