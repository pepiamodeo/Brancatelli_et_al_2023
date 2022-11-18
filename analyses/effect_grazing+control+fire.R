#Library

library(ggplot2)
library(reshape2)

# load custom functions for data loading and matrix construction
source("./R/fun_matrix_load.R")

## Effects of grazing+fires

matrix_load(path="./data/grazing+control50_param.csv",
            name="graz_control50") #grazing + control 50 matrix

matrix_load(path="./data/fire_param.csv",
            name="fire") # fire matrix


# forest vector 
ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20)


listGraz_Cont_Fire<-list(mat.m.graz_control50,mat.m.fire)

#Check primitivity, irreducibility and ergodicity

sapply(X=listGraz_Cont_Fire, FUN=function(x){isPrimitive(A=x)})
sapply(X=listGraz_Cont_Fire, FUN=function(x){isIrreducible(A=x)})
sapply(X=listGraz_Cont_Fire, FUN=function(x){isErgodic(A=x)})

##20% fire prob.(every 5 years)
pF <- 0.2 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

projGCF5<- project(listGraz_Cont_Fire, ini.vec.f,Aseq = PmatF,time=100)

#Figure 10
windows()
par(mfrow=c(2,2))
plot(projGCF5, log = "y",main="(a)", xlab= "Time", ylab="Population size")

Aseq(projGCF5) 
years.f<-which(Aseq(projGCF5)==2) 
abline(v=years.f,col="red") 


#Stochastic growth rate
stoch(listGraz_Cont_Fire, c("lambda", "var"), vector = ini.vec.f, Aseq = PmatF,
      iterations = 2000, discard = 100)

#loop number of individuals at 50 and 100 years, total and adults (average of 2000 iterations)

n.ad.50<-NA
n.tot.50<-NA
n.tot.100<-NA

for(i in 1:2000){
  proj<-project(listGraz_Cont_Fire, ini.vec.f,Aseq = PmatF,time=100)
  n.tot.100[[i]]<-sum(vec(proj)[100,]) 
  n.tot.50[[i]]<-sum(vec(proj)[50,]) 
  n.ad.50[[i]]<-sum(vec(proj)[50,9:12]) 
}

# 100 years of the projection
summary(n.tot.100) 
n.tot.100<1 
sum(n.tot.100<1)
sum(n.tot.100<1)/2000 #proportion of iterations that are extinct at 100 years

# adults at 50 years of the projection
summary(n.ad.50) 


##11% fire prob.(every 9 years)
pF <- 0.11 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

projGCF9<- project(listGraz_Cont_Fire, ini.vec.f,Aseq = PmatF,time=100)

#Figure 10

plot(projGCF9, log = "y",main="(b)", xlab= "Time", ylab="Population size")

Aseq(projGCF9) 
years.f<-which(Aseq(projGCF9)==2) 
abline(v=years.f,col="red") 


#Stochastic growth rate
stoch(listGraz_Cont_Fire, c("lambda", "var"), vector = ini.vec.f, Aseq = PmatF,
      iterations = 2000, discard = 100)

#loop number of individuals at 50 and 100 years, total and adults (average of 2000 iterations)

n.ad.50<-NA
n.tot.50<-NA
n.tot.100<-NA

for(i in 1:2000){
  proj<-project(listGraz_Cont_Fire, ini.vec.f,Aseq = PmatF,time=100)
  n.tot.100[[i]]<-sum(vec(proj)[100,]) 
  n.tot.50[[i]]<-sum(vec(proj)[50,]) 
  n.ad.50[[i]]<-sum(vec(proj)[50,9:12]) 
}

# 100 years of the projection
summary(n.tot.100) 
n.tot.100<1 
sum(n.tot.100<1)
sum(n.tot.100<1)/2000 #proportion of iterations that are extinct at 100 years

# adults at 50 years of the projection
summary(n.ad.50) 


##6.7% fire prob.(every 15 years)
pF <- 0.067 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

projGCF15<- project(listGraz_Cont_Fire, ini.vec.f,Aseq = PmatF,time=100)

#Figure 10

plot(projGCF15, log = "y",main="(c)", xlab= "Time", ylab="Population size")

Aseq(projGCF15) 
years.f<-which(Aseq(projGCF15)==2) 
abline(v=years.f,col="red") 


#Stochastic growth rate
stoch(listGraz_Cont_Fire, c("lambda", "var"), vector = ini.vec.f, Aseq = PmatF,
      iterations = 1900, discard = 100)

#loop number of individuals at 50 and 100 years, total and adults (average of 2000 iterations)

n.ad.50<-NA
n.tot.50<-NA
n.tot.100<-NA

for(i in 1:2000){
  proj<-project(listGraz_Cont_Fire, ini.vec.f,Aseq = PmatF,time=100)
  n.tot.100[[i]]<-sum(vec(proj)[100,]) 
  n.tot.50[[i]]<-sum(vec(proj)[50,]) 
  n.ad.50[[i]]<-sum(vec(proj)[50,9:12]) 
}

# 100 years of the projection
summary(n.tot.100) 
n.tot.100<1 
sum(n.tot.100<1)
sum(n.tot.100<1)/2000 #proportion of iterations that are extinct at 100 years

# adults at 50 years of the projection
summary(n.ad.50) 


#Figure 11
df_projGCF<-data.frame(Time=rep(1:100,3),
                      FiresProbability=factor(rep(c("0.2 (mean interval 5 years)", "0.11 (mean interval 9 years)", "0.07 (mean interval 15 years)"),each=100)),
                      Proj=c(projGCF5[1:100],projGCF9[1:100],projGCF15[1:100]))

ggplot(data=df_projGCF,aes(x=Time,y=Proj,colour=FiresProbability))+
  geom_line()+
  scale_y_log10()+
  labs(x = "Time" , y = "Population size")

