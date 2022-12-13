#Library

library(ggplot2)
library(reshape2)

# load custom functions for data loading and matrix construction
source("./R/fun_matrix_load.R")
source("./R/proj_stoch_control_fire.R") # projection and outcomes
markov.mat <- function(pF,qF){return(matrix(c(1-pF, pF,1-qF,qF),2,2))} #Markov matrix construction

## Effects of grazing+fires

matrix_load(path="./data/grazing_param.csv",
            name="graz_control") #grazing + control 50 matrix

matrix_load(path="./data/fire_param.csv",
            name="fire") # fire matrix


# forest vector 
ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20)
ex.ad <- 0.86^8 # minimum effective number of adults

listGraz_Cont_Fire<-list(mat.m.graz_control,mat.m.fire)

#Check primitivity, irreducibility and ergodicity

sapply(X=listGraz_Cont_Fire, FUN=function(x){isPrimitive(A=x)})
sapply(X=listGraz_Cont_Fire, FUN=function(x){isIrreducible(A=x)})
sapply(X=listGraz_Cont_Fire, FUN=function(x){isErgodic(A=x)})

##20% fire prob.(every 5 years)
pF <- 0.2 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "GCF5"
proj.stoch.control.fire(list.mat=listGraz_Cont_Fire,
                        ini.vec=ini.vec.f,
                        Aseq=PmatF,
                        removal=10000,
                        iterations=2000)

##11% fire prob.(every 9 years)
pF <- 0.11 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "GCF9"
proj.stoch.control.fire(list.mat=listGraz_Cont_Fire,
                        ini.vec=ini.vec.f,
                        Aseq=PmatF,
                        removal=10000,
                        iterations=2000)

##6.7% fire prob.(every 15 years)
pF <- 0.067 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "GCF15"
proj.stoch.control.fire(list.mat=listGraz_Cont_Fire,
                        ini.vec=ini.vec.f,
                        Aseq=PmatF,
                        removal=10000,
                        iterations=2000)

#Figure 11
df_projGCF<-data.frame(Time=rep(1:100,3),
                      FiresProbability=factor(rep(c("0.2 (mean interval 5 years)", "0.11 (mean interval 9 years)", "0.07 (mean interval 15 years)"),each=100)),
                      Proj=c(colSums(projGCF5[,1:100]),
                             colSums(projGCF9[,1:100]),
                             colSums(projGCF15[,1:100])))

ggplot(data=df_projGCF,aes(x=Time,y=Proj,colour=FiresProbability))+
  geom_line()+
  scale_y_log10()+
  labs(x = "Time" , y = "Population size")

