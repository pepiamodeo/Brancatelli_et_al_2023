#Library

library(ggplot2)
library(reshape2)

# load custom functions for data loading and matrix construction
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch_control_fire.R") # projection and outcomes
markov.mat <- function(pF,qF){return(matrix(c(1-pF, pF,1-qF,qF),2,2))} #Markov matrix construction

## Effects of control+fires

# general setup

ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20) # forest vector 
ex.ad <- 0.86^8 # minimum effective number of adults

# build matrix

matrix_load(path="./data/mean_param.csv",
            name="control") #control 50 matrix

matrix_load(path="./data/fire_param.csv",
            name="fire") # fire matrix

listContFire<-list(mat.m.control,mat.m.fire)

#Check primitivity, irreducibility and ergodicity

sapply(X=listContFire, FUN=function(x){isPrimitive(A=x)})
sapply(X=listContFire, FUN=function(x){isIrreducible(A=x)})
sapply(X=listContFire, FUN=function(x){isErgodic(A=x)})

##20% fire prob.(every 5 years)
pF <- 0.2 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "CF5"
proj.stoch.control.fire(list.mat=listContFire,
                  ini.vec=ini.vec.f,
                  Aseq=PmatF,
                  removal=10000,
                  iterations=2000)

##11% fire prob.(every 9 years)
pF <- 0.11 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "CF9"
proj.stoch.control.fire(list.mat=listContFire,
                        ini.vec=ini.vec.f,
                        Aseq=PmatF,
                        removal=10000,
                        iterations=2000)


##6.7% fire prob.(every 15 years)
pF <- 0.067 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "CF15"
proj.stoch.control.fire(list.mat=listContFire,
                        ini.vec=ini.vec.f,
                        Aseq=PmatF,
                        removal=10000,
                        iterations=2000)
#Figure 8

# no se puede con esta función nueva

#Figure 9
df_projCF<-data.frame(Time=rep(1:100,3),
                      FiresProbability=factor(rep(c("0.2 (mean interval 5 years)", "0.11 (mean interval 9 years)", "0.07 (mean interval 15 years)"),each=100)),
                      Proj=c(colSums(projCF5[,1:100]),
                             colSums(projCF9[,1:100]),
                             colSums(projCF15[,1:100])))

fig_CF<-ggplot(data=df_projCF,aes(x=Time,y=Proj,colour=FiresProbability))+
  geom_line()+
  scale_y_log10()+
  labs(x = "Time" , y = "Population size",colour= "Removal 10000 + Fire Probability")

ggsave(plot=fig_CF,"./fig/fig9fixed.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_CF,"./fig/fig9fixed.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")


