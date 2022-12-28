#Library

library(ggplot2)
library(reshape2)

# load custom functions for data loading and matrix construction
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes
markov.mat <- function(pF,qF){return(matrix(c(1-pF, pF,1-qF,qF),2,2))} #Markov matrix construction

## Effects of grazing+fires

# general setup

ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20) # forest vector 
ex.ad <- 0.86^8 # minimum effective number of adults

# build matrix

matrix_load(path="./data/grazing_param.csv",
            name="grazing") #grazing matrix

matrix_load(path="./data/fire_param.csv",
            name="fire") # fire matrix

listGrazFire<-list(mat.m.grazing,mat.m.fire)

#Check primitivity, irreducibility and ergodicity

sapply(X=listGrazFire, FUN=function(x){isPrimitive(A=x)})
sapply(X=listGrazFire, FUN=function(x){isIrreducible(A=x)})
sapply(X=listGrazFire, FUN=function(x){isErgodic(A=x)})

##20% fire prob.(every 5 years)
pF <- 0.2 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "GF5"
proj.stoch(list.mat=listGrazFire,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##11% fire prob.(every 9 years)
pF <- 0.11 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "GF9"
proj.stoch(list.mat=listGrazFire,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##6.7% fire prob.(every 15 years)
pF <- 0.067 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 

PmatF <- matrix(c(1-pF, pF,1-qF,qF),2,2) #Markov matrix construction

name <- "GF15"
proj.stoch(list.mat=listGrazFire,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

#Figure 5

#windows()

tiff("./fig/fig5.tiff",width=180,height=180,units="mm",
     res = 600,compression="lzw")

par(mfrow=c(2,2))
plot(projGF5, log = "y",main="(a) grazing+fire (p=0.2)", xlab= "Time", ylab="Population size")

Aseq(projGF5) 
years.f<-which(Aseq(projGF5)==2) 
abline(v=years.f,col="red") 

plot(projGF9, log = "y",main="(b) grazing+fire (p=0.11)", xlab= "Time", ylab="Population size")

Aseq(projGF9) 
years.f<-which(Aseq(projGF9)==2) 
abline(v=years.f,col="red") 

plot(projGF15, log = "y",main="(c) grazing+fire (p=0.07)", xlab= "Time", ylab="Population size")

Aseq(projGF15) 
years.f<-which(Aseq(projGF15)==2) 
abline(v=years.f,col="red") 

dev.off()

### Figure adults

df_projGF_ad<-data.frame(Time=rep(1:100,3),
                         FireProbability=factor(rep(c("0.2 (mean interval 5 years)", "0.11 (mean interval 9 years)", "0.07 (mean interval 15 years)"),each=100)),
                         Proj=c(rowSums(vec(projGF5)[1:100,9:12]),
                                rowSums(vec(projGF9)[1:100,9:12]),
                                rowSums(vec(projGF15)[1:100,9:12])))

fig_GF <-ggplot(data=df_projGF_ad,aes(x=Time,y=Proj,colour=FireProbability))+
  geom_line()+
  scale_y_continuous(limits=c(0.0001,10000000000000),trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_GF

ggsave(plot=fig_GF,"./fig/fig5.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_GF,"./fig/fig5.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")



