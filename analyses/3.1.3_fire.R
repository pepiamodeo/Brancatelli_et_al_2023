#Library

library(ggplot2)
library(reshape2)

# load custom functions
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes
markov.mat <- function(pF,qF){return(matrix(c(1-pF, pF,1-qF,qF),2,2))} #Markov matrix construction

# Effects of fires

# general setup

# create the initial vector with only one adult
ini.vec.c<-c(0,0,0,0,0,0,0,0,0,0,0,1) # colonization vector 
ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20) # forest vector 

ex.ad <- 0.86^8 # minimum effective number of adults

# build matrix

matrix_load(path="./data/mean_param.csv",
             name="mean") #mean matrix

matrix_load(path="./data/fire_param.csv",
             name="fire") # fire matrix

listFireDet<-list(mat.m.mean,mat.m.fire) # create a list of alternative matrices

#Check primitivity, irreducibility and ergodicity

isPrimitive(mat.m.fire)
isIrreducible(mat.m.fire)
isErgodic(mat.m.fire)


##30% fire prob.(every 3.3 years)
pF <- 0.3 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire3"

proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##20% fire prob.(every 5 years)
pF <- 0.2 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire5"

proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##14% fire prob.(every 7.1 years)
pF <- 0.14 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire7"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##11% fire prob.(every 9.1 years)
pF <- 0.11 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire9"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##9% fire prob.(every 11.1 years)
pF <- 0.09 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire11"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##7.7% fire prob.(every 13 years)
pF <- 0.077 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire13"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##6.7% fire prob.(every 14.9 years)
pF <- 0.067 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire15"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##5.9% fire prob.(every 16.9 years)
pF <- 0.059 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire17"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)

##5.3% fire prob.(every 18.9 years)
pF <- 0.053 # mean-fire transition probability 
qF <- 0.01 #fire-fire transition probability 
PmatF<-markov.mat(pF=pF,qF=qF)

name <- "fire19"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=PmatF,iterations=2000)


#Figure 3
#windows()

tiff("./fig/fig3.tiff",width=180,height=180,units="mm",
       res = 600,compression="lzw")


par(mfrow=c(4,2))

plot(projfire3, log = "y",main="(a) p = 0.3", xlab= "", ylab="Population size")
Aseq(projfire3) 
years.f<-which(Aseq(projfire3)==2) 
abline(v=years.f,col="red") 


plot(projfire5, log = "y",main="(b) p = 0.2", xlab= "", ylab="Population size")
Aseq(projfire5) 
years.f<-which(Aseq(projfire5)==2) 
abline(v=years.f,col="red") 

plot(projfire7, log = "y",main="(c) p = 0.14", xlab= "", ylab="Population size")
Aseq(projfire7) 
years.f<-which(Aseq(projfire7)==2) 
abline(v=years.f,col="red") 


plot(projfire9, log = "y",main="(d) p = 0.11", xlab= "", ylab="Population size")
Aseq(projfire9) 
years.f<-which(Aseq(projfire9)==2) 
abline(v=years.f,col="red") 


plot(projfire11, log = "y",main="(e) p = 0.09", xlab= "", ylab="Population size")
Aseq(projfire11) 
years.f<-which(Aseq(projfire11)==2) 
abline(v=years.f,col="red") 

plot(projfire13, log = "y",main="(f) p = 0.08", xlab= "", ylab="Population size")
Aseq(projfire13) 
years.f<-which(Aseq(projfire13)==2) 
abline(v=years.f,col="red") 

plot(projfire15, log = "y",main="(g) p = 0.07", xlab= "Time", ylab="Population size")
Aseq(projfire15) 
years.f<-which(Aseq(projfire15)==2) 
abline(v=years.f,col="red") 

plot(projfire17, log = "y",main="(h) p = 0.06", xlab= "Time", ylab="Population size")
Aseq(projfire17) 
years.f<-which(Aseq(projfire17)==2) 
abline(v=years.f,col="red") 

dev.off()

### Figure 4: adult population size

df_projfire_ad<-data.frame(Time=rep(1:100,9),
                                 FireProbability=factor(rep(c("0.3 (mean interval 3 years)", "0.2 (mean interval 5 years)", "0.14 (mean interval 7 years)","0.11 (mean interval 9 years)", "0.09 (mean interval 11 years)","0.08 (mean interval 13 years)","0.07 (mean interval 15 years)", "0.06 (mean interval 17 years)", "0.05 (mean interval 19 years)"),each=100)),
                                 Proj=c(rowSums(vec(projfire3)[1:100,9:12]),
                                        rowSums(vec(projfire5)[1:100,9:12]),
                                        rowSums(vec(projfire7)[1:100,9:12]),
                                        rowSums(vec(projfire9)[1:100,9:12]),
                                        rowSums(vec(projfire11)[1:100,9:12]),
                                        rowSums(vec(projfire13)[1:100,9:12]),
                                        rowSums(vec(projfire15)[1:100,9:12]),
                                        rowSums(vec(projfire17)[1:100,9:12]),
                                        rowSums(vec(projfire19)[1:100,9:12])))

fig_fire <-ggplot(data=df_projfire_ad,aes(x=Time,y=Proj,colour=FireProbability))+
  geom_line()+
  scale_y_continuous(limits=c(0.0001,10000000000000),trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_fire

ggsave(plot=fig_fire,"./fig/fig4.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_fire,"./fig/fig4.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")



