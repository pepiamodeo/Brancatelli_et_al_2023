#Library

library(ggplot2)
library(reshape2)

# load custom functions
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes

# Effects of prescribed fires

# general setup

# create the initial vector
ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20)# forest vector 
ex.ad <- 0.86^8 # minimum effective number of adults

# build matrix

matrix_load(path="./data/mean_param.csv",
            name="mean") #mean matrix

matrix_load(path="./data/fire_param.csv",
            name="fire") # fire matrix

listFireDet<-list(mat.m.mean,mat.m.fire)

#Check primitivity, irreducibility and ergodicity

isPrimitive(mat.m.fire)
isIrreducible(mat.m.fire)
isErgodic(mat.m.fire)

#Projections

# fire every 3 years

sec3<-rep(c(1,1,2),33) # fire sequence every 3 years

name <- "burn3"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=sec3,iterations=1000)

# fire every 5 years
sec5<-rep(c(1,1,1,1,2),20) 
name <- "burn5"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=sec5,iterations=1000)

# fire every 7 years
sec7<-rep(c(1,1,1,1,1,1,2),15)
name <- "burn7"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=sec7,iterations=1000)

# fire every 9 years
sec9<-rep(c(1,1,1,1,1,1,1,1,2),11)
name <- "burn9"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=sec9,iterations=1000)

# fire every 11 years
sec11<-rep(c(1,1,1,1,1,1,1,1,1,1,2),9)
name <- "burn11"
proj.stoch(list.mat=listFireDet,ini.vec=ini.vec.f,
           Aseq=sec11,iterations=1000)


#Figure

df_Burning<-data.frame(Time=rep(1:100,5),
                       TimeBetweenFires=factor(rep(c(3,5,7,9,11),each=100)),
                        Proj=c(projburn3[1:100],projburn5[1:100],projburn7[1:100],
                               projburn9[1:100],projburn11[1:100]))

ggplot(data=df_Burning,aes(x=Time,y=Proj,colour=TimeBetweenFires))+
  geom_line()+
  scale_y_log10()+
  labs(x = "Time" , y = "Population size")

### Figure adults

df_Burning<-data.frame(Time=rep(1:100,5),
                       TimeBetweenFires=factor(rep(c(3,5,7,9,11),each=100)),
                       Proj=c(rowSums(vec(projburn3)[1:100,9:12]),
                       rowSums(vec(projburn5)[1:100,9:12]),
                       rowSums(vec(projburn7)[1:100,9:12]),
                       rowSums(vec(projburn9)[1:100,9:12]),
                       rowSums(vec(projburn11)[1:100,9:12])))

fig_burning<-ggplot(data=df_Burning,aes(x=Time,y=Proj,colour=TimeBetweenFires))+
  geom_line()+
  scale_y_continuous(limits=c(0.0001,10000000000000000),trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_burning

ggsave(plot=fig_burning,"./fig/fig12.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_burning,"./fig/fig12.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")

