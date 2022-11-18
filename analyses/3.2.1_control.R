#Library

library(ggplot2)

# load custom functions
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes

##Effects population control 

# general setup

# create the initial vector
ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20) # forest vector 
ex.ad <- 0.86^8 # minimum effective number of adults

##Control 01: annual removal of 1% of the population (sa=0.96)

name="control01"
matrix_load(path="./data/control01_param.csv",
            name=name) 

proj.stoch(list.mat=mat.s.control01,ini.vec=ini.vec.f,
           iterations=2000)

##Control20: annual removal of 20% of the population (sa=0.77)

name="control20"
matrix_load(path="./data/control20_param.csv",
            name=name) 

proj.stoch(list.mat=mat.s.control20,ini.vec=ini.vec.f,
           iterations=2000)

##Control50: annual removal of 50% of the population (sa=0.47)

name="control50"
matrix_load(path="./data/control50_param.csv",
            name=name) 

proj.stoch(list.mat=mat.s.control50,ini.vec=ini.vec.f,
           iterations=2000)

##Control80: annual removal of 80% of the population (sa=0.17)

name="control80"
matrix_load(path="./data/control80_param.csv",
            name=name) 

proj.stoch(list.mat=mat.s.control80,ini.vec=ini.vec.f,
           iterations=2000)

#Figure

df_projControl<-data.frame(Time=rep(1:100,4),
                       Removal=factor(rep(c("1% control","20% control", "50% control","80% control"),each=100)),
                       Proj=c(projcontrol01[1:100],projcontrol20[1:100],projcontrol50[1:100],projcontrol80[1:100]))


ggplot(data=df_projControl,aes(x=Time,y=Proj,colour=Removal))+
  geom_line()+
  scale_y_log10()+
  labs(x = "Time" , y = "Population size")

# Figure adults

df_projControl_ad<-data.frame(Time=rep(1:100,4),
                           Removal=factor(rep(c("1% control","20% control", "50% control","80% control"),each=100)),
                           Proj=c(rowSums(vec(projcontrol01)[1:100,9:12]),
                                  rowSums(vec(projcontrol20)[1:100,9:12]),
                                  rowSums(vec(projcontrol50)[1:100,9:12]),
                                  rowSums(vec(projcontrol80)[1:100,9:12])))

fig_control <-ggplot(data=df_projControl_ad,aes(x=Time,y=Proj,colour=Removal))+
  geom_line()+
  scale_y_continuous(trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_control

ggsave(plot=fig_control,"./fig/fig7.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_control,"./fig/fig7.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")
