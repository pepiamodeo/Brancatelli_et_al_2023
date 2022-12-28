#Library

library(ggplot2)

# load custom functions
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch_control.R") # projection and outcomes

# the parameter removal defines a fixed number of individuals of class adult 1 which are removed every year

##Effects population control 

# general setup

# create the initial vector
ini.vec.f<-c(6194767,32,73,77,50,39,23,14,34,33,62,20) # forest vector 
ex.ad <- 0.86^8 # minimum effective number of adults

##Control 1000: annual removal of 1000 individuals

name="control1000"
matrix_load(path="./data/mean_param.csv",
            name=name) 

proj.stoch.control(list.mat=mat.s.control1000,
                   ini.vec=ini.vec.f,
                   iterations=2000,
                   removal=1000)

##Control10000: annual removal of 10000 individuales 

name="control10000"
matrix_load(path="./data/mean_param.csv",
            name=name) 

proj.stoch.control(list.mat=mat.s.control10000,
                   ini.vec=ini.vec.f,
                   iterations=2000,
                   removal=10000)

##Control 50000: annual removal of 50000 individuals

name="control50000"
matrix_load(path="./data/mean_param.csv",
            name=name) 

proj.stoch.control(list.mat=mat.s.control50000,
                   ini.vec=ini.vec.f,
                   iterations=2000,
                   removal=50000)

##Control100000: annual removal of 100000 of individuals

name="control100000"
matrix_load(path="./data/mean_param.csv",
            name=name) 

proj.stoch.control(list.mat=mat.s.control100000,
                   ini.vec=ini.vec.f,
                   iterations=2000,
                   removal=100000)


#Figure
library(ggplot2)

df_projControl<-data.frame(Time=rep(1:100,4),
                       Removal=factor(rep(c("1000", "10000","50000","100000"),each=100),levels=c("1000", "10000","50000","100000")),
                       Proj=c(colSums(projcontrol1000[,1:100]),
                              colSums(projcontrol10000[,1:100]),
                              colSums(projcontrol50000[,1:100]),
                              colSums(projcontrol100000[,1:100])))


ggplot(data=df_projControl,aes(x=Time,y=Proj,colour=Removal))+
  geom_line()+
  scale_y_log10()+
  labs(x = "Time" , y = "Population size")

# Figure adults

df_projControl_ad<-data.frame(Time=rep(1:100,4),
                           Removal=factor(rep(c("1000","10000", "50000","100000"),each=100),levels=c("1000", "10000","50000","100000")),
                           Proj=c(colSums(projcontrol1000[9:12,1:100]),
                                  colSums(projcontrol10000[9:12,1:100]),
                                  colSums(projcontrol50000[9:12,1:100]),
                                  colSums(projcontrol100000[9:12,1:100])))

fig_control <-ggplot(data=df_projControl_ad,aes(x=Time,y=Proj,colour=Removal))+
  geom_line()+
  scale_y_continuous(trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_control

ggsave(plot=fig_control,"./fig/fig7fixed.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_control,"./fig/fig7fixed.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")
