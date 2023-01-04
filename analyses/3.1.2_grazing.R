#Library

library(ggplot2)

# load custom functions 
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes

# Effects of grazing

# general setup

ini.vec<-c(0,0,0,0,0,0,0,0,0,0,0,1) # create the initial vector with only one adult
ex.ad <- 0.86^8 # minimum effective number of adults

##Grazing

name="Grazing"
matrix_load(path="./data/grazing_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.Grazing,ini.vec,iterations=1000)

##No grazing

name="NoGrazing"
matrix_load(path="./data/not_grazing_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.NoGrazing,ini.vec,iterations=1000)

### Figure 2: adult population size

df_projGrazing_ad<-data.frame(Time=rep(1:100,2),
                              Grazing=factor(rep(c("Grazing", "No Grazing"),each=100)),
                              Proj=c(rowSums(vec(projGrazing)[1:100,9:12]),
                                        rowSums(vec(projNoGrazing)[1:100,9:12])))

fig_grazing <-ggplot(data=df_projGrazing_ad,aes(x=Time,y=Proj,colour=Grazing))+
  geom_line()+
  scale_y_continuous(limits=c(0.1,10000000000000),trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_grazing

ggsave(plot=fig_grazing,"./fig/fig2.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_grazing,"./fig/fig2.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")



