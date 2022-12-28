#Library

library(ggplot2)

# load custom functions
source("./R/fun_matrix_load.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes

# Effects of the topography

# general setup

ini.vec<-c(0,0,0,0,0,0,0,0,0,0,0,1) # create the initial vector with only one adult
ex.ad <- 0.86^8 # minimum effective number of adults

##NE LOW SLOPE (germination= 0.4225; establishment= 0.0059)

name="NELow"
matrix_load(path="./data/NELow_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.NELow,ini.vec=ini.vec,
           iterations=2000)

##NE MID SLOPE (germination= 0.235; establishment= 0.043)

name="NEMid"
matrix_load(path="./data/NEMid_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.NEMid,ini.vec=ini.vec,
           iterations=2000)

##NE HIGH SLOPE(germination= 0.1925; establishment= 0)

name="NEHigh"
matrix_load(path="./data/NEHigh_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.NEHigh,ini.vec=ini.vec,
           iterations=2000)

##SW LOW SLOPE (germination= 0.05; establishment= 0)

name="SWLow"
matrix_load(path="./data/SWLow_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.SWLow,ini.vec=ini.vec,
           iterations=2000)

##SW MID SLOPE (germination= 0.1475; establishment= 0.344)

name="SWMid"
matrix_load(path="./data/SWMid_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.SWMid,ini.vec=ini.vec,
           iterations=2000)

##SW HIGH SLOPE (germination= 0.1425; establishment= 0.088)

name="SWHigh"
matrix_load(path="./data/SWHigh_param.csv",
            name=name) 
proj.stoch(list.mat=mat.s.SWHigh,ini.vec=ini.vec,
           iterations=2000)

### Figure adult population size

df_projTopography_ad<-data.frame(Time=rep(1:100,6),
                                 Slope=factor(rep(c("NEHigh", "SWHigh","NEMid", "SWMid","NELow", "SWLow"),each=100)),
                                 Proj=c(rowSums(vec(projNEHigh)[1:100,9:12]),
                                        rowSums(vec(projSWHigh)[1:100,9:12]),
                                        rowSums(vec(projNEMid)[1:100,9:12]),
                                        rowSums(vec(projSWMid)[1:100,9:12]),
                                        rowSums(vec(projNELow)[1:100,9:12]),
                                        rowSums(vec(projSWLow)[1:100,9:12])))

fig_topography <-ggplot(data=df_projTopography_ad,aes(x=Time,y=Proj,colour=Slope))+
  geom_line()+
  scale_y_continuous(limits=c(0.0001,10000000000000),trans="log10")+
  labs(x = "Time (years)" , y = "Adult Population Size")

fig_topography

ggsave(plot=fig_topography,"./fig/fig1.pdf",width=180,height=140,units="mm",
       dpi = 600, colormodel = "cmyk")
ggsave(plot=fig_topography,"./fig/fig1.tiff",width=180,height=180,units="mm",
       dpi = 600,compression="lzw")



