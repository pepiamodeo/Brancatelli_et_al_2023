# Stochastic projection

# manual proyection (instead of using project() function)
# in order to be able to extract a fixed number of individuals per year

# the parameter removal defines a fixed number of individuals of class adult 1 which are removed every year

project.control.fire<- function(list.mat, ini.vec, 
                                Aseq=Aseq,
                                time=100,removal){
  nYears <- time
  outcome <- matrix(0,nrow=length(ini.vec),ncol=nYears+1)          # initialize storage array for recording age structured abundances for the entire projection 
  rownames(outcome) <- rownames(ini.vec)      # assign row and column names
  colnames(outcome) <- seq(0,nYears)
  outcome[,1] <- ini.vec                      # initialize the simulated abundances
  # loop
  
  remove <- removal # control actions, removing a fixed number every year
  sel.mat=1
  for(t in 2:(nYears+1)){
    outcome[9,t-1] <- outcome[9,t-1]-remove # remove individuals of class adult 1
    if(outcome[9,t-1]<0){outcome[9,t-1]<-0} # correction if it is <0
    if(sel.mat==1){sel.mat <- sample(1:2,size=1,prob=PmatF[,1])}
    else{sel.mat <- sample(1:2,size=1,prob=PmatF[,2])}
    
    outcome[,t] <-  list.mat[[sel.mat]] %*% outcome[,t-1]     # perform matrix multiplication for each year of the simulation
    
  }
  return(outcome)
}
