# Stochastic projection

# manual proyection (instead of using project() function)
# in order to be able to extract a fixed number of individuals per year

project.control<- function(list.mat, ini.vec, time=100,removal){
  nYears <- time
  outcome <- matrix(0,nrow=length(ini.vec),ncol=nYears+1)          # initialize storage array for recording age structured abundances for the entire projection 
  rownames(outcome) <- rownames(ini.vec)      # assign row and column names
  colnames(outcome) <- seq(0,nYears)
  outcome[,1] <- ini.vec                      # initialize the simulated abundances
  # loop
  
  remove <- removal # control actions, removing a fixed number every year
  
  for(t in 2:(nYears+1)){
    sel.mat <- sample(1:100,size=1) # random sampling one matrix
    outcome[9,t-1] <- outcome[9,t-1]-remove # remove individuals of class adult 1
    if(outcome[9,t-1]<0){outcome[9,t-1]<-0} # correction if it is <0
    outcome[,t] <-  list.mat[[sel.mat]] %*% outcome[,t-1]     # perform matrix multiplication for each year of the simulation
  }
  return(outcome)
}
