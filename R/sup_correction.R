
# survival correction for stages in the model
# calculated from the annual survival rates estimated in the field

sup_correction <- function(sup,d){
  P<-((1-(sup^(d-1)))/(1-sup^d))*sup  #ver Bruce&Shernock
  G<- ((P^d)*(1-sup))/(1-P^d)  #ver Bruce&Shernock
  result<-list(P=P,G=G)
  return(result)
  }

#sup_correction(sup=0.97,d=45)
