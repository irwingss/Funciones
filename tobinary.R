# Versión 1
tobinary<-function(x){
  residuo<-c()
  numero <- c(x)
  
  while(numero >= 1){
    residuo <- c(residuo, numero%%2)
    numero <- numero%/%2
  }
  residuo<-c(residuo)
  return(rev(residuo))
}

# Versión 2
tobinary<-function(x){
  residuo<-c()
  numero <- c(x)
  
  while(numero >= 1){
    if(numero %% 2 == 0) {
      residuo <- c(residuo, 0)
      numero <- numero%/%2
    } else if(numero %% 2 != 0) {
      residuo <- c(residuo, 1)
      numero <- numero%/%2
      }
  }
  return(rev(residuo))
}


tobinary(12)
tobinary(28)
tobinary(64)
