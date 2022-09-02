
todecimal<-function(binario){
  bin <- as.numeric(strsplit(as.character(binario), "")[[1]])
  posiciones <- c(0, seq_along(bin))[1:(length(bin))]
  potencias <- c()
  
  for (i in posiciones){
    potencias <- c(potencias, 2^i)
    }
  decimal<-sum(potencias*rev(bin))
  return(decimal)
}

todecimal(11100)
