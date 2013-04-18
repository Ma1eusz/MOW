f2 <- function (x,comb) {
  l=list()
  for(i in 1:(dim(comb)[2])) {
   l=c(l,x[,comb[,i]])
  }
  l
  
}