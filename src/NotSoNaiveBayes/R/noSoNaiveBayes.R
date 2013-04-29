notSoNaiveBayes <- function (obj,x) {
  classC = obj$classC
  #liczba atrybutów 
  attrC = length(obj$apriori)
  
  #transponowane drzewo polączeń
  transDirectedTree=t(obj$directedTree)

  dimNum <- function(nameList, name) {
    i=0
    repeat {
      i = i+1
      if(nameList[i] == name)
        break;
      if(i>length(nameList))
        simpleError("błąd atrybutu")
    }
    i
  }
  
  estProb <- function(c,vec) {
    temp = log(obj$classProb[c])
    for(i in 1:length(vec)) {
      if(max(transDirectedTree[i,])==0){
        apriori = as.matrix(obj$apriori[[i]])
        dimNamesI=dimnames(apriori)[[2]]
        ii = dimNum(dimNamesI,vec[i])
 
        temp = temp + log(apriori[c,ii])
      } 
      else {
        j = which(transDirectedTree[i,]==1)
        pos = i+(j-1)*attrC
        arrProb = obj$conditionalProb[pos]
        arrProb = arrProb[[1]]
        dimNamesI=dimnames(arrProb)[[1]]
        dimNamesJ=dimnames(arrProb)[[2]]
        ii = dimNum(dimNamesI,vec[i])
        jj = dimNum(dimNamesJ,vec[j])
        temp = temp + log(arrProb[ii,jj,c])
      }
    }
    temp
      
  }

 # vec=x[1,]
 estClass <- function (vec) {
    classProb = array(0,classC)
    for(i in 1:classC) {
      classProb[i] = estProb(i,vec)
    }
    maxi = which(max(classProb)==classProb)
    className = dimnames(obj$classProb)[[1]][maxi]
    className
  }
  ret = estClass(x[1,])
  ret = apply(x,1,estClass)
  ret
}