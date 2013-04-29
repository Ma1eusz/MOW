notSoNaiveBayes <- function(obj,..) 
  UseMethod(notSoNaiveBayes)



notSoNaiveBayes.defualt <- function(x,y) {
  rows <- dim(x)[1]
  cols <- dim(x)[2]
  comb <- combn(cols,2)
  
  est <- function(var) {
    if (is.numeric(var)) {
      simpleError("Tylko atrybuty nominalne!!!")
    } else {
      tab <- table(y, var)
      (tab ) / (rowSums(tab) )
    }
  }
  
  prob <- function (var) {
    r = var / sum(var)
    r[is.nan(r)] <- 0
    r
  }
  
  
  classProb <- table(y)
  classProb <- classProb / sum(classProb)
  
  apriori <- lapply(x,est)
  
  I1 = array(-10,c(cols,cols))
  for(k in 1:(dim(comb)[2])) {
    Aij = x[,comb[,k]]
    Ai = table(Aij[,1],y)
    Aj =table(Aij[,2],y)
    pAi = t(t(Ai)/colSums(Ai))
    pAj = t(t(Aj)/colSums(Aj))
    pAj[is.nan(pAj)] <- 0
    pAi[is.nan(pAi)] <- 0
    
    pAiAj = table(Aij[,1],Aij[,2],y)
    pAiAjcC = pAiAj/sum(pAiAj)
    pAiAj = apply(pAiAj,3,prob)
    lrows = length(dimnames(Ai)[[1]])
    lcols = length(dimnames(Aj)[[1]])
    lclass = length(levels(y))
    dim(pAiAj)=c(lrows,lcols,lclass)
    
    
    sum1 =0;
    for(c in 1:lclass) {
      if(classProb[c] !=0)
        for(j in 1:lcols) {
          if(pAj[j,c]!= 0)
            for(i in 1:lrows) {
              if(pAi[i,c] !=0)
                if(pAiAj[i,j,c] !=0)
                  sum1 = sum1 + pAiAjcC[i,j,c] *log2((pAiAj[i,j,c])/(pAi[i,c]*pAj[j,c]))
            }
        }
    }
    I1[comb[1,k],comb[2,k]]=sum1
    
    
  }
  con = kruskal(I1)
  
  directedTree = buildDirectedTree(con,4)
  dep=t(directedTree)
  
  conditionalProb = list()
  for (j in 1:cols) {
    for (i in 1:cols) {
      if(dep[i,j] == 0) {
        conditionalProb = c(conditionalProb,0)
      }
      else {
        Aij = x[,c(i,j)]
        pAiAj = table(Aij[,1],Aij[,2],y)
        pAiAj = apply(pAiAj,c(2,3),prob)
        conditionalProb = c(conditionalProb, list(pAiAj))
        
      }
    }
  }
  
  
  
  
  structure(list(classC = length(levels(y)),
                 apriori = apriori,
                 classProb=classProb,
                 I1=I1,
                 con=con,
                 dep=dep,
                 directedTree = directedTree,
                 conditionalProb = conditionalProb
  ),
            
            class = "notSoNaiveBayes"
  )
  
}



predict.notSoNaiveBayes <- function (obj,x) {
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

buildDirectedTree <- function(I,root) {
  nodesCount = dim(I)[1]
  nodes = array(0,nodesCount)
  retI= array(0,c(nodesCount,nodesCount))
  #Ustalamy wierzchołek początkowy
  #0 - wierzchołek niezsotał jescze napotakany
  #1 - wierzchołek do przetworzenia
  #2 - wierzchołek został przetworzony
  nodes[root]= 1
  
  repeat {
    #aktualnie prztwarzane wierzchołki
    curr =  which(nodes==1)
    for(i in curr) {
      con = which(I[i,]==1)
      for(j in con) {
        if(nodes[j]==0) {
          retI[i,j] =1
          nodes[j] = 1
        }
      }
      nodes[i] = 2
      
    }
    
    if(min(nodes)>0) {
      break
    }
  }
  
  retI
  
}

kruskal <- function (I) {
  rows = dim(I)[1]
  cols = dim(I)[2]
  nodes = cols
  
  con = array(0,c(nodes,nodes))
  parts = array(0,nodes)
  parts[0] = 1
  for (i in 1:(nodes-1)) {
    
    repeat{
      flag=1
      maxv = max(I, na.rm=TRUE)
      w = which.max(I)
      maxC = w%/%rows+1
      maxR = w%%rows
      I[maxR, maxC]  <- -19 #Magic number :)
      
      if(parts[maxR] != parts[maxC]) {
        #Węzły należą do róznych podgrafów można dodać krawędź
        if(parts[maxR] > parts[maxC]){
          smaller = maxC
          bigger = maxR
        }
        else{
          smaller = maxR
          bigger = maxC
        }
        temp = parts[smaller]
        parts[smaller] = parts[bigger]
        if(temp != 0 ) {
          parts[which(parts==temp)] <- parts[bigger]
        }
        con[smaller,bigger] =1
        
      }
      else {
        if(parts[maxR]==0) {
          parts[maxR] = max(parts) +1
          parts[maxC] = parts[maxR]
          con[maxR,maxC] =1
        }
        else{
          flag = 0;
        }
        
      }
      
      
      
      if(flag > 0){
        break
      }
    }
    
  }
  con=con+t(con)
  con
  
}