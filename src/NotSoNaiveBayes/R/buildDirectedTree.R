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