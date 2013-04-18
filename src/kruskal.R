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