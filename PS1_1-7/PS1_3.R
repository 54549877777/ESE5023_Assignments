Pascal_triangle<-function(k){
  P<-array(0,dim=c(k,k))
  for(i in 1:k){
    P[i,c(1,i)]<-1
  }
  if(k>=1){
    for(i in 3:k){
      for(j in 2:(k-1)){
        P[i,j]<-P[i-1,j-1]+P[i-1,j]
      }
    }
  }
  return(P[k,])
}
print(Pascal_triangle(100))
print(Pascal_triangle(200))

