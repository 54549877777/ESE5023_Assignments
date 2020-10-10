#1:
A<-sample(0:50,50)
M1<-matrix(A,5,10)
M2<-matrix(A,10,5)
# 2:function Matrix_multip
Matrix_multip <- function(matrix1,matrix2){
  r <- nrow(matrix1)
  c <- ncol(matrix2)
  M3<-matrix(0,r,c)
  for(i in 1:r){
    for(j in 1:c){
      num=M1[i,]*M2[,j]
      M3[i,j] <- sum(num)
    }
  }
  return(M3)
}
print(Matrix_multip(M1,M2))
# %*% operator
M1%*%M2

