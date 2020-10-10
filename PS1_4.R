x<-sample(1:100,1)
Least_moves<-function(x){
   for(i in 1:7){
     if(x==2^i){
       m=i
       break
     }
     else if(2^i>x){
       m=x+i-1-2^(i-1)
       break
     }
   }
  return(m)
}
print(c('x=',x))
print(c("Least_moves=",Least_moves(x)))

