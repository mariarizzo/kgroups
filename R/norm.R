# The norm in R^n space
Norm=function(x,alpha){
  # x: is a vector in R^n 
  # alpha: 0< alpha <= 2
  l=  (sqrt(sum(x*x)))^alpha
  return(l)
}