library(RadioGx)

#CI<-function(a,A,b,B)  {a/A + b/B} #Calculating the combination index
#linear.quadratic<-function(D,a,b){exp(-a*D-b*D^2)} #Linear quadratic function

ComputeDX<-function(a,b,S) { 
  if (a & b > 0){ (-a + sqrt(a^2-(4*b*log(S))))/(b*2)
  } else if (a ==0 & b> 0){(sqrt(log(S)/-b)) 
  } else if (a>0 & b == 0) { (log(S)/-a)
  } else {NA}}
##Equivalent to RadioGX computeD10
##Use for other effect levels 