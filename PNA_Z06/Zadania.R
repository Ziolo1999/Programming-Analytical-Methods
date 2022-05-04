install.packages("rgl")
library("rgl")

#Zadanie1

f1 = function(x,y){
  val = sin(x^2/2-y^2/4)*cos(2*x-exp(y))
  return(val)
}

x = seq(from=-4, to=4, by=0.01)
y = seq(from=-4, to=4, by=0.01)
fval = matrix(0, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    fval[i,j] = f1(x[i], y[j])
  }
}

persp3d(x,y,fval, col="blue")
image(x,y,fval)

#Zadanie1

f1 = function(x,y){
  val = (1-x)^2+100*(y-x^2)^2
  return(val)
}

x = seq(from=-4, to=4, by=0.01)
y = seq(from=-4, to=4, by=0.01)
fval = matrix(0, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    fval[i,j] = f1(x[i], y[j])
  }
}

persp3d(x,y,fval, col="yellow")
image(x,y,fval)

# Zadanie 5

x = c(4.76,0.35,0.04,
      -1.26,3.30,3.79,0.82,-1.18,-0.77,
      2.47,1.50,2.62,1.62,3.27,1.89,1.45,
      1.61,2.78,-0.98,2.41)
N=length(x)

f5= function(mu, sigma){
  val = -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2)
  return(val)
}

mu = seq(from=-4, to=4, by=0.01)
sigma = seq(from=-4, to=4, by=0.01)
fval = matrix(0,nrow=length(mu), ncol=length(sigma))

for(i in 1:length(mu)){
  for(j in 1:length(sigma)){
    fval[i,j] = f5(mu[i], sigma[j])
  }
}

persp3d(mu,sigma,fval, col="yellow")
