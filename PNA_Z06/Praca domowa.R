library("rgl")
library("maxLik")

#### Zadanie 1 ####
#a)

g1 = function(x,y){
  val = sin(x-y^2)
  return(val)
}

x = seq(from=-5, to=5, by=0.01)
y = seq(from=-5, to=5, by=0.01)
gval = matrix(0, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    gval[i,j] = g1(x[i], y[j])
  }
}

persp3d(x,y,gval, col="blue")
image(x,y,gval)

#b)

h1 = function(x,y){
  val = sqrt(9-x^2-y^2)
  return(val)
}

x = seq(from=-2, to=2, by=0.01)
y = seq(from=-2, to=2, by=0.01)
hval = matrix(0, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    hval[i,j] = h1(x[i], y[j])
  }
}

persp3d(x,y,hval, col="blue")
image(x,y,hval)


#### Zadanie 2 ####

#a)

lnL = function(lambda) {
  ll = -log(prod(factorial(k)))-n*lambda+log(lambda^(sum(k)))
  -n*log(1-exp(-lambda))
  return(ll)
}

curve(lnL(x), from=0, to=5)

#b)

data = read.csv('runshoes.csv')
View(data)
k = data$shoes
n = length(k)

wynik = maxNR(fn=lnL, start=0.01)
summary(wynik)