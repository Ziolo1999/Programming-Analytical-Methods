# Zadanie 1

#Logarytmujemy funkcje wiarygodnosci

lnL = function(p) {
  ll = 5*log(p)+3*log(1-p)
  return(ll)
}
curve(lnL(x), from=0, to=1)

abline(v=5/8, col="red")


install.packages("maxLik")
library("maxLik")

wynik = maxNR(fn=lnL, start=0.5)
summary(wynik)
 
#Gradient pochodna funkcji log wiarygodnosci
#Hessian druga pochodna funkcji log wiarygodnosci

gradient = function(p){
  gr = 5/p-3/(1-p)
  return(gr)
}

hessian = function(p){
  h = -5/p^2-3/(1-p)^2
  return(h)
}


wynik = maxNR(fn=lnL, grad=gradient, hess=hessian, start=0.5)

summary(wynik)

#Zadanie2

x = c(3,4,3,7)

lnL = function(lambda){
  ll = sum(x)*log(lambda)-lambda*sum(x)-sum(log(factorial(x)))
  return(ll)
}

curve(lnL(x), from=0, to=5)

gradient = function(lambda){
  gr = sum(x)/lambda-sum(x)
  return(gr)
}

hessian = function(lambda) {
  h = -sum(x)/lambda^2
  return(h)
}

wynik = maxNR(fn=lnL, grad=gradient, hess=hessian, start=0.5)
summary(wynik)

#dodatkowo bez stalej
lnL = function(lambda){
  ll = sum(x)*log(lambda)-lambda*sum(x)
  return(ll)
}

curve(lnL(x), from=0, to=5)

#Zadanie5

x = c(1,2,2,3,4)
lnL = function(theta){
  ll=-N*log(pi)-sum(log(1+(x-theta)^2))
  return(ll)
}

wynik = maxNR(fn=lnL, start=0)
summary(wynik)
curve(lnL(x), from=0, to=10)
# wektor x jest mniejszy niz wektor theta

theta_vector = seq(from=0, to=4, by=0.01)
lnL = 0
for(i in 1:length(x)) {
  lnL = lnL + (-log(pi)-log(1+(x[i]-theta_vector)^2))
}

plot(theta_vector, lnL, type="l")

#Zadanie6
x = c(51,115,150,190,217,228,350)
N = length(x)

lnL = function(lambda){ 
  ll= N *log(lambda)-lambda*sum(x)
  return(ll)
}

gradient = function(lambda){
  gr = N/lambda-sum(x)
  return(gr)
}

hessian = function(lambda){
  h = -N/lambda^2
  return(h)
}

wynik = maxNR(fn=lnL, grad=gradient, hess = hessian, start=1)
summary(wynik)

curve(lnL(x), from=0, to=0.01)
abline(v=wynik$estimate, col="blue")
