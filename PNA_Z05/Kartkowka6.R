#zad 1

library("maxLik")

x = c(0.5677523, 0.8236657, 0.4568736, 0.5125185, 0.3192458)
x

lnL = function(u){
  ll = 5*log(1/(2*pi)) - (1/2)*sum((x-u)^2)
  return(ll)
}

wynik = maxNR(fn=lnL, start=0.3)
summary(wynik)

gradient = function(g){
  gr = sum(x-g)
  return(gr)
}

hessian = function(h){
  hs = -5
  return(hs)
}

wynik2 = maxNR(fn=lnL, grad=gradient, hess=hessian, start=0.3)
summary(wynik2)

#Zadanie 2

dane = read.csv("Strajki.csv", header = TRUE, sep = ",")
View(dane)

x = dane["x"]
n = 62

lnL = function(lambda){
  ll = n*log(lambda) - lambda*sum(x)
  return(ll)
}

gradient = function(lambda){
  gr = n/lambda - sum(x)
  return(gr)
}

hessian = function(lambda){
  hs = -n/lambda^2
  return(hs)
}

wynik = maxNR(fn=lnL, grad = gradient, hess = hessian,start = 1)

x = dane["x"]
N = 62
1/(sum(x)/62)

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

wynik = maxNR(fn=lnL, gradient, hessian, start=0.001)
summary(wynik)

curve(lnL(x), from=0, to=0.08)
abline(v=wynik$estimate, col="blue")