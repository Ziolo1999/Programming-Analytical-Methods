#### Zajecia 10 ####

#Zad1
library(maxLik)

dane = read.csv("t1000.csv")
View(dane)
y = dane$x
N = length(y)
Qmin = function(v){
  m = rbind((y-0)^2-v/(v-2),
            (y-0)^4-3*v^2/((v-2)*(v-4)))
  M = rowMeans(m)
  w = m%*%t(m)/N
  val = -1/2*t(M)%*%solve(w)%*%M
  return(val)
}

wynik = maxNR(Qmin, start = 5)
summary(wynik)

# H0: v= 6

vcov = -solve(wynik$hessian)
std.err = sqrt(vcov)
(z.test = (wynik$estimate-6)/std.err*sqrt(N))
#z.test - N(0,1)
#wart. kryt. 1.96

#p.value
(p.value = 2*(1-pnorm(abs(z.test), mean=0, sd=1)))
# brak podstaw do odrzucenia H0, p-value = 49%

#Zadanie 2
dane = read.csv("GammaSamp.csv")
y = dane$x
N = length(y)

Qmin = function(v){
  m = rbind(y-v,
            y^2-v*(v+1))
  M = rowMeans(m)
  w = m%*%t(m)/N
  val = -1/2*t(M)%*%solve(w)%*%M
  return(val)
}

wynik = maxNR(Qmin, start = 5)
summary(wynik)

vcov = -solve(wynik$hessian)
std.err = sqrt(vcov)
(z.test = (wynik$estimate-5)/std.err*sqrt(N))

(p.value = 2*(1-pnorm(abs(z.test), mean=0, sd=1)))

# p.value = 32% brak podstaw do odrzucenia hipotezy

#Zadanie3

dane = read.csv("Norm1000.csv")
y = dane$x
N = length(y)

Qmin = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  m = rbind(y-mu,
            (y-mu)^2-sigma^2,
            (y-mu)^4 - 3*sigma^4)
  M = rowMeans(m)
  w = m%*%t(m)/N
  val = -1/2*t(M)%*%solve(w)%*%M
  return(val)
}

wynik = maxNR(Qmin, start = c(mean(y),sd(y)))
summary(wynik)

vcov = -solve(wynik$hessian)
std.err = sqrt(vcov)[1,1]
(z.test = (wynik$estimate[1]-3)/std.err*sqrt(N))

(p.value = 2*(1-pnorm(abs(z.test), mean=0, sd=1)))

#p-value = 28% brak podstaw do odrzucenia H0

R =diag(2)
theta = wynik$estimate
q = rbind(3,1)
s = R%*%theta-q
(W = N*t(s)%*%solve(R%*%vcov%*%t(R))%*%s)
alpha = 0.05
g = 2
qchisq(1-alpha,g)
# Statystyka nie wpada do obszaru krytycznego brak podstaw do odrzuceni H0

#H0=3
R =cbind(1,0)
theta = wynik$estimate
q = 3
s = R%*%theta-q
(W = N*t(s)%*%solve(R%*%vcov%*%t(R))%*%s)
alpha = 0.05
g = 2
qchisq(1-alpha,g)

z.test^2
