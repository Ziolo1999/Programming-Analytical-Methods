##### LIBRARIES #####
install.packages('dgof')
library("tseries")
library("maxLik")
library("resampledata")
library("rootSolve")
library("dgof")

##### QQ PLOT #####

x = rexp(1000, rate= 1) #obserwacje
centyle =seq(from=0.01, to=0.99, by=0.01)
kw.emp = quantile(x=x, probs=centyle)

kw.teo = qgamma(p=centyle,shape=1, rate=1) #wybierz odpowiedni rozklad
plot(kw.emp, kw.teo)
abline(a=0, b=1, col="blue")

##### JARQUE-BER TEST EKSPERYMENT #####

niter = 10000
nobs = 30
mean = 4
std = 2
wektor_wynikow = rep(NA, times = niter)

for (x in 1:niter){
  rn = rnorm(nobs, mean, std) #wybierz rozklad
  JB = jarque.bera.test(rn)
  wektor_wynikow[x]=(JB$p.value>0.05)
}
odrzucenia = which(wektor_wynikow==FALSE)
liczba.odrzucen = length(odrzucenia)
(procent = liczba.odrzucen/niter)

##### REGRESJA LINIOWA MNK #####

dane = read.csv("vacation.csv", header = TRUE, sep = ",") #wrzu? dane

n = nrow(dane)
x = cbind(rep(1, times = N), dane$income, dane$age, dane$kids) #ustaw zmienne niezale?ne
y = dane$miles #ustaw zmienne zale?ne

(beta.hat = solve((t(x)%*%x))%*%t(x)%*%y)

e = y - x%*%beta.hat
s2e = sum(e^2)/(n-4)
vcov = s2e*solve(t(x)%*%x)
print(vcov)

RSS = sum(e^2)
TSS = sum((y-mean(y))^2)
(R2 = 1 - RSS/TSS)
(R2adj = 1 -(N-1)/(N-4*(1-R2)))

##### FUNKCJA WIARYGODNOSCI ROZKLAD NORMALNY PARAMETR mu #####

x = c(51,115,150,190,217,228,350) #obserwacje mog? byc z df
N = length(x)
sigma = 1#dodac sigme

lnL = function(mu){ #wybierz parametr
  ll= -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2) #ustaw odpowiednia niewiadoma
  return(ll)
}

gradient = function(mu){#wybierz parametr
  gr = sum(x-mu)/sigma^2 #Pierwsza pochodna zlogarytmowanej funkcji wiarygodnosci
  return(gr)
}

hessian = function(mu){ #wybierz parametr
  h = -N/sigma^2 #Druga pochodna funkcji wiarygodnosci
  return(h)
}

wynik = maxNR(fn=lnL, grad=gradient, hess = hessian, start=1)
summary(wynik)

curve(lnL(x), from=0, to=0.01)
abline(v=wynik$estimate, col="blue")

##### FUNKCJA WIARYGODNOSCI ROZKLAD NORMALNY PARAMETR sigma #####

x = c(51,115,150,190,217,228,350) #obserwacje mog? byc z df
N = length(x)
mu = #dodac mu
  
  lnL = function(sigma){ #wybierz parametr
    ll= -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2) #ustaw odpowiednia niewiadoma
    return(ll)
  }

gradient = function(sigma){#wybierz parametr
  gr = -N/sigma+sum((x-mu)^2)/sigma^3 #Pierwsza pochodna zlogarytmowanej funkcji wiarygodnosci
  return(gr)
}

hessian = function(mu){ #wybierz parametr
  h = -2*sum(x-mu)/sigma^3 #Druga pochodna funkcji wiarygodnosci
  return(h)
}

wynik = maxNR(fn=lnL, grad=gradient, hess = hessian, start=1)
summary(wynik)

curve(lnL(x), from=0, to=0.01)
abline(v=wynik$estimate, col="blue")

##### FUNKCJA WIARYGODNO?CI ROZKLAD WYKLADNICZY #####

x = c(51,115,150,190,217,228,350) #obserwacje mog? byc z df
N = length(x)

lnL = function(lambda){
  ll= N*log(lambda)-lambda*sum(x) #zlogarytmowana funkcja wiarygodnosci (ln(pi(f(X))))
  return(ll)
}

gradient = function(lambda){
  gr = N/lambda-sum(x) #Pierwsza pochodna zlogarytmowanej funkcji wiarygodnosci
  return(gr)
}

hessian = function(lambda){
  h = -N/lambda^2 #Druga pochodna funkcji wiarygodnosci
  return(h)
}

wynik = maxNR(fn=lnL, grad=gradient, hess = hessian, start=1)
summary(wynik)

curve(lnL(x), from=0, to=0.01)
abline(v=wynik$estimate, col="blue")

##### FUNKCJA WIARYGODNOSCI ROZKLAD BERNOULLIEGO/DWUMIANOWY #####

# E[X] = n * p -- p = mean(x) / n 
# VAR[x] = n * p * (1 - p)

x = #obserwacje
n = length(x)
s = #sukcesy
p = n - s
lnl = function(p) {
  ll = 5*log(p)+3*log(1-p)
  return(ll)
}


##### FUNKCJA WIARYGODNOSCI ROZKLADU POISSONA #####

dane = read.csv("runshoes.csv ")
x = dane$shoes
n = length(x)
lnL = function(lambda) {
  ll = sum(x) * log(lambda) - lambda * n - sum(log(factorial(x)))
  return(ll)
}

gradient = function(lambda) {
  gr = sum(x) / lambda - n
  return(gr)
}

hessian = function(lambda) {
  h = - sum(x) / lambda^2
}

wynik = maxNR(fn=lnL, grad=gradient, hess = hessian, start=1)
summary(wynik)

curve(lnL(x), from=0, to=5)
abline(v=wynik$estimate, col="blue")

##### FUNKCJA WIARYGODNOSCI ROZKLADU CAUCHEGO #####
x=c(1,2,2,3,4)
N=length(x)

lnL=function(theta) {
  ll = -N*log(pi)-sum(log(1+(x-theta)^2))
  return(ll)
}

wynik=maxNR(fn=lnL, start=1)
summary(wynik)

theta_vector=seq(from=0, to=4, by=0.01)
lnL=0
for(i in 1:length(x)) {
  lnL=lnL+(-log(pi)-log(1+(x[i]-theta_vector)^2))
  
}
plot(theta_vector, lnL, type="l")

##### RYSOWANIE FUNKCJI WIARYGODNOSCI DWA PARAMETRY #####

x = c(4.76,0.35,0.04,
      -1.26,3.30,3.79,0.82,-1.18,-0.77,
      2.47,1.50,2.62,1.62,3.27,1.89,1.45,
      1.61,2.78,-0.98,2.41) #ustaw obserwacje moga byc z df
mean(x)
sd(x)
N=length(x)

f5= function(mu, sigma){
  val = -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2) #Funkcja (rozkl normalny by deafault )
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
persp3d(mu,sigma,exp(fval), col="yellow") #przeksztalcamy f logwiarygodnosci nawiarygodnosci
image(mu,sigma,fval)
image(mu,sigma,exp(fval))

##### METODA NAJWIEKSZEJ WIARYGODNOSCI ROZKLAD NORMALNY DWA PARAMETRY #####

x = c(4.76,0.35,0.04,
      -1.26,3.30,3.79,0.82,-1.18,-0.77,
      2.47,1.50,2.62,1.62,3.27,1.89,1.45,
      1.61,2.78,-0.98,2.41) #ustaw obserwacje moga byc z df
mean(x)
sd(x)
N=length(x)

lnL = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  val = -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2)
}

gradient = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  gr = rep(0,times=length(parametry))
  gr[1] = sum(x-mu)/sigma^2
  gr[2] = -N/sigma+sum((x-mu)^2)/sigma^3
  return(gr)
}

hessian = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  h = matrix(0, nrow=2, ncol=2)
  h[1,1] = -N/sigma^2
  h[2,2] = N/sigma^2-3*sum((x-mu)^2)/sigma^4
  h[1,2] = -2*sum(x-mu)/sigma^3
  h[2,1] = h[1,2]
  return(h)
}

wynik = maxNR(fn = lnL, grad=gradient, hess = hessian, start=c(2,2))
summary(wynik)

##### METODA NAJWIEKSZEJ WIARYGODNOSCI ROZKLAD GAMMA DWA PARAMETRY #####

x = #obserwacje
mean(x)
sd(x)
N=length(x)

lnl=function(parametry){
  k=parametry[1]
  theta=parametry[2]
  ll=(k-1)*sum(log(x))-sum(x/theta)-N*k*log(theta)-N*log(gamma(k))
  return(ll)
}



##### METODA WIARYGODNOSCI Z TESTOWANIEM ISTOTNOSCI PARAMETRU #####

x = dane$TimeDiff # obserwacje
n = length(x)

lnL = function(parametry){
  k = parametry[1]
  lambda = parametry[2]
  ll = n*log(k)-k*n*log(lambda)+(k-1)*sum(log(x))-sum((x/lambda)^k)
  return(ll)
}
wynik = maxNR(fn=lnL, start=c(1,1))
summary(wynik)

#TEST DLA DWOCH ZMIENNYCH

lnL_U = wynik$maximum
lnL_R = lnL(c(1,20))
LR.test = 2*(lnL_U-lnL_R)
print(LR.test)

alpha = 0.05
g = 2
qchisq(1-alpha, df=g)

#K = (5.99, inf)
#odrzucamy hipoteze zerowa

p.value = 1 - pchisq(LR.test, df=g)
print(p.value)

#odrzucamy

#TEST DLA JEDNEJ ZMIENNEJ k=1

vcov = -solve(wynik$hessian)
std.err.k = sqrt(vcov[1,1])
z.test = (wynik$estimate[1]- 1)/std.err.k
print(z.test)

p.value = 2*(1-pnorm(abs(z.test),mean=0,sd=1))
print(p.value)

#odrzucamy

#TEST DLA JEDNEJ ZMIENNEJ lambda=0

vcov = -solve(wynik$hessian)
std.err.lambda = sqrt(vcov[2,2])
z.test = (wynik$estimate[2]- 17)/std.err.lambda
print(z.test)

p.value = 2*(1-pnorm(abs(z.test),mean=0,sd=1))
print(p.value)

#brak podstaw do odrzucenia

##### METODA MOMENTOW DWA PARAMETRY #####

dane = read.csv("zep.csv") #wybierz baze danych
y = dane$variable #wybierz obserwacje
uklad.rownan = function(x){
  alpha = x[1]
  beta = x[2]
  r1 = mean(y)-alpha/beta
  r2 = var(y)-alpha/beta^2
  return(c(r1,r2))
}

wynik = multiroot(f=uklad.rownan,start=c(1,1))
wynik$root

ks.test(y, "pgamma", wynik$root[1], wynik$root[2])

##### ROZWIAZANIE WIELOMIANOW #####

f2 = function(x){
  val = x^4-8*x^3+10*x^2-3*x+9
  return(val)
}

curve(f2, from=0, to=10)

wynik = uniroot(f2, interval=c(0,4))
print(wynik$root)


##### PROJEKT ####
View(dane)
dane = dane[c(1,2)]
dane=na.omit(dane)
plot(density(dane$alcconsumption))
dane = read.csv("gapminder_alcohol.csv")
N = length(dane$country)
x = dane$alcconsumption

##### METODA MOMENTOW #####
# ROZKLAD NORMALNY

y = dane$alcconsumption
uklad.rownan = function(x){
  mu = x[1]
  sigma = x[2]
  r1 = mean(y)-mu
  r2 = var(y)-sigma^2
  return(c(r1,r2))
}

wynik_norm = multiroot(f=uklad.rownan,start=c(1,1))
wynik_norm$root

ks.test(y, "pnorm", wynik_norm$root[1], wynik_norm$root[2])

# ROZKLAD LOGNORMALNY

y = dane$alcconsumption
uklad.rownan = function(x){
  mu = x[1]
  sigma = x[2]
  r1 = mean(y)-exp(mu+(sigma^2)/2)
  r2 = median(y)-exp(mu)
  return(c(r1,r2))
}

wynik_lnorm = multiroot(f=uklad.rownan,start=c(1,1))
wynik_lnorm$root

ks.test(y, "plnorm", wynik_lnorm$root[1], wynik_lnorm$root[2])


##### METODA NAJWIEKSZEJ WIARYGODNOSCI #####
# ROZKLAD NORMALNY 

mean(x)
sd(x)
N=length(x)

lnL = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  val = -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2)
}

gradient = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  gr = rep(0,times=length(parametry))
  gr[1] = sum(x-mu)/sigma^2
  gr[2] = -N/sigma+sum((x-mu)^2)/sigma^3
  return(gr)
}

hessian = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  h = matrix(0, nrow=2, ncol=2)
  h[1,1] = -N/sigma^2
  h[2,2] = N/sigma^2-3*sum((x-mu)^2)/sigma^4
  h[1,2] = -2*sum(x-mu)/sigma^3
  h[2,1] = h[1,2]
  return(h)
}

wynik_norm_MNW = maxNR(fn = lnL, grad=gradient, hess = hessian, start=c(wynik_norm$root[1], wynik_norm$root[2]))
summary(wynik_norm_MNW)

ks.test(x, "pnorm", wynik_norm_MNW$estimate[1], wynik_norm_MNW$estimate[2])

# ROZKLAD LOGNORMALNY 

lnl = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  val = -sum(log(x)) - N/2 * log(2*pi) - N*log(sigma) - 1/(2*sigma^2)*sum((log(x) - mu)^2)
}

gradient = function(parametry) {
  mu = parametry[1]
  sigma = parametry[2]
  gr = rep(0,times=length(parametry))
  gr[1] = sum(log(x))/sigma^2 - (2*N*mu) / (2*sigma^2)
  gr[2] = -N/(2*sigma^2) + sum((log(x) - mu)^2) / (2*sigma^4)
  return(gr)
}

hessian = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  h = matrix(0, nrow=2, ncol=2)
  h[1,1] = -N / sigma^2
  h[2,2] = N / (2*sigma^4) - 2* sum((log(x) - mu)^2) / (2*sigma^6)
  h[1,2] = 0
  h[2,1] = 0
  return(h)
}

wynik_lnorm_MNW = maxNR(lnl, gradient, hessian, start=c(wynik_lnorm$root[1],wynik_lnorm$root[2]))
summary(wynik_lnorm_MNW)

ks.test(x, "plnorm", wynik_lnorm_MNW$estimate[1], wynik_lnorm_MNW$estimate[2])

##### TESTOWANIE HIPOTEZ #####
## TEST Z
# NORMALNY
mu_norm_z = exp(1.95)

vcov = -solve(wynik_norm_MNW$hessian)
std.err.k = sqrt(vcov[1,1])
z.test = (wynik_norm_MNW$estimate[1]- mu_norm_z)/std.err.k
print(z.test)

p.value = 2*(1-pnorm(abs(z.test),mean=0,sd=1))
print(p.value)

#LOG-NORMALNY
mu_lognorm_z = 1.95

vcov = -solve(wynik_lnorm_MNW$hessian)
std.err.k = sqrt(vcov[1,1])
z.test = (wynik_lnorm_MNW$estimate[1]- mu_lognorm_z)/std.err.k
print(z.test)

p.value = 2*(1-pnorm(abs(z.test),mean=0,sd=1))
print(p.value)



## TEST LR
# NORMALNY

mu_test_norm = exp(1.95)
sigma_test_norm = exp(1.55)

lnL_U = wynik_norm_MNW$maximum
lnL_R = lnL(c(mu_test_norm,sigma_test_norm))
LR.test.norm = 2*(lnL_U-lnL_R)
print(LR.test.norm)

alpha = 0.05
g = 2
qchisq(1-alpha, df=g)
pchisq()
p.value = 1-pchisq(q = LR.test.norm, df = g)
print(p.value)

# LOGNORMALNY
mu_test_lnorm = 1.95
sigma_test_lnorm = 1.55

lnL_U = wynik_lnorm_MNW$maximum
lnL_R = lnl(c(mu_test_lnorm,sigma_test_lnorm))
LR.test.lnorm = 2*(lnL_U-lnL_R)
print(LR.test.lnorm)

p.value = 1-pchisq(q = LR.test.lnorm, df = g)
print(p.value)

