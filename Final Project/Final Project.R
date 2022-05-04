dane = read.csv("gapminder_alcohol.csv") N = length(dane$country)
x = dane$alcconsumption

##### METODA MOMENTOW ##### 

# ROZKLAD NORMALNY
y = dane$alcconsumption uklad.rownan = function(x){
  mu = x[1]
  sigma = x[2]
  r1 = mean(y)-mu
  r2 = var(y)-sigma^2 return(c(r1,r2))
}
wynik_norm = multiroot(f=uklad.rownan,start=c(1,1)) wynik_norm$root
ks.test(y, "pnorm", wynik_norm$root[1], wynik_norm$root[2]) 

# ROZKLAD LOGNORMALNY
y = dane$alcconsumption uklad.rownan = function(x){
  mu = x[1]
  sigma = x[2]
  r1 = mean(y)-exp(mu+(sigma^2)/2) r2 = median(y)-exp(mu) return(c(r1,r2))
}
wynik_lnorm = multiroot(f=uklad.rownan,start=c(1,1)) wynik_lnorm$root
ks.test(y, "plnorm", wynik_lnorm$root[1], wynik_lnorm$root[2])

##### METODA NAJWIEKSZEJ WIARYGODNOSCI ##### 

# ROZKLAD NORMALNY

mean(x) sd(x) N=length(x)
lnL = function(parametry){
  mu = parametry[1]
  sigma = parametry[2]
  val = -N/2*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((x-mu)^2)
}
gradient = function(parametry){ mu = parametry[1]
sigma = parametry[2]
gr = rep(0,times=length(parametry))
gr[1] = sum(x-mu)/sigma^2
gr[2] = -N/sigma+sum((x-mu)^2)/sigma^3 return(gr)
}
hessian = function(parametry){ mu = parametry[1]
sigma = parametry[2]
h = matrix(0, nrow=2, ncol=2) h[1,1] = -N/sigma^2
h[2,2] = N/sigma^2-3*sum((x-mu)^2)/sigma^4 h[1,2] = -2*sum(x-mu)/sigma^3
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
  gr[2] = -N/(2*sigma^2) + sum((log(x) - mu)^2) / (2*sigma^4) return(gr)
}
hessian = function(parametry){ mu = parametry[1]
sigma = parametry[2]
h = matrix(0, nrow=2, ncol=2) h[1,1] = -N / sigma^2
h[2,2] = N / (2*sigma^4) - 2* sum((log(x) - mu)^2) / (2*sigma^6) h[1,2] = 0
h[2,1] = 0
return(h)
}
wynik_lnorm_MNW = maxNR(lnl, gradient, hessian, start=c(wynik_lnorm$root[1],wynik_lnorm$root[2])) summary(wynik_lnorm_MNW)
ks.test(x, "plnorm", wynik_lnorm_MNW$estimate[1], wynik_lnorm_MNW$estimate[2])

##### TESTOWANIE HIPOTEZ ##### 

## TEST Z

# NORMALNY

mu_norm_z = exp(1.95)

vcov = -solve(wynik_norm_MNW$hessian)
std.err.k = sqrt(vcov[1,1])
z.test = (wynik_norm_MNW$estimate[1]- mu_norm_z)/std.err.k print(z.test)
p.value = 2*(1-pnorm(abs(z.test),mean=0,sd=1)) print(p.value)

#LOG-NORMALNY 

mu_lognorm_z = 1.95
vcov = -solve(wynik_lnorm_MNW$hessian)
std.err.k = sqrt(vcov[1,1])
z.test = (wynik_lnorm_MNW$estimate[1]- mu_lognorm_z)/std.err.k print(z.test)
p.value = 2*(1-pnorm(abs(z.test),mean=0,sd=1)) print(p.value)

## TEST LR

# NORMALNY

mu_test_norm = exp(1.95) 
sigma_test_norm = exp(1.55)
lnL_U = wynik_norm_MNW$maximum
lnL_R = lnL(c(mu_test_norm,sigma_test_norm)) 
LR.test.norm = 2*(lnL_U-lnL_R) print(LR.test.norm)
alpha = 0.05
g =2
qchisq(1-alpha, df=g)
pchisq()
p.value = 1-pchisq(q = LR.test.norm, df = g) print(p.value)

# LOGNORMALNY 
mu_test_lnorm = 1.95 
sigma_test_lnorm = 1.55
lnL_U = wynik_lnorm_MNW$maximum
lnL_R = lnl(c(mu_test_lnorm,sigma_test_lnorm)) 
LR.test.lnorm = 2*(lnL_U-lnL_R) print(LR.test.lnorm)
p.value = 1-pchisq(q = LR.test.lnorm, df = g) print(p.value)