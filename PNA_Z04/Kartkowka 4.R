#Zadanie1
Niter = 10000
Nobs = 100
df = 5
wyniki = rep(NA, Niter)

for (x in 1:Niter){
  rt = rt(Nobs, df)
  KS = ks.test(rt, "pt", df)
  wyniki[x] = (KS$p.value>0.05)
}

prawidlowe = which(wyniki==TRUE)
liczba.przy = length(prawidlowe)
proc = liczba.przy/Niter
proc

#Zadanie2

curve(dgamma(x,2,3), from=0, to=10)
curve(dexp(x,2), from=0, to=10)

#Zadanie3
Niter = 15000
Nobs = 200
df1 = 2
df2 = 3
wyniki = rep(NA, Niter)

for (x in 1:Niter){
  gamma = rgamma(Nobs, df1, df2)
  KS = ks.test(gamma, "pexp", 2)
  wyniki[x]=(KS$p.value>0.05)
}

(popr = which(wyniki==TRUE))
liczba = length(popr)
proc = liczba/Niter
proc

#Zadanie 6

data = read.csv("beef.csv", header = TRUE, sep=",")
y = log(data$QB)
n = nrow(data)
x = rep(1, n)
x = cbind(x, log(data$IN), log(data$PB), log(data$PL), log(data$PP))

k = ncol(data)-1
beta = solve((t(x)%*%x))%*%t(x)%*%y
beta
(e = y - x%*%beta)
(s2e = sum(e^2)/(n-5))
(vcov = s2e*solve(t(x)%*%x))
t = beta[2]/(vcov[2,2]**0.5)
t
lb = qt(0.025,n-k-1)
ub=qt(0.975,n-k-1)
lb
ub

RSS = sum(e^2)
TSS = sum((y-mean(y))^2)
(R2 = 1 - RSS/TSS)
(R2adj = 1 -(N-1)/(N-k)*(1-R2))

#Zadanie7
y = c(5,4,3,2,1)
x = c(1,2,3,4,5)

plot(x,y)
beta = solve(t(x)%*%x)%*%t(x)%*%y
beta
(e = y - x%*%beta)
(s2e = sum(e^2)/(n-1))
RSS = sum(e^2)
TSS = sum((y-mean(y))^2)
(R2 = 1 - RSS/TSS)