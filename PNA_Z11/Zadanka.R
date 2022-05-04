# Przyklad z prezentacji

dane = c(-0.15,-0.22,0.09,-2.47,-1.02,-0.69,0.52,-0.04,-0.24,-2.74,-0.18,0.39,
         -1.14, 0.69,-0.99,-0.20,-0.40,0.33,1.02,-0.22,0.14,0.33,-1.26,0.23,
         -1.12,-1.65,-0.36,2.18,-0.39,-0.98,-0.22,1.63,-2.18,0.22,1.13,0.24,
         0.54,-0.85,-1.07,0.25)
N = length(dane)
mean(dane)

# petla bootsrapowa
N_iter = 1000
wektor_srednich = rep(NA, times=N_iter)
for (i in 1:N_iter){
  pdprobka = sample(x=dane, size=N, replace=TRUE)
  wektor_srednich[i] = mean(pdprobka)
}

hist(wektor_srednich)
abline(v=mean(dane), col="blue")
abline(v=mean(wektor_srednich), col="red")

#Zadanie 3

dane = c(4.11,3.55,3.37,4.46,3.69,3.44,1.64,3.29,3.57,3.35,3.30,1.58,1.75,3.30,2.68)
N = length(dane)

# przedzial ufnosci dla sredniej 

N_iter = 1000
wektor_srednich = rep(NA, times=N_iter)

for (i in 1:N_iter){
  podprobka = sample(dane, N, replace=TRUE)
  wektor_srednich[i]=mean(podprobka)
}

hist(wektor_srednich)

#Przedzialy ufnosci w bootstrapie
#Percentylowy przedzial ufnosci
alpha = 0.05
quantile(x=wektor_srednich,probs = c(alpha/2, 1-alpha/2))

# CI = (2.69, 3.55)

# Zadanie 4
library("resampledata")
dane = FishMercury
N_iter = 1000
N = length(dane$Mercury)
wektor_srednich = rep(NA, N_iter)

for (i in 1:N_iter){
  podprobka = sample(dane$Mercury, N, TRUE)
  wektor_srednich[i]=mean(podprobka)
}
alpha = 0.05
quantile(wektor_srednich, probs = c(alpha/2,1-alpha/2))

hist(wektor_srednich)
#CI = (0.11, 0.30)

# Zadanie 5

dane = c(431,450,431,453,481,449,441,476,460,482,472,465,421,452,451,430,458,446,466,476)
N = length(dane)
#H): theta=440

alpha =0.05

#wyznaczam rozklad statystki testowej za pomca petli bootsrapowej

N_iter = 1000
wektor.U.test = rep(NA, times=N_iter)

for(i in 1:N_iter){
  #losuje podprobke
  podprobka=sample(dane, N, TRUE)
  #wyznaczam stat. testowa
  U.test = (mean(podprobka)-mean(dane))/sd(podprobka)*sqrt(N)
  #zapisuje wynik
  wektor.U.test[i]=U.test
}

hist(wektor.U.test)

#przedzial ufnosci
przedzial.ufnosci = quantile(x = wektor.U.test, probs = c(alpha/2,1-alpha/2))
przedzial.ufnosci
# CI = (-2.01; 2.044)
(U.test = (mean(dane)-440)/sd(dane)*sqrt(N))

#Zadanie 6
dane = BookPrices
x = dane$Price[which(dane$Area=="Math & Science")]
N=length(x)

alpha = 0.05
N_iter = 1000
wektor.U.test = rep(NA, N_iter)

for (i in 1:N_iter){
  podprobka=sample(x,N,TRUE)
  wektor.U.test[i]=(mean(podprobka)-mean(x))/sd(podprobka)*sqrt(N)
}

przedzial.ufnosci = quantile(x = wektor.U.test, probs = c(alpha/2,1-alpha/2))
przedzial.ufnosci
# CI = (-2.16,2.12)
(U.test = (mean(x)-150)/sd(x)*sqrt(N))
