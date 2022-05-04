exam = c(15, 27, 37, 36, 44, 17, 47, 34, 40, 49, 25, 46, 42, 50, 48, 39, 44, 49, 20, 48,
         19, 44, 53, 50, 54, 54, 15, 53, 47, 20, 35, 32, 15, 49, 27, 30, 34, 24, 52, 36,
         42, 34, 49, 43, 53, 54, 27, 41, 37, 43)
srednia = mean(exam)
odch.std = sd(exam)
median(exam)

#wspolczynnik zmiennosci 
sd(exam)/mean(exam)*100

Q1 = quantile(exam, 0.25)
Q3 = quantile(exam, 0.75)
Med = median(exam)
X=(Q3-2*Med+Q1)/(Q3-Q1)

#Zadanie 2
sigma = 2
N = 9
alpha = 0.05
mu0 = 2
srednia = 1.4

U = (srednia - mu0)/sigma*sqrt(N)
(lb = -qnorm(1-alpha))
U

#Zadanie 3
mu0 = 150
N = 65
m = 139
s = 9.8
alpha = 0.01

(T = (m-mu0)/s*sqrt(N))

(lb = qt(0.01,N-1))

#Zadanie 4

N = 50 
s2 = sqrt(107.3)
alpha = 0.05
sigma2 = 100

(chi = (N-1)*s2/sigma2)

(lb = qchisq(1-alpha, N-1))

#Zadanie 5

pop1 = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
pop2 = c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
pop3 = c(6.31, 5.12, 5.54, 5.50, 5.37, 5.29, 4.92, 6.15, 5.80, 5.26)

