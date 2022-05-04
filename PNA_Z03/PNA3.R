# PNA

#Zadanie 1
#a)
curve(2*x-4,from=0,to+1)
#b)
curve(x**3-2*x**2+x-6, from=0, to=1)
#c)
curve(sin(10*pi/3*x), from=0, to=2*pi, n=100)

#Zadanie 2
setwd("C:\\Users\\Piotrek.DESKTOP-6E2FTU6\\Desktop\\1st Master\\PNA\\PNA_Z03")
dane = read.csv("EngScoResults.csv", header=TRUE, sep=";")
View(dane)
dane$Rok[44] = 1920
dane$Rok[64] = 1947
#a) liczba goli zdobytych goli przez Anglikow
plot(x=dane$Rok, y=dane$Goals.England)
#b) liczba goli zdobytych przez Szkotow w meczach w Szkocji
index = which(dane$Host == "Scotland")
plot(x=dane$Rok[index], y= dane$Goals.Scotland[index])

#Zadanie3
dane = read.csv(file="PNA_Z03.csv", header=TRUE, sep=",")
View(dane)

centyle = seq(from=0.01, to=0.99, by=0.01)
kwantyle_emp = quantile(x=dane$x, probs=centyle)
kwantyle_teo = qnorm(p=centyle, mean=0,sd=1)
plot(kwantyle_emp, kwantyle_teo)
abline(a=0, b=1, col="red")

#Zadanie 5
set.seed(23)
tst = rt(n=1000, df=3)
centyle =seq(from=0.01, to=0.99, by=0.01)
kw.emp = quantile(x=tst, probs=centyle)

#a) normalny
kw.teo = qnorm(p=centyle, mean=0, sd=1)
plot(kw.emp, kw.teo)
abline(a=0, b=1, col="blue")
#b)
kw.teo = qt(p=centyle, df=3)
plot(kw.emp, kw.teo)
abline(a=0, b=1, col="blue")

#Zadanie 4
exp = rexp(1000, rate= 1)
centyle =seq(from=0.01, to=0.99, by=0.01)
kw.emp = quantile(x=exp, probs=centyle)

kw.teo = qgamma(p=centyle,shape=1, rate=1)
plot(kw.emp, kw.teo)
abline(a=0, b=1, col="blue")

#Zadanie 6