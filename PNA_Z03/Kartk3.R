#Zadanie 1
#a)
pole_kola = function(x){
  pole = pi*x^2
  return(pole)
}
pole_kola(2)

#Zadanie 2
objetosc = function(a,b=a,c=a){
  obj = a*b*c
  return(obj)
}
objetosc(2,3,4)
objetosc(2)

#Zadanie 3
t120 = rt(120,3)
t2000 = rt(2000,40)
centyle=seq(from=0.01, to=0.99, by=0.01)

kwantyl_emp_t120=quantile(t120, probs=centyle)
kwantyl_emp_t2000=quantile(t2000, probs=centyle)

kwantyl_teo_norm = qnorm(centyle)
kwantyl_teo_t120 = qt(centyle, 3)
kwantyl_teo_t2000 = qt(centyle, 40)

plot(kwantyl_teo_norm, kwantyl_emp_t120)
abline(a=0, b=1)

plot(kwantyl_teo_t120, kwantyl_emp_t120)
abline(a=0,b=1)

plot(kwantyl_teo_norm,kwantyl_emp_t2000)
abline(a=0, b=1)

plot(kwantyl_emp_t2000, kwantyl_teo_t2000)
abline(a=0,b=1)



e3 = rexp(1200, 1/3)
centyle = seq(0.01, 0.99, 0.01)
kw.emp = quantile(e3, probs=centyle)
kw.teo = qnorm(centyle)
plot(kw.teo, kw.emp)
abline(a=0, b=1)









#Zadanie 4

dane = read.csv("EngScoResults.csv", header = TRUE, sep = ";")
View(dane)

plot(dane$Rok,dane$Goals.Scotland)

curve(x*sin(x),from=0, to=20*pi)
