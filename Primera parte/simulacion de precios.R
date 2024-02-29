#Movimiento browniano------------------------------------------------------------
Z0 <- 10
dt <- 1/365 #o 252, ahora discutimos
eps <- rnorm(1) #es para simular 1 solo valor
Z1 <- Z0 + eps * sqrt(dt)
Z1

#si simulo 1000 valores
eps <- rnorm(1000) 
Z1 <- Z0 + eps * sqrt(dt)
mean(Z1) #tiende a Z0, pero hay un margen de error por ser una simulación
sd(Z1) #tiende a sqrt(dt)
sqrt(dt)

diferenciaMean <- Z0 - mean(Z1)
diferenciaSd <- sqrt(dt) - sd(Z1)
diferenciaSd

library(tidyverse)
Z <- tibble(Z= c(Z0,rep(NA,364))) #construirlo con matrix 
for (i in 2:365){
  Z[i,1] = Z[i-1,1] + rnorm(1)*sqrt(dt)
}
Z

df = tibble(t= 1:365, Z= Z$Z) 
ggplot(df, aes(x=t, y=Z))+
  geom_line()

#ahora generamos para 1000
nsimu=1000
Z<-matrix(NA, ncol=365,nrow=nsimu) #col=fechas, row=simu
Z[,1] <- Z0

for (i in 1:nsimu){ #simu de simulaciones
  for (t in 2:365){
  Z[i,t] = Z[i,t-1] + rnorm(1)*sqrt(dt) #simu de tiempo
  }
}


plot(1:365,Z[1,],
     type="l",ylim=c(min(Z),max(Z)),
     ylab="Z", xlab="dia") #grafica el primer camino

for (i in 2:nsimu){
  lines(1:365,Z[i,],col=trunc(runif(1)*nsimu)) #el trunc es 1 color aleatorio
}

mean(Z[,365]) #es aprox Z0
sd(Z[,365]) #es aprox 1, por ser 1 año, porque suma todos los dt

sd(Z[,182]) #el desvio en medio año
sqrt(0.5) #debería ser similar a esto

#esto no sirve, porque los precios tienen una tendencia

#Movimiento browniano generalizado------------------------------------------------------------
#ahora generamos para 1000
nsimu=1000
X<-matrix(NA, ncol=365,nrow=nsimu) #col=fechas, row=simu
X[,1] <- Z0

#"a" es cuanto aumenta en cada time step, es en terminos anuales
a <- 0.5 #aumenta 50 cvos por día
#"b" cuanto es la desviacion estandar en cada time step
b <- 0.2 #aumenta 20 cvos por día

for (i in 1:nsimu){ #simu de simulaciones
  for (t in 2:365){
    X[i,t] = X[i,t-1] + (a*dt + b* rnorm(1)*sqrt(dt)) #el delta Z es el rnorm*sqrt
  }
}


plot(1:365,X[1,],
     type="l",ylim=c(min(X),max(X)),
     ylab="X", xlab="dia") #grafica el primer camino

for (i in 2:nsimu){
  lines(1:365,X[i,],col=trunc(runif(1)*nsimu)) #el trunc es 1 color aleatorio
}

mean(X[,365]) #es aprox Z0 + a*T (T=1)
sd(X[,365]) #es aprox b*T, por ser 1 año, porque suma todos los dt
colMeans(X) #calcula la media de cada columna

lines(1:365,colMeans(X),lwd=5,col="black") #dibuja la linea de media para cada time step 

SD <- matrix(NA,ncol=365,nrow=1)
for (t in 2:365){
  SD[1,t] = sd(X[,t])
}
#IC a 96% de confianza con los +/- 2SD
lines(1:365,colMeans(X)+2*SD,lwd=3,col="lightgreen")
lines(1:365,colMeans(X)-2*SD,lwd=3,col="lightgreen")

#desventaja: admite precios negativos

#movimiento geometrico browniano
nsimu=1000
X<-matrix(NA, ncol=365,nrow=nsimu) #col=fechas, row=simu
X[,1] <- Z0

#"a" es cuanto aumenta en cada time step, es en terminos anuales
mu <- 0.5 #aumenta 50 cvos por día
#"b" cuanto es la desviacion estandar en cada time step
sigma <- 0.2 #aumenta 20 cvos por día

for (i in 1:nsimu){ #simu de simulaciones
  for (t in 2:365){
    X[i,t] = X[i,t-1] + (mu*X[i,t-1]*dt + sigma*X[i,t-1]* rnorm(1)*sqrt(dt)) #el delta Z es el rnorm*sqrt
  }
}


plot(1:365,X[1,],
     type="l",ylim=c(min(X),max(X)),
     ylab="X", xlab="dia") #grafica el primer camino

for (i in 2:nsimu){
  lines(1:365,X[i,],col=trunc(runif(1)*nsimu)) #el trunc es 1 color aleatorio
}

mean(X[,365]) #es aprox Z0 + a*T (T=1)
sd(X[,365]) #es aprox b*T, por ser 1 año, porque suma todos los dt
colMeans(X) #calcula la media de cada columna

lines(1:365,colMeans(X),lwd=5,col="black") #dibuja la linea de media para cada time step 

SD <- matrix(NA,ncol=365,nrow=1)
for (t in 2:365){
  SD[1,t] = sd(X[,t])
}
#IC a 96% de confianza con los +/- 2SD
lines(1:365,colMeans(X)+2*SD,lwd=3,col="lightgreen")
lines(1:365,colMeans(X)-2*SD,lwd=3,col="lightgreen")

#tiene una tendencia "exponencial"
