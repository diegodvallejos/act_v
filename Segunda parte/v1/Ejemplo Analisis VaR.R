#Ejemplo de análisis de VaR de cartera--------
rm(list = ls())
library(tidyverse)

#Datos

Precios=c(10,40,20)
Cantidades=c(300,200,250)
sigmas=matrix(c(25,15,20)/100,nrow = 3) #Volatilidad Anual

data=cbind(Precios,Cantidades,sigmas)
colnames(data)[3] <- "Volatilidad"
data= cbind(Acciones = c("A", "B", "C"),data)
data

(correlaciones=matrix(c(1.0,0.7,0.5,
                       0.7,1.0,0.6,
                       0.5,0.6,1.0),nrow = 3))

covarianzas = matrix(rep(NA,9),nrow = 3)
for(i in 1:3){
  for(j in 1:3){
    covarianzas[i,j]=sigmas[i]*sigmas[j]*correlaciones[i,j]
  }
}
covarianzas

(W=Precios*Cantidades)

(w_porcentaje = matrix(W/sum(W), nrow = 3 ))

h=10/252 #VaR a 10 dias sobre los 252 habiles
c=0.99 #Nivel de Confianza
z=qnorm(c) #Supongo distribucion normal

VaR_Ind=z*sigmas*sqrt(h)*W
print(VaR_Ind)

ponderadores=cbind(W,w_porcentaje,VaR_Ind)
ponderadores=cbind(Acciones = c("A", "B", "C"),ponderadores)
colnames(ponderadores)[3] <- "%W"
colnames(ponderadores)[4] <- "VaR Individual"

ponderadores

(varianza_cartera=as.double(t(w_porcentaje) %*% covarianzas %*% w_porcentaje)) #Anual

(sigma_cartera = sqrt(varianza_cartera)) #Anual

(VaR_Diversificado=z*sigma_cartera*sqrt(h)*sum(W))

(Betas= (covarianzas %*% w_porcentaje)/varianza_cartera)

(VaR_Marginal=VaR_Diversificado/ sum(W)*Betas)

(VaR_Component=VaR_Diversificado * as.double(w_porcentaje)*Betas)

(Porcentaje_VaR=VaR_Component/VaR_Diversificado)

tabla=cbind(Acciones = c("A", "B", "C"),Betas,VaR_Marginal,VaR_Component,Porcentaje_VaR)
colnames(tabla)[2] <- "Beta"
colnames(tabla)[3] <- "VaR Marginal"
colnames(tabla)[4] <- "Component VaR"
colnames(tabla)[5] <- "%VaR"
View(tabla)

#Chequeo de VaR Diversificado
sum(VaR_Component)
VaR_Diversificado
