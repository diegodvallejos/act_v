rm(list = ls())
library(tidyverse)

Precios=c(10,40,20)
Cantidades=c(300,200,250)
sigmas=matrix(c(25,15,20)/100,nrow = 3)
correlaciones=matrix(c(1.0,0.7,0.5,
                       0.7,1.0,0.6,
                       0.5,0.6,1.0),nrow = 3)
covarianzas = matrix(rep(NA,9),nrow = 3)
for(i in 1:3){
  for(j in 1:3){
    covarianzas[i,j]=sigmas[i]*sigmas[j]*correlaciones[i,j]
  }
}

W=Precios*Cantidades
w_porcentaje = matrix(W/sum(W), nrow = 3 )
h=10/252
c=0.99
z=qnorm(c)

VaR_Ind=z*sigmas*sqrt(h)*W
print(VaR_Ind)

varianza_cartera=as.double(t(w_porcentaje) %*% covarianzas %*% w_porcentaje )

sigma_cartera = sqrt(varianza_cartera)

VaR_Diversificado=z*sigma_cartera*sqrt(h)*sum(W)

Betas= (covarianzas %*% w_porcentaje)/varianza_cartera

VaR_Marginal=VaR_Diversificado/ sum(W)*Betas

VaR_Component=VaR_Diversificado * as.double(w_porcentaje)*Betas

Porcentaje_VaR=VaR_Component/VaR_Diversificado
