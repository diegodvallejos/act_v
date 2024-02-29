#Diego Demian Vallejos
#Numero de registro: 899536

#Ejercicio 1

library(tidyverse)
library(quantmod) #conectarse con yahoo finance y traer precios historicos

rm(list = ls())
graphics.off()


getSymbols(c('TS', 'GGAL','NKE'), from = "2021-01-01", to = '2023-12-01')

#CrO un conjunto de datos con los precios de cierre
sum(!(index(TS) == index(GGAL)))
sum(!(index(GGAL) == index(NKE)))# -> Chequeo que las fechas coincidan

Tabla = tibble(date = index(TS),
               TS = as.double (TS$TS.Close), #trabajo con los precios de cierre, pero puedo trabajar con cualquiera
               GGAL = as.double(GGAL$GGAL.Close),
               NKE = as.double(NKE$NKE.Close)) #as.double me da solo el numero (no la estructura matricial con la fecha)
Tabla


# a) Calculo volatilidad ---------------------------------------------------
calculo_de_ewma <- function(retornos, lambda = 0.94) {
  r_promedio <- 0
  sumatoria <- 0
  
  for (i in length(retornos):1) {
    sumatoria <- sumatoria + lambda^(length(retornos) - i) * (retornos[i] - r_promedio)^2
  }
  
  return((sqrt((1 - lambda) * sumatoria))*sqrt(252)) #anualizada
}



Rend_TS = log(Tabla$TS[2:nrow(Tabla)]/Tabla$TS[1:nrow(Tabla) - 1]) #Retornos = media
(volatilidad_TS = sd(Rend_TS)*sqrt(252)) #El año tiene aproximadamente 252 dias habiles. Como aca estoy calculando los retornos diarios, los anualizo multiplicando por la raiz de 252

Rend_GGAL = log(Tabla$GGAL[2:nrow(Tabla)]/Tabla$GGAL[1:nrow(Tabla) - 1])
(volatilidad_GGAL = sd(Rend_GGAL)*sqrt(252))

Rend_NKE = log(Tabla$NKE[2:nrow(Tabla)]/Tabla$NKE[1:nrow(Tabla) - 1]) #Retornos = media
(volatilidad_NKE = sd(Rend_NKE)*sqrt(252))

#Ahora por EWMA

volatilidad_TS_EWMA <- calculo_de_ewma(Rend_TS, 0.94)
volatilidad_GGAL_EWMA <- calculo_de_ewma(Rend_GGAL, 0.94)
volatilidad_NKE_EWMA <- calculo_de_ewma(Rend_NKE, 0.94)

volatilidad_TS_EWMA
volatilidad_GGAL_EWMA
volatilidad_NKE_EWMA

print("La diferencia principal entre ambas alternativas consiste en como se poneran las observaciones en cuanto a su antiguedad. Siendo EWMA una forma de hacer que sean exponencialmente menos importantes las observaciones mas antiguas permitiendo esto por la incorporacion del ponderador lambda")

#b)Correlaciones -----------------------------------------------------------------

rendimientos <- data.frame(RA=Rend_TS,RI=Rend_GGAL, RG=Rend_NKE)
volat <- c(volatilidad_TS,volatilidad_GGAL, volatilidad_NKE)
matriz_corr = cor(rendimientos) #Correlacion
matriz_corr

mat_varcov <- diag(volat) %*% matriz_corr %*% diag(volat)
mat_varcov

covarianza_exponencial <- function(lambda = 0.94, data1, data2) {
  first_step <- 0
  for (j in 1:(length(data1)-1)) {
    first_step <- first_step + lambda^(j-1) * data1[length(data1)-j+1] * data2[length(data2)-j+1]
  }
  cov_exp <- 252 * (1 - lambda) / (1 - lambda^(length(data1)-1)) * first_step
  return(cov_exp)
}

#Covarianza EWMA

covar_TS_GGAL <-covarianza_exponencial(0.94, Rend_TS, Rend_GGAL)
covar_GGAL_NKE <-covarianza_exponencial(0.94, Rend_GGAL, Rend_NKE)
covar_TS_NKE <- covarianza_exponencial(0.94, Rend_TS, Rend_NKE)

covar_TS_GGAL
covar_GGAL_NKE
covar_TS_NKE

#c) VaR parametrico

c <- 0.99 #Nivel de confianza
z <- qnorm(c) #Percentil de la distribucion normal
h <- 5/252 # horizonte de tiempo= 5 dias, 252 dias habiles --> años

## Valuacion -------------------------------------------------------------

W_TS_ = 100 * Tabla$TS[nrow(Tabla)] #de mi variable stoc data, de la columna de apple mi ultima fila (el ultimo precio) nrow --> ultima fila
W_GGAL_ = 100 * Tabla$GGAL[nrow(Tabla)] #Valor de la posicion en GGAL
W_NKE_ = 100 * Tabla$NKE[nrow(Tabla)]

W_ = W_TS_ + W_GGAL_ + W_NKE_ #Total de mi posicion
w_ <- c(W_TS_, W_GGAL_, W_NKE_)/W_ #Proporcion invertida 


volat_P <- sqrt(t(w_)%*%mat_varcov%*% w_) %>% as.double() #Desvio estandar de mi portafolio 



(VaR_TS_Normal = z*volatilidad_TS_EWMA*sqrt(h)*W_TS_) #VaR si solo tuviera mis 700 acciones de apple #normal = diversificado
z*sd(Rend_TS)*sqrt(5)*W_TS_
(VaR_TS_Normal/W_TS_ )#Son un 6.7% de la cartera. Hay un 1% de probabiidad de que la perdida en un horizonte de 5 dias, sea superior a 892 dolares, si solo tengo a apple en mi cartera 

(VaR_GGAL_Normal = z*volatilidad_GGAL_EWMA*sqrt(h)*W_GGAL_)
z*sd(Rend_GGAL)*sqrt(5)*W_GGAL_
(VaR_GGAL_Normal/W_GGAL_) #Si solo tuviera GGAL en el portafolio, en un horizonte de 5 dias, hay un 1% de probabilidad de que la perdida sea mayor a 766 dolares. Representa un 5.7% del portafolio

(VaR_NKE_Normal = z*volatilidad_NKE_EWMA*sqrt(h)*W_NKE_)
(VaR_NKE_Normal/W_NKE_)

(VaR_P_Normal_NoDiv <- VaR_TS_Normal + VaR_GGAL_Normal + VaR_NKE_Normal) #Suponiendo correlacion de 1 --> no hay beneficios de diversificacion
(VaR_P_Normal_NoDiv/W_) #6.2% del portafolio, mitad de camino de los W_A y W_I

(VaR_P_Normal <- z*volat_P*sqrt(h)*W_) #var relativo diversificado
VaR_P_Normal/W_ #Menor que si no hay benef por diversif
(Benef_Diver <- VaR_P_Normal_NoDiv - VaR_P_Normal)


#d) VaR No Parametrico----------------------------------------------------------
Tabla <- Tabla %>% #agrego una colummna en Tabla con %% mutate
  mutate( P =  w_[1]*Tabla$TS + w_[2]*Tabla$GGAL + w_[3]*Tabla$NKE) #P hace ref al portafolio
Rend_P = log(Tabla$P[2:nrow(Tabla)]/Tabla$P[1 : (nrow(Tabla) -1)])   #R_P rendimiento del portafolio (desde el segundo hasta el ultimo / el primero hasta el anteultimo)

plot(Rend_P, type = "l")

quantile(Rend_P, 1-c)    #Mis retornos diarios tienen una prob de 1% de tener una perdida mayor al 1,89%

VarABS_NoParam_1d <- -quantile(Rend_P, 1-c)*W_ #VaR estimado de mi portafolio a un dia. Si quiero horizonte mas largo por ej. 5 dias
VarABS_NoParam_1d
VarRel_NoParam_1d <- (mean(Rend_P)-quantile(Rend_P, 1-c))*W_
VarABS_NoParam_1d













