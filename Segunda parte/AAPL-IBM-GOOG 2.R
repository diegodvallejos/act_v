#Ejercicio VAR 3 variables

library(flextable)
library(tidyverse)
library(ggplot2)
library(quantmod)

#cantidades dadas

#Datos
Tabla <- tibble(Acciones = c("AAPL","IBM","GOOG"),
                Cantidades = c(200,300,350),
                Precio = c(190,134,120),
                Volatilidad = c(0.22,0.18,0.24))

Tabla

c<- 0.995
Zc <- qnorm(c)
h<- 5/252

rho_AAPL_IBM <- 0.7
rho_AAPL_GOOG <- 0.2
rho_IBM_GOOG <- 0.24

W_AAPL <- Tabla$Cantidades[1]*Tabla$Precio[1]
W_IBM <- Tabla$Cantidades[2]*Tabla$Precio[2]
W_GOOG <- Tabla$Cantidades[3]*Tabla$Precio[3]

W <- W_AAPL+W_IBM+W_GOOG

w<- c(W_AAPL, W_IBM, W_GOOG)/W
w

#Calculo el VaR Normal (parametrico) Individual y Cartera
VaR_AAPL_Indiv <- Zc*Tabla$Volatilidad[1]*sqrt(h)*W_AAPL
VaR_AAPL_Indiv

VaR_IBM_Indiv <- Zc*Tabla$Volatilidad[2]*sqrt(h)*W_IBM
VaR_IBM_Indiv

VaR_GOOG_Indiv <- Zc*Tabla$Volatilidad[3]*sqrt(h)*W_GOOG
VaR_GOOG_Indiv

VaR_P_Indiv_nd <- VaR_AAPL_Indiv+VaR_IBM_Indiv+VaR_GOOG_Indiv
VaR_P_Indiv_nd

#VaR Diversificado
matriz_correlaciones = matrix(c(1, rho_AAPL_IBM,rho_AAPL_GOOG,
                      rho_AAPL_IBM,1,rho_IBM_GOOG,
                      rho_AAPL_GOOG,rho_IBM_GOOG,1), nrow=3)
matriz_correlaciones

matriz_varcov = matrix(c(Tabla$Volatilidad[1]^2, Tabla$Volatilidad[1]*Tabla$Volatilidad[2]*rho_AAPL_IBM, Tabla$Volatilidad[1]*Tabla$Volatilidad[3]*rho_AAPL_GOOG,
                         Tabla$Volatilidad[1]*Tabla$Volatilidad[2]*rho_AAPL_IBM, Tabla$Volatilidad[2]^2 , Tabla$Volatilidad[2]*Tabla$Volatilidad[3]*rho_IBM_GOOG,
                         Tabla$Volatilidad[1]*Tabla$Volatilidad[3]*rho_AAPL_GOOG, Tabla$Volatilidad[2]*Tabla$Volatilidad[3]*rho_IBM_GOOG, Tabla$Volatilidad[3]^2), nrow=3)
matriz_varcov

matriz_varcov_2 <- diag(Tabla$Volatilidad) %*% matriz_correlaciones %*% diag(Tabla$Volatilidad) #USAR ESTA QUE ES MAS CORTA

volatilidad_P <- sqrt(t(w)%*% matriz_varcov %*% w) %>% as.double()#volatilidad anual del portafolio/desvio estandar del portafolio
volatilidad_P

varianza_anual <- volatilidad_P^2
varianza_anual

VaR_P_div <- Zc*volatilidad_P*sqrt(h)*W
VaR_P_div

#VaR Marginal
beta <- (matriz_varcov%*%w)/varianza_anual
beta

sum(t(w)%*% beta) #chequeo que tiene que dar 1

VaR_Marginal<- VaR_P_div/W*beta
VaR_Marginal

#Component VaR
Component_VaR <- VaR_P_div*w*beta
Component_VaR

sum(Component_VaR)-VaR_P_div #chequeo que suma de component var me tiene que dar lo mismo, osea la resta =0 no coincide por 0.00jkJZBk

#%VaR
(Component_VaR/VaR_P_div)


#EN BASE A YAHOO FINANCE CON 6 MESES
library(tidyverse)
library(quantmod) #conectarse con yahoo finance y traer precios historicos

rm(list = ls())
graphics.off()


getSymbols(c('AAPL', 'IBM','GOOG'), from = "2023-01-01", to = '2023-06-30')

#Crear un conjunto de datos con los precios de cierre
sum(!(index(AAPL) == index(IBM)))
sum(!(index(IBM) == index(GOOG)))#Chequeo que las fechas coincidan. Si me da 0 es que todas las fechas son iguales

Tabla = tibble(date = index(AAPL),
                    AAPL = as.double (AAPL$AAPL.Close), #trabajo con los precios de cierre, pero puedo trabajar con cualquiera
                    IBM = as.double(IBM$IBM.Close),
                    GOOG = as.double(GOOG$GOOG.Close)) #as.double me da solo el numero (no la estructura matricial con la fecha)
Tabla

#Rendimientos logaritmicos--------------------------------------------------------------
Rend_AAPL = log(Tabla$AAPL[2:nrow(Tabla)]/Tabla$AAPL[1:nrow(Tabla) - 1]) #Retornos = media
volatilidad_AAPL = sd(Rend_AAPL)*sqrt(252) #El año tiene aproximadamente 252 dias habiles. Como aca estoy calculando los retornos diarios, los anualizo multiplicando por la raiz de 252

Rend_IBM = log(Tabla$IBM[2:nrow(Tabla)]/Tabla$IBM[1:nrow(Tabla) - 1])
volatilidad_IBM = sd(Rend_IBM)*sqrt(252)
 
Rend_GOOG = log(Tabla$GOOG[2:nrow(Tabla)]/Tabla$GOOG[1:nrow(Tabla) - 1]) #Retornos = media
volatilidad_GOOG = sd(Rend_GOOG)*sqrt(252)

Tabla <- Tabla%>%
  mutate(Rend_AAPL = c(NA, Rend_AAPL), #NA pq Apple tiene solo 122 valores de retorno contra 123 precios
         Rend_IBM = c(NA, Rend_IBM),
         Rend_GOOG = c(NA, Rend_GOOG)) #Vector de retorno

rendimientos <- data.frame(RA=Rend_AAPL,RG=Rend_GOOG,RI=Rend_IBM)
volat <- c(volatilidad_AAPL,volatilidad_IBM, volatilidad_GOOG)
matriz_corr = cor(rendimientos) #Correlacion

mat_varcov <- diag(volat) %*% matriz_corr %*% diag(volat)  #matriz de correlaciones ya anualizada

#Crear el grafico de lineas de correlacion---------------------------------------------------
ggplot(Tabla, aes(x = date)) + 
  geom_line(aes(y = AAPL, color = "AAPL")) +
  geom_line(aes(y = IBM, color = "IBM")) +
  geom_line(aes(y = GOOG, color = "GOOG"))
  scale_color_manual(values = c("AAPL" = "lightblue", "IBM" = "pink", "GOOG"= "yellow")) +
  labs(title = "Precios de cierre de acciones",
       x = "Fecha",
       y = "Precio de cierre",
       color = "Acciones") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b - %y") +
  theme(axis.text.x = element_text(angle = 90))

#Crear el grafico de lineas rendimiento diario---------------------------------------------------
ggplot(Tabla, aes(x = date)) + 
  geom_line(aes(y = Rend_AAPL, color = "AAPL")) +
  geom_line(aes(y = Rend_IBM, color = "IBM")) +
  geom_line(aes(y = Rend_GOOG, color = "GOOG"))
  scale_color_manual(values = c("AAPL" = "lightblue", "IBM" = "pink", "GOOG"= "yellow")) +
  labs(title = "Rendimiento diario",
       x = "Fecha",
       y = "Precio de cierre",
       color = "Acciones") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b - %y") +
  theme(axis.text.x = element_text(angle = 90))



#Analisis de Var: Cartera con 70 acciones AAPL y 100 de IBM y 50 GOOG--------------------
c <- 0.99 #Nivel de confianza
z <- qnorm(c) #Percentil de la distribucion normal
h <- 5/252 # horizonte de tiempo= 5 dias, 252 dias habiles --> años

## Valuacion -------------------------------------------------------------

W_AAPL_ = 70 * Tabla$AAPL[nrow(Tabla)] #de mi variable stoc data, de la columna de apple mi ultima fila (el ultimo precio) nrow --> ultima fila
W_IBM_ = 100 * Tabla$IBM[nrow(Tabla)] #Valor de la posicion en IBM
W_GOOG_ = 50* Tabla$GOOG[nrow(Tabla)]

W_ = W_AAPL_ + W_IBM_ + W_GOOG_ #Total de mi posicion
w_ <- c(W_AAPL_, W_IBM_, W_GOOG_)/W_ #Proporcion invertida 


volat_P <- sqrt(t(w_)%*%mat_varcov%*% w_) %>% as.double() #Desvio estandar de mi portafolio 


## VaR No Paramétrico -----------------------------------------------------
#Ultimos 6 meses --> decidir tiempo a tomar (historia)
Tabla <- Tabla %>% #agrego una colummna en Tabla con %% mutate
  mutate( P =  w_[1]*Tabla$AAPL + w_[2]*Tabla$IBM + w_[3]*Tabla$GOOG) #P hace ref al portafolio
Rend_P = log(Tabla$P[2:nrow(Tabla)]/Tabla$P[1 : (nrow(Tabla) -1)])   #R_P rendimiento del portafolio (desde el segundo hasta el ultimo / el primero hasta el anteultimo)

plot(Rend_P, type = "l")

quantile(Rend_P, 1-c)    #Mis retornos diarios tienen una prob de 1% de tener una perdida mayor al 1,89%

VarABS_NoParam_1d <- -quantile(Rend_P, 1-c)*W_ #VaR estimado de mi portafolio a un dia. Si quiero horizonte mas largo por ej. 5 dias
VarABS_NoParam_1d
VarRel_NoParam_1d <- (mean(Rend_P)-quantile(Rend_P, 1-c))*W_
VarABS_NoParam_1d

#para hacer por mas dias, no puedo hacer VarABS 1d x 5 porque hay overlapping porque no son valores independientes. (no lo toma)

## VaR Normal/Parametrico: Individual y Cartera ---------------------------------------
(VaR_AAPL_Normal = z*volatilidad_AAPL*sqrt(h)*W_AAPL_) #VaR si solo tuviera mis 700 acciones de apple #normal = diversificado
z*sd(Rend_AAPL)*sqrt(5)*W_AAPL_
(VaR_AAPL_Normal/W_AAPL_ )#Son un 6.7% de la cartera. Hay un 1% de probabiidad de que la perdida en un horizonte de 5 dias, sea superior a 892 dolares, si solo tengo a apple en mi cartera 

(VaR_IBM_Normal = z*volatilidad_IBM*sqrt(h)*W_IBM_)
z*sd(Rend_IBM)*sqrt(5)*W_IBM_
(VaR_IBM_Normal/W_IBM_) #Si solo tuviera iBM en el portafolio, en un horizonte de 5 dias, hay un 1% de probabilidad de que la perdida sea mayor a 766 dolares. Representa un 5.7% del portafolio

(VaR_GOOG_Normal = z*volatilidad_GOOG*sqrt(h)*W_GOOG_)
(VaR_GOOG_Normal/W_GOOG_)

(VaR_P_Normal_NoDiv <- VaR_AAPL_Normal + VaR_IBM_Normal + VaR_GOOG_Normal) #Suponiendo correlacion de 1 --> no hay beneficios de diversificacion
(VaR_P_Normal_NoDiv/W_) #6.2% del portafolio, mitad de camino de los W_A y W_I

(VaR_P_Normal <- z*volat_P*sqrt(h)*W_) #var relativo diversificado
VaR_P_Normal/W_ #Menor que si no hay benef por diversif
(Benef_Diver <- VaR_P_Normal_NoDiv - VaR_P_Normal)

#VaR Marginal---------------------------------------------------------
(Betas <- (mat_varcov %*% w_) / volat_P^2)
sum(t(w_) %*% Betas) #chequeo promedio ponderado tiene que dar 1
(VaR_Marginal_ <- VaR_P_Normal/W_*Betas)

#Component VaR --------------------------
Component_VaR_ <- VaR_P_Normal*w_*Betas
Component_VaR_

(sum(Component_VaR_)-VaR_P_Normal) #chequeo que suma de component var me tiene que dar lo mismo, osea la resta =0 no coincide por 0.00jkJZBk

#%VaR
(Component_VaR_/VaR_P_Normal)
