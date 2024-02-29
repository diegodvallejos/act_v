#EJERCICIO 2 RTI - EVAR EXACTO LARA

#Activos
tasa_fija_activo <- 0.19
ATF <- 6000
ATV <- 5000 #tarjeta de credito en general es a tasa variable porque deuda de la tarjeta se la financia con la tasa en ese momento 
A<- ATF+ATV

#Pasivos
PTV <- 3500 #plazo fijo: es fijo para el primer perido pero variable para los periodos que siguen
PTF <- 5500 #prestamo
P<- PTV+PTF
tasa_fija_pasivo <- 0.16

PN <- 2000

tasa_referencia <- 0.15
spread_activo <- 0.04
spread_pasivo <- 0.01
confianza <- 0.99
volatilidad_tasas <- 0.015

#MOMENTO INICIAL - importa la exposicion de los capitales

#EaR (cuanto es lo que podemos perder, se mide en valor absoluto por lo general) - Enfoque contable--------------------------------------
#hacemos foto del balance

#Primero calcular Gap TV
Gap_TV <- ATV-PTV
Gap_TV #perjudica una baja en la tasa. 

#variacion en la tasa
if (Gap_TV > 0){
  zc <- qnorm(1-confianza)
}else{ zc <- qnorm(confianza)
}
zc <- qnorm(confianza)
variacion_tasa <- zc*volatilidad_tasas #lo peor que me puede pasar es que baje 3.49%

EaR<- abs(Gap_TV*variacion_tasa) #lo peor que me puede pasar es perder 52.4...

#Economic Value at Risk - agarramos lo que tenemos para calcular los flujos futuros---------------------------------------
#Hacemos flujo de fondos

#Activos:
tasa_activo<- tasa_referencia+spread_activo
#--> ATF
capital_ATF <- c(rep(ATF/3, 3))
interes_ATF <- c(tasa_activo*ATF,tasa_activo*(ATF-capital_ATF[1]), tasa_activo*(capital_ATF[1]+capital_ATF[2]))

FF_ATF <-  c(sum(capital_ATF[1]+interes_ATF[1]), sum(capital_ATF[2]+interes_ATF[2]), sum(capital_ATF[3]+interes_ATF[3]))

#--> ATV #se mantiene cosntante y amortiza al final=se cobra el capital en el tercer periodo 
capital_ATV <- c(0,0,5000)
interes_ATV <- c(rep(tasa_activo*ATV,3))

#FF Activo: todo lo que gano por capital y por intereses
FF_A <- c(sum(capital_ATF[1]+interes_ATF[1]+capital_ATV[1]+interes_ATV[1]), sum(capital_ATF[2]+interes_ATF[2]+capital_ATV[2]+interes_ATV[2]), sum(capital_ATF[3]+interes_ATF[3]+capital_ATV[3]+interes_ATV[3]))

#Pasivos
tasa_pasivo <- tasa_referencia+spread_pasivo
#--> PTF
capital_PTF <- c(0,0,5500)
interes_PTF <- c(rep(tasa_pasivo*PTF,3))

#--> PTV
capital_PTV <- c(0,0,3500)
interes_PTV <- c(rep(tasa_pasivo*PTV,3))

#FF Pasivo
FF_P <- c(sum(capital_PTF[1]+interes_PTF[1]+capital_PTV[1]+interes_PTV[1]), sum(capital_PTF[2]+interes_PTF[2]+capital_PTV[2]+interes_PTV[2]), sum(capital_PTF[3]+interes_PTF[3]+capital_PTV[3]+interes_PTV[3]))

#ahora calculo el valor actual de los fondos
t<- 1:3
FD_A <- (1+tasa_activo)^-t
FD_P <- (1+tasa_pasivo)^-t

VA_FF_A <- sum(FF_A*FD_A)
VA_FF_P <- sum(FF_P*FD_P)

VA_FF_PN <- VA_FF_A-VA_FF_P

#EVaR de manera exacta: ver que pasa si cambian las tasas
#la tasa baja en lo peor que podria bajar
tasa_referencia_n <- tasa_referencia+variacion_tasa
tasa_activo_n  <- tasa_referencia_n+spread_activo # solo ATV
tasa_pasivo_n  <- tasa_referencia_n+spread_pasivo # solo PTV

#FF de ATF sisigue teniendo tasa 19% y FF ATV cambia tasa
#FF PTF no cambia el interes,y PTV si cambia la tasa

#PF ya tenia fijado el interes, por mas que se vea afectado el flujo de tasa de interes, el interes del primer periodo no se tendria que ver afectado porque pf ya esta consituido.
#para la tarjeta pasa algo parecido. la tasa del primer periodo mantiene tasa vieja

#FF TV bajo el escenario de baja
intereses_ATV_n <- c(tasa_activo*ATV,rep(tasa_activo_n*ATV,2))
intereses_PTV_n <- c(tasa_pasivo*PTV,rep(tasa_pasivo_n*PTV,2))

#--> ATF
capital_ATF <- c(2000,2000,2000)
interes_ATF <- c(tasa_activo*ATF,tasa_activo*(ATF-2000), tasa_activo*(ATF-4000))

#--> ATV #se mantiene cosntante y amortiza al final=se cobra el capital en el tercer periodo 
capital_ATV <- c(0,0,5000)
interes_ATV_n <- c(tasa_activo*ATV,rep(tasa_activo_n*ATV,2))

#-->FF Activo: todo lo que gano por capital y por intereses
FF_A_n <- c(sum(capital_ATF[1]+interes_ATF[1]+capital_ATV[1]+interes_ATV_n[1]), sum(capital_ATF[2]+interes_ATF[2]+capital_ATV[2]+interes_ATV_n[2]), sum(capital_ATF[3]+interes_ATF[3]+capital_ATV[3]+interes_ATV_n[3]))

#Pasivos

#--> PTF
capital_PTF <- c(0,0,5500)
interes_PTF <- c(rep(tasa_pasivo*PTF,3))

#--> PTV
capital_PTV <- c(0,0,3500)
interes_PTV_n <- c(tasa_pasivo*PTV,rep(tasa_pasivo_n*PTV,2))

#-->FF 
FF_P_n <- c(sum(capital_PTF[1]+interes_PTF[1]+capital_PTV[1]+interes_PTV_n[1]), sum(capital_PTF[2]+interes_PTF[2]+capital_PTV[2]+interes_PTV_n[2]), sum(capital_PTF[3]+interes_PTF[3]+capital_PTV[3]+interes_PTV_n[3]))

#los FD cambian desde el primer periodo
FD_A_n <- (1+tasa_activo_n)^-t
FD_P_n <- (1+tasa_pasivo_n)^-t

VA_FF_A_n <- sum(FF_A_n*FD_A_n)
VA_FF_P_n <- sum(FF_P_n*FD_P_n)

VA_FF_PN_n <- VA_FF_A_n-VA_FF_P_n

#baja en la tasa perjudico

#EVaR es la diferencia entre el nuevo VA del patrimonio y el viejo VA patrimonio. ese valor es la perdida que yo puedo tener con un 99% de confianza dado mi estructura de vencimiento y de tasa de interes. el valor tiene que ser absoluto.
#si habia una suba de tasa, el EVaR va a ser positivo, entonces no voy a ver una situacion mala. representa un economic value pero no hay riesgo porque resulta favorable. el que preocupa es el escenario de baja. 

EVaR_exacto <- abs(VA_FF_PN_n-VA_FF_PN)
EVaR_exacto

