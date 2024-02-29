#Ejercicio diapo 27: Enfoque Contable=EaR

#VER QUE ONDA EL SPREAD

#Activo
ATF<- 700
ATV<- 300
A<- ATF+ATV
tasa_referencia_A<-0.12

#intereses ganados
IATF<- ATF*tasa_referencia_A
IATV<- ATV*tasa_referencia_A

I_A <- IATF+IATV

#Riesgo tasa: si sube 1% tasa
spread<- 0.01
tasa_referencia_A_n <-tasa_referencia_A+spread #solo se usa para ATV

IATF_n <- IATF #no cambia porque es tasa fija
IATV_n <- ATV*(tasa_referencia_A_n)

I_A_n <- IATF_n + IATV_n
I_A_n

Variacion_I_A <- I_A_n - I_A
Variacion_I_A 

#Pasivo
PTF<- 800
PTV<- 200
P<- PTF+PTV
tasa_referencia_P <- 0.06

#intereses pagados
IPTF <- PTF *tasa_referencia_P
IPTV <- PTV *tasa_referencia_P

I_P <- IPTF+IPTV
I_P

#Riesgo tasa:si sube 1% tasa
spread <- 0.01
tasa_referencia_P_n <- tasa_referencia_P+spread #solo se usa para PTV

IPTF_n <- IPTF
IPTV_n <- PTV*tasa_referencia_P_n

I_P_n <- IPTF_n+IPTV_n
I_P_n

Variacion_I_P<- I_P_n-I_P
Variacion_I_P

#Comparacion total 
(Gap_Tasa <- ATV-PTV)

Margen<- I_A-I_P #margen por intereses
Margen

Margen_n<- I_A_n-I_P_n
Margen_n

Variacion_tot <- Variacion_I_A-Variacion_I_P
Variacion_tot

difMargen<- Margen_n-Margen
difMargen

Variacion_tot==difMargen



