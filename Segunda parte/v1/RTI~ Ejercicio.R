#RTI: Ejemplo 
library(tidyverse)

#Enfoque Contable
ATF=6000
ATV=5000

PTF=5500
PTV=3500

A_spread=0.04
P_spread=0.01
tasa_referencia = 0.15

tabla=tibble(
  concepto = c("Activos tasa fija", "Activo tasa variable", "Activo total", "Pasivo tasa fija", "Pasivo tasa variable", "Pasivo total"),
  saldos = c(6000, 5000, 6000+5000, 5500, 3500, 3500+5500),
  margen = c(0.04, 0.04, NA, 0.01, 0.01, NA)
)
tabla

#Gap de Tasa

(Gap_TI = ATV - PTV) #Me perjudica una baja en la tasa


#Intereses cobrados

(I_ATF = ATF*(tasa_referencia + A_spread))
(I_ATV = ATV*(tasa_referencia + A_spread))

(I_A = I_ATF + I_ATV)

(I_PTF=PTF*(tasa_referencia + P_spread))
(I_PTV=PTV*(tasa_referencia + P_spread))

(I_P = I_PTF + I_PTV)

(margen=I_A - I_P)

nueva_fila= tibble(
  tasa=c(tasa_referencia+A_spread,tasa_referencia+A_spread, NA, 
         tasa_referencia+P_spread,tasa_referencia+P_spread,NA),
  Intereses=c(I_ATF, I_ATV,I_A,I_PTF,I_PTV, I_P)
)

tabla=add_column(tabla,nueva_fila)
View(tabla)

#Volatilidad 1.5%
c=0.99
z=qnorm(1-c) #Porque me perjudica una baja
volat=0.015

(dTI=volat*z) 

(tasa_referencia_n = tasa_referencia + dTI)

#Obs: solo debo actualizar los datos que dependen de tasa variable 

(I_ATV_n = ATV*(tasa_referencia_n + A_spread))

(I_A_n = I_ATF + I_ATV_n)

(I_PTV_n = PTV*(tasa_referencia_n + P_spread))

(I_P_n = I_PTF + I_PTV_n)

nueva_fila2= tibble(
  tasa_variac=c(tasa_referencia+A_spread,tasa_referencia_n+A_spread, NA, 
                tasa_referencia+P_spread,tasa_referencia_n+P_spread,NA),
  int_variac=c(I_ATF, I_ATV_n,I_A_n,I_PTF,I_PTV_n, I_P_n)
)

tabla=add_column(tabla,nueva_fila2)
View(tabla)

(margen_n = I_A_n - I_P_n) 

(dMargen= margen_n - margen)

(EaR=abs(dMargen))

cat("Por lo tanto lo que se puede perder ante una variación en la tasa de 
    interés es", EaR,"\n")

#Gap de Tasa

(Gap_TI = ATV - PTV)

(dmargen_v2 = Gap_TI * dTI)

if (all(Gap_TI >= 0)) {
  cat("Como el GAP de tasa de interés resulta positivo, entonces una baja en las tasas perjudicará a la entidad.\n")
} else {
  cat("El GAP de tasa de interés no es positivo. Una suba en las tasas perjudicará a la entidad en este caso.\n")
}

#Enfoque Economico --------------------
#Metodo Exacto
#EVaR -->  calcula el posible deterioro en el valor presente de los activos netos debido a movimientos 
#adversos en las tasas de interés

tasa_referencia = 0.15
A_spread=0.04
P_spread=0.01
confianza = 0.99
volatilidad_tasas = 0.015

#Escenario inicial

#Activos
tasa_fija_activo = 0.19   #Tasa de referencia + A_spread
ATF = 6000
ATV = 5000
t = 1:3
#Prestamo a tasa fija 19%, amortiza en cuotas iguales
(capital_atf=c(rep(ATF/3,3)))

(inter_atf=c(ATF*tasa_fija_activo,(ATF-capital_atf[2])*tasa_fija_activo,
             (ATF-capital_atf[2]-capital_atf[3])*tasa_fija_activo))

#Saldos financiados en tarjeta de credito, intereses constantes y se amortiza al final del periodo
(capital_atv=c(0,0,ATV))

(inter_atv=c(rep(ATV*tasa_fija_activo,3)))

(FF_A=t(data.frame(t=c(1,2,3),capital_atf,inter_atf,capital_atv,inter_atv)))

(FD_A = (1 + tasa_fija_activo)^-t)


(CF_A=c(sum(FF_A[, 1])-t[1],
       sum(FF_A[, 2])-t[2],
       sum(FF_A[, 3])-t[3]))

(VA_activo = CF_A*FD_A)

(sum(VA_activo)) #Verificacion es el Valor Actual de ATV + ATF

ATV+ATF

(FF_A = rbind(FF_A, CF_A))

#Pasivos
PTV = 3500
PTF = 5500
tasa_fija_pasivo = 0.16
t = 1:3

#Prestamo a tasa fija

(capital_ptf=c(0,0,PTF))

(inter_ptf=c(rep(PTF*tasa_fija_pasivo,3)))

#Plazo fijo que renegocia la tasa

(capital_ptv=c(0,0,PTV))

(inter_ptv=c(rep(PTV*tasa_fija_pasivo,3)))

(FF_P=t(data.frame(t=c(1,2,3),capital_ptf,inter_ptf,capital_ptv,inter_ptv)))

(FD_P = (1 + tasa_fija_pasivo)^-t)

(CF_P=c(sum(FF_P[, 1])-t[1],
       sum(FF_P[, 2])-t[2],
       sum(FF_P[, 3])-t[3]))

(VA_pasivo = CF_P*FD_P)

(sum(VA_pasivo)) #Verificacion es el Valor Actual de ATV + ATF

PTV+PTF

(FF_P = rbind(FF_P, CF_P))

#Capital
capital=2000

(VA_C=sum(VA_activo)-sum(VA_pasivo))

#Cambio en la tasa-------------------
(dTI)
tasa_referencia_n

#Activos

     #Prestamo a tasa fija 19%, amortiza en cuotas iguales
(capital_atf)

(inter_atf)

     #Saldos financiados en tarjeta de credito, intereses constantes y se amortiza al final del periodo
(capital_atv)

(tasamodif_a=tasa_referencia_n+A_spread)

#Para el primer periodo no va a cambiar, porque ya esta dada desde el mto de emision del resumen
(inter_atv_n=c(ATV*tasa_fija_activo,rep(tasamodif_a*ATV,2)))

(FF_A_n=t(data.frame(t=c(1,2,3),capital_atf,inter_atf,capital_atv,inter_atv_n)))

(FD_A_n = (1 + tasamodif_a)^-t)


(CF_A_n=c(sum(FF_A_n[, 1])-t[1],
       sum(FF_A_n[, 2])-t[2],
       sum(FF_A_n[, 3])-t[3]))

(VA_activo_n = CF_A_n*FD_A_n)

(FF_A_n = rbind(FF_A_n, CF_A_n))

(sum(VA_activo_n)) 

sum(VA_activo_n)-sum(VA_activo)

#Pasivos

    #Prestamo a tasa fija

(capital_ptf)

(inter_ptf)

    #Plazo fijo que renegocia la tasa

(capital_ptv)

(tasamodif_p=tasa_referencia_n+P_spread)

#Sin que se ve afectado el interes del primer peridodo, porque la tasa para ese periodo ya fue fijada.

(inter_ptv_n=c(PTV*tasa_fija_pasivo,rep(tasamodif_p*PTV,2)))

(FF_P_n=t(data.frame(t=c(1,2,3),capital_ptf,inter_ptf,capital_ptv,inter_ptv_n)))

(FD_P_n = (1 + tasamodif_p)^-t)

(CF_P_n=c(sum(FF_P_n[, 1])-t[1],
        sum(FF_P_n[, 2])-t[2],
        sum(FF_P_n[, 3])-t[3]))

(VA_pasivo_n = CF_P_n*FD_P_n)

(FF_P_n = rbind(FF_P_n, CF_P_n))

(sum(VA_pasivo_n)) 

(sum(VA_pasivo_n)-sum(VA_pasivo))

#Capital
capital=2000

(VA_C_n=sum(VA_activo_n)-sum(VA_pasivo_n))


#Economic Value at Risk ~ Metodo Exacto
(EVaR= VA_C_n-capital) 
#La situacion es peor a la inicial
cat("La perdida obtenida con un nivel de confianza del",c,"en 
    terminos de valor economico es",abs(EVaR),"dado la estructura de vencimientos y 
    tasa de interes","\n")

#Obs: si EVar > 0 entonces implica una oportunidad porque el VA_C es mayor al capital
#Es una situacion favorable frente a una modificacion en las tasas. 

#Economic Value at Risk ~ Metodo Aproximado
FF_A

(CF_Atf1=c(sum(FF_A[2, 1],FF_A[3,1])))
(CF_Atv1=c(sum(FF_A[4, 1],FF_A[5,1])))

(CF_Atf2=c(sum(FF_A[2, 2],FF_A[3,2])))
(CF_Atv2=c(sum(FF_A[4, 2],FF_A[5,2])))

(CF_Atf3=c(sum(FF_A[2, 3],FF_A[3,3])))
(CF_Atv3=c(sum(FF_A[4, 3],FF_A[5,3])))


(duration_atf=((CF_Atf1*(1+tasa_fija_activo)^(-t[1]))
              +(CF_Atf2*t[2]*(1+tasa_fija_activo)^(-t[2]))
             +(CF_Atf3*t[3]*(1+tasa_fija_activo)^(-t[3])))/ATF)

(duration_atv=((inter_atv[1]+ATV)*(1+tasa_fija_activo)^(-1))/ATV)

FF_P

(CF_Ptf1=c(sum(FF_P[2, 1],FF_P[3,1])))
(CF_Ptv1=c(sum(FF_P[4, 1],FF_P[5,1])))

(CF_Ptf2=c(sum(FF_P[2, 2],FF_P[3,2])))
(CF_Ptv2=c(sum(FF_P[4, 2],FF_P[5,2])))

(CF_Ptf3=c(sum(FF_P[2, 3],FF_P[3,3])))
(CF_Ptv3=c(sum(FF_P[4, 3],FF_P[5,3])))


(duration_ptf=((CF_Ptf1*(1+tasa_fija_pasivo)^(-t[1]))
               +(CF_Ptf2*t[2]*(1+tasa_fija_pasivo)^(-t[2]))
               +(CF_Ptf3*t[3]*(1+tasa_fija_pasivo)^(-t[3])))/PTF)

(duration_ptv=((inter_ptv[1]+PTV)*(1+tasa_fija_pasivo)^(-1))/PTV)

(duration_activo=duration_atf+duration_atv)

(duration_pasivo=duration_ptf+duration_ptv)

(duration_GAP=(-duration_atf*ATF*1/(1+tasa_fija_activo))
             + (-duration_atv*ATV*1/(1+tasa_fija_activo))
             + (duration_ptf*PTF*1/(1+tasa_fija_pasivo))
            + (duration_ptv*PTV*1/(1+tasa_fija_pasivo)))

if (duration_GAP >= 0) {
  cat("Como el duration GAP resulta positivo, entonces un movimiento
      en las tasas perjudicara a la entidad.\n")
} else {
  cat("El duration GAP no es positivo. Un movimiento en las tasas 
      beneficiará a la entidad.\n")
}

cat("Este metodo contempla todos los periodos.\n")

(EVaR_aprox=duration_GAP*dTI)
abs(EVaR_aprox)
 
EVaR

#Como el GAP TV es positivo, para eliminar el riesgo de tasa tengo que salir a fondear.
Gap_TI
#Si la empresa desea cubrirse ante una baja en las tasas de interes, 
#debera recibir pagos a tasa fija, realizar pagos a tasa variable. 
#De tal forma que al fijar la tasa se protege frente a la disminucion en la tasa de interes y 
#y compensara la variabilidad en sus egresos, con ingresos a tasa fija.  

#Si desea cubrirse frente a una suba en la tasa de interes, acordara recibir 
#pagos a en una tasa variable (tal como la tasa de referencia del mercado) y 
#realizar pagos a una tasa fija prefijada en el swap.
#Como recibe pagos a tasa variable, se beneficia si se produce un aumento. 
#Y los pagos fijos actuan como una proteccion contra el aumento de los costos de financiamineto.

#Determine si existe riesgo de liquidez. Que consideraciones
#debe tenerse desde la gestion al administrar el riesgo de liquidez a traves de GAPs.
Activos=c(ATV+ATF,ATV+ATF-2000,ATV+ATF-4000,0)
A_Amortizacion=c(0, -diff(Activos))

Pasivos=c(PTV+PTF,PTV+PTF,PTV+PTF,0)
P_Amortizacion=c(0, -diff(Pasivos))

Capital=c(rep(2000,4))

Gap= Activos - Pasivos- Capital

(GapMarginal=c(0, diff(Gap)))

(GapMgnal=A_Amortizacion - P_Amortizacion)

(gestion=tibble(t=c(0,1,2,3),Activos,A_Amortizacion,Pasivos,P_Amortizacion,
               Gap,GapMarginal))

cat("El GAP resulta negativo en todos los periodos",Gap)
cat("Por lo tanto no hay riesgo de liquidez y los fondos son ociosos")
