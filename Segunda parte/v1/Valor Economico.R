#RTI:Ejemplo ~Enfoque valor economico 
#eVaR----------
#Calculo del capital econ0omico por riesgo de tasa  de interes, es decir como cambia el valor economico de los activos y pasivos, 
#en consecuenciadel capital de un banco ante cambios en las tasas de interes.

library(tidyverse)
balance=tibble(
  concepto=c("Activo","Pasivo","Capital"),
  Valor_mercado=c(1000,900,100),
  Contable=c(1000,900,100)
)
balance  #Al mto inicial los valores coinciden

#En este ejercicio se supone que el activo esta compuesto por prestamos a tasa fija 10% 
#n=5 y un  Plazo fijo repactable cada periodo con i= 9%

   #Prestamo a tasa fija (ACTIVO)
t=1:5
tasa_A=0.10
(CF_Activo=balance$Contable[1]*c(rep(tasa_A,4),(1+tasa_A))) #Cash Flow (flujo de efectivo)
#Donde durante 4 periodos es solo intereses
#En el ultimo capital + intereses
(FD_A=(1+tasa_A)^-t) #Factores de dto
(VA_A=CF_Activo*FD_A)
sum(VA_A) #Chequeo

activo=tibble(t,CF_Activo,FD_A,VA_A)
colnames(activo) <- c("Periodo", "Cash Flow A", "Factor Dto A", "Valor Actual A")
activo

   #Plazo fijo Anual Repactable, tasa=9% (Pasivo)
tasa_P=0.09
(CF_Pasivo=balance$Contable[2]*c(rep(tasa_P,4),(1+tasa_P))) #Flujos de efectivo esperados
(FD_P=(1+tasa_P)^-t)
(VA_P=CF_Pasivo*FD_P)
sum(VA_P)

pasivo=tibble(t,CF_Pasivo,FD_P,VA_P)
colnames(pasivo) <- c("Periodo", "Cash Flow P", "Factor Dto P", "Valor Actual P")
pasivo

 #Capital (Accionistas)
(CF_Capital=CF_Activo-CF_Pasivo)
(VA_Capital=VA_A-VA_P)
sum(VA_Capital)

capital=tibble(t,CF_Capital,VA_Capital)
colnames(capital) <- c("Periodo", "Cash Flow C", "Valor Actual C")
capital

balance_final=tibble(t,CF_Activo,FD_A,VA_A,CF_Pasivo,FD_P,VA_P,CF_Capital,VA_Capital)
colnames(balance_final) <- c("Periodo","Cash Flow A", "Factor Dto A", "Valor Actual A","Cash Flow P", "Factor Dto P", "Valor Actual P", "Cash Flow C", "Valor Actual C")
View(balance_final)

#Como impacta en el balance la suba  de 200 bps en las tasas de interes--------------
    #Si sube~Escenario suba 200 bps 
tasa_A_s=0.12

    #Prestamo a tasa fija (ACTIVO)
(CF_Activo_s=balance$Contable[1]*c(rep(tasa_A,4),(1+tasa_A))) #No se modifica, son activos a tasa fija
(FD_A_s=(1+tasa_A_s)^-t) 
(VA_A_s=CF_Activo_s*FD_A_s) 
suma=sum(VA_A_s) 

cat("Conclusion:\n")
cat("Se ve reducido el Valor Actual ya que se descuenta al", tasa_A_s,"en comparacion con", tasa_A,"\n")
cat("Por lo tanto, el prestamo va a tener un valor economico bajo la par de",suma,  "\n")
cat("La suba de 200 bps en la tasa de interes hace que el activo caiga desde",balance$Valor_mercado[1], "hasta",suma,"\n")

    #Plazo fijo Anual Repactable, tasa=9% (Pasivo)
tasa_P_s=0.11
(CF_Pasivo_s=balance$Contable[2]*c(tasa_P,rep(tasa_P_s,3),(1+tasa_P_s))) 
#Flujos de efectivo se modifican a partir del segundo periodo en adelante

(FD_P_s=(1+tasa_P_s)^-t)
(VA_P_s=CF_Pasivo_s*FD_P_s)
sum(VA_P_s)

#Obs: la caida proporcional del pasivo es mucho menor en relacion con el activo
#Ya que se actualizan a la nueva tasa los flujos siguientes. 

#Capital (Accionistas)
(CF_Capital_s=CF_Activo_s-CF_Pasivo_s)
(VA_Capital_s=VA_A_s-VA_P_s)
sum(VA_Capital_s)

cat("Conclusion:\n")
cat("Una suba del", tasa_A_s-tasa_A, "en las tasas de intereses de los activos y pasivos
    tiene un impacto en el valor economico de", balance$Contable[3]-sum(VA_Capital_s),"\n" )

#Contablemente es el mismo, pero el valor economico cae.
#El activo cae  mucho mas que el pasivo, porque el pasivo es a tasa variable. 
#Al repactar la tasa el valor economico no sufre tanto ante movimientos en la tasa de interes
#El activo como es a tasa fija, el movimiento en la tasa de interes tiene un fuerte impacto en el valor economico
#Como se deteriora el valor del activo, y el pasivo no cae tanto, impacta fuertemente en el valor economico.

