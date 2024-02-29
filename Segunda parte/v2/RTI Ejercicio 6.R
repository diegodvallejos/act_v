#Resuelto x ayudante Lara 

#Activos
tasa_fija_activo <- 0.19
ATF <- 6000
ATV <- 5000
A<- ATF+ATV

#Pasivos
PTV <- 3500 #plazo fijo
PTF <- 5500 #prestamo
P<- PTV+PTF
tasa_fija_pasivo <- 0.16

tasa_referencia <- 0.15
spread_activo <- 0.04
spread_pasivo <- 0.01
confianza <- 0.99
volatilidad_tasas <- 0.015
capital <- 2000

#a) EaR <- valor absoluto(GAP_TV*variacion tasas) 

Zc <- qnorm(confianza)

Gap_TV <- ATV-PTV
variacion_tasas <- Zc*volatilidad_tasas

EaR <- abs(Gap_TV*variacion_tasas)
EaR

#b) Aproximacion EVaR aproximado

#EVaR_aproximado = valor absoluto (variacion tasas*DurGap)

#DurGap = -Dur_m_ATV * ATV - Dur_m_ATF * ATF + Dur_m_PTV * PTV + Dur_m_PTF * PTF
#Todas las Dur... son las duartion modificadas
#Duration modificada (casos variables) = DUR_V / (1+i) ej Dur_m_ATV= DUR_ATV / (1+i_A)

Dur_ATV <- 1
Dur_PTV <- 1 #las duration de los variables es siempre 1

#i_A para los variables es i_A=tasa_referencia + spread_activo #siiii lo hice asi y me da!!!
i_ATV <- tasa_referencia+spread_activo
i_PTV <- tasa_referencia+spread_pasivo

Dur_m_ATV <- Dur_ATV / (1+i_ATV)
Dur_m_PTV <- Dur_PTV / (1+i_PTV)

#Para los casos TF la duration se calcula "mas dificil"
#Dur_TF = sumatoria (t*FF_t*FD_t)/ TF --> necesito hacer un FF
t = 0:3 #va a dar lo mismo
FD_A<- (1 + tasa_fija_activo)^-t
FD_P <- (1+tasa_fija_pasivo)^-t

#FF_t = Amort+Int

Amortizacion_ATF <- c(0,ATF/3,ATF/3,ATF/3)
Int_ATF <- c(0,ATF*tasa_fija_activo, (ATF-ATF/3)*tasa_fija_activo, (ATF-2*ATF/3)*tasa_fija_activo )

Dur_ATF <- sum((t*(Amortizacion_ATF+Int_ATF)*FD_A)/ATF)

Dur_m_ATF <- Dur_ATF/(1+tasa_fija_activo)


Amortizacion_PTF <- c(0,0,0,5500)
Int_PTF <- c(0, PTF*tasa_fija_pasivo, PTF*tasa_fija_pasivo, PTF*tasa_fija_pasivo)

Dur_PTF <- sum((t*(Amortizacion_PTF+Int_PTF)*FD_P)/PTF)

Dur_m_PTF <- Dur_PTF / (1+tasa_fija_pasivo)

Dur_Gap <- -Dur_m_ATV*ATV-Dur_m_ATF*ATF+Dur_m_PTF*PTF+Dur_m_PTV*PTV
Dur_Gap

EVaR_aproximado <- abs(variacion_tasas*Dur_Gap)
EVaR_aproximado

#punto c)
#Riesgo: baja de tasa de interes
#Cobertura:Swap con intercambio de tasa variable por tasa fija (es decir, pago tasa variable y recibo tasa fija)
#Condiciones:
#  Periodicidad = anual
#  VN= Gap_TV
#  TV*TF
#En el caso que una empresa busca cubrirse contra una baja en las tasas de interes, deberan recibir pagos basados en una tasa fija y realizar pagos basados en atsa variable.
#La tasa fija actuara como una proteccion contra el riesgo de una disminucion en las tasas de interes.
#Al recibir pagos basados en la tasa fija, la empresa compensara cualquier reduccion en los ingresos generados por sus activos o pasivosque estan vinculados a tasa varible.

#En el caso que la empresa busca cubrirse contra una suba en la tasa de interes, la empresa acordara recibir pagos basados en una tasa variable (como la tasa de referencia del mercado, como la tasa LIBOR) y realizar pagos basados en una tasa fija acordada en el swap.
#Al recibir pagos basados en la tasa variable, la empresa se beneficiara de un aumento en las tasas de interes, ya que sus ingresos generados por los activos o pasivos vinculados a las tasas variablesaumentaran.
#Al mismo tiempo, los pagos fijos realizados por la empresa actuaran como una proteccion contra el aumento de los costos de financiamineto.


#punto d) 
#FF= Capital - Amortizacion (NO TIENE EN CUENTA LOS INTERESES)
# en este caso en los FF devuelve todo el capital al final salvo en ATF que es un caso de Sistema de Prestamos Aleman

#los flujos van desde el momento 0 hasta el momento 3 

#ATV: devuelve al final
Capital_ATV <- c(5000,5000,5000,0)
Amortizacion_ATV <- c(0,0,0,5000)
Amortizacion_ATV_2 <- c(0,-diff(Capital_ATV))
FF_ATV <- Capital_ATV - Amortizacion_ATV

#ATF: sistema aleman (cuotas de amort constantes) Americano amortiza todo al final y Frances tiene amortizacion creciente (paga siempre lo mismo, cada vez mas amort y menos intereses)
cant_periodos <- 3
Amort_ATF <- ATF/cant_periodos
Capital_ATF <- c(ATF,ATF-Amort_ATF,ATF-2*Amort_ATF,ATF -3*Amort_ATF) # c(6000,4000,2000,0)
Amortizacion_ATF <- c(0,Amort_ATF,Amort_ATF,Amort_ATF) 
Amortizacion_ATF_2 <- c(0,-diff(Capital_ATF)) #siiii va
FF_ATF <- Capital_ATF - Amortizacion_ATF


#si fuese un sistema americano --> Capital (ATF, ATF, ATF, 0) Amort (0,0,0,ATF)
#si fuese un sistema frances: a chequear
ATF
cant_periodos
tasa_fija_activo

Cuota_f <- ATF * (tasa_fija_activo * (1 + tasa_fija_activo)^cant_periodos) / ((1 + tasa_fija_activo)^cant_periodos- 1)

Int_P1 <- ATF*tasa_fija_activo
Amort_1 <- Cuota_f-Int_P1

Int_P2<- (ATF-Amort_1)*tasa_fija_activo
Amort_2 <- Cuota_f-Int_P2

Int_P3<- (ATF-Amort_1-Amort_2)*tasa_fija_activo
Amort_3<- Cuota_f-Int_P3

Capital_ATF_f <- c(ATF, ATF-Amort_1, ATF-Amort_1-Amort_2,0)
Amortizacion_ATF_f <- c(0,Amort_1, Amort_2, Amort_3)

VA_amort <- Amortizacion_ATF_f*FD_A
(sum(VA_amort))#NO ME DA, CHEQUEAR


#PTV: devuelve al final
Capital_PTV <- c(3500,3500,3500,0)
Amortizacion_PTV <- c(0,0,0,3500) 
Amortizacion_PTV_2 <- c(0, -diff(Capital_PTV)) #sii
FF_PTV <- Capital_PTV - Amortizacion_PTV

#PTF: devuelve todo al final
Capital_PTF <- c(5500,5500,5500,0)
Amortizacion_PTF <- c(0,0,0,5500)
Amortizacion_PTF_2 <- c(0, -diff(Capital_PTF)) #siii
FF_PTF <- Capital_PTF - Amortizacion_PTF

#tener en cuenta el capital propio
capital <- 2000
C <- c(capital, capital, capital, capital)

#Gap= A-(P+C) (solo la parte de capital)
Gap <- Capital_ATF+Capital_ATV-Capital_PTV-Capital_PTF-C
Gap

Gap_Marginal <- c(0, diff(Gap))
Gap_Marginal

#El Gap es <0 en todos los periodos, por lo tanto no hay riesgo de liquidez, hay fondos ociosos.
#El Gap marginal indica cuanto tendra considerando que todos los Gaps anteriores se cerraron.
#en t=3 si hay riesgo de liquidez y habria que tomar fondos
 