capital <- 2000

#Activos
tasa_fija_activo <- 0.19
ATF <- 6000
ATV <- 5000

#Pasivos
PTV <- 3500
PTF <- 5500
tasa_fija_pasivo <- 0.16

tasa_referencia <- 0.15
spread_activo <- 0.04
spread_pasivo <- 0.01
confianza <- 0.99
volatilidad_tasas <- 0.015

# a) Calcular EaR para la foto de balance de hoy -------------------------------

 #Intereses ganados
I_ATF = ATF*(tasa_referencia + spread_activo)
I_ATV = ATV*(tasa_referencia + spread_activo)

I_Activo <- I_ATF + I_ATV

 #Intereses pagados
I_PTF = PTF*(tasa_referencia + spread_pasivo)
I_PTV = PTV*(tasa_referencia + spread_pasivo)

I_Pasivo <- I_PTF + I_PTV

 #Margen por intereses
Margen = I_Activo - I_Pasivo 


#Ahora considero la volatilidad

tasa_referencia_nueva <- tasa_referencia + volatilidad_tasas

 #Intereses ganados escenario nuevo
I_ATV_Nuevo <- ATV*(tasa_referencia_nueva + spread_activo)

 #Intereses pagados escenario viejo
I_PTV_Nuevo <- PTV*(tasa_referencia_nueva + spread_pasivo)

I_Activo_Nuevo <- I_ATF + I_ATV_Nuevo
I_Pasivo_Nuevo <- I_PTF + I_PTV_Nuevo

Nuevo_margen <- I_Activo_Nuevo - I_Pasivo_Nuevo

 #Diferencia de margenes
(Dif_margenes <- Nuevo_margen - Margen) # <- Diferencia de Gap tasas de interes


#-------------------------------#Gap de tasas#---------------------------------#
(Gap_TI <- ATV - PTV)

(dmargen_v2 <- Gap_TI*volatilidad_tasas)

calculo_dif_margenes <- function(ATV, PTV, dTI){
  "Calcula diferencia de margenes.
  
  Parametros
  ----------
  ATV : int
    Activo a tasa variable
  PTV : int
    Pasivo a tasa variable
  dTI : float
    Volatilidad de tasas o diferencia de tasas
  
  Return
  ------
  calculo : float
    Diferencia de margenes entre ambos escenarios de calculo de GAP de liquidez"
  calculo_intermedio = ATV - PTV
  calculo = calculo_intermedio*dTI
  
  return(calculo)
}

verificacion <- calculo_dif_margenes(ATV, PTV, volatilidad_tasas)


#Enfoque economico -------------------------------------------------------------
 #Escenario inicial

  #Activo: prestamo tasa fija por 3 periodos
t = 1:3

Cash_flow_Activo <- ATF * c(rep(tasa_fija_activo, 2), (1 + tasa_fija_activo))
Cash_flow_Activo

FD_Activo <- (1 + tasa_fija_activo)^-t

VA_activo <- Cash_flow_Activo * FD_Activo

(sum(VA_activo)) #Verificacion

 #Pasivo: Prestamo tasa fija 

