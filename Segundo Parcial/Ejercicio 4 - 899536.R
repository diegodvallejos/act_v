#EJERCICIO 4 - CAROLA BRUNO - 899323

ATF<- 700
ATV<- 300
A<- ATF+ATV
i_A <- 0.08

PTF<- 800
PTV<- 100
P<- PTF+PTV
i_P <- 0.07

capital <- 100

#a) FLUJOS DE FONDOS 
t=0:4
FD_A <- (1 + i_A) ^-t
FD_P <- (1 + i_P) ^-t

  #ATF- prestamos a 4 años (supongo que se amortiza todo al final)
Int_ATF <- c(0,rep(ATF*i_A,4))
Amort_ATF <- c(0,0,0,0,ATF)
FF_ATF <- Int_ATF+Amort_ATF
FF_ATF

VA_FF_ATF <- FF_ATF*FD_A
VA_FF_ATF

sum(VA_FF_ATF) #me da = ATF

  #ATV - supongo que amortiza todo al final
FF_ATV <- ATV*c(0,rep(i_A,3), (1+i_A))
FF_ATV

VA_FF_ATV <- FF_ATV*FD_A
VA_FF_ATV

sum(VA_FF_ATV)

  #PTF - supongo que devuelve al final
FF_PTF <- PTF*c(0,rep(i_P,3), (1+i_P))
FF_PTF

VA_FF_PTF <- FF_PTF*FD_P
VA_FF_PTF

sum(VA_FF_PTF)

  #PTV - supongo que devuelve todo al final
FF_PTV <- PTV*c(0,rep(i_P,3), (1+i_P))
FF_PTV

VA_FF_PTV <- FF_PTV*FD_P
VA_FF_PTV

sum(VA_FF_PTV) 


  #Capital
FF_Cap <- FF_ATF+FF_ATV-FF_PTF-FF_PTV
FF_Cap

VA_Cap <- VA_FF_ATF+VA_FF_ATV-VA_FF_PTF-VA_FF_PTV 
VA_Cap

#B)como el Gap TI es >0 un aumento en la tasa nos beneficia.
# El valor contable no se modifica

#c) sube un 1%

bps <- 0.01
i_A_n <- i_A +bps
i_P_n <- i_P+bps

FD_A_n <- (1 + i_A_n) ^-t
FD_P_n <- (1 + i_P_n) ^-t

#ATF
FF_ATF_n <- FF_ATF
FF_ATF_n

VA_FF_ATF_n <- FF_ATF_n*FD_A_n
VA_FF_ATF_n

#ATV

FF_ATV_n <- ATV*c(0,i_A, rep(i_A_n,2), (1+i_A_n))
FF_ATV_n

VA_FF_ATV_n <- FF_ATV_n*FD_A_n
VA_FF_ATV_n

#PTF
FF_PTF_n <- FF_PTF #porque es tasa fijada de antes
FF_PTF_n

VA_FF_PTF_n <- FF_PTF_n*FD_P_n
VA_FF_PTF_n

#PTV
FF_PTV_n <- PTV*c(0,i_P, rep(i_P_n,2), (1+i_P_n))
FF_PTV_n

VA_FF_PTV_n <- FF_PTV_n*FD_P_n
VA_FF_PTV_n 


#EVaR exacto no llegue a calcularlo por tiempo