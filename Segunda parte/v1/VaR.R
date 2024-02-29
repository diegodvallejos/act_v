library(tidyverse)
library(quantmod) #Permite obtener datos de precios historicos de Yahoo Finance!

rm(list = ls())
graphics.off()

#Obtener datos de precios de acciones utilizando el simbolo de ticker de Yahoo Finance!
ticker1="AAPL"
ticker2="IBM"
desde="2023-01-01"  #Respetar el formato
hasta="2023-06-30"
getSymbols(c(ticker1,ticker2), from = desde , to= hasta)

#Crear un conjunto de datos con los precios de cierre
#Quiero obtener una misma matriz con ambos datos, para eso chequeo que el indice (fecha) coincida

sum(!(index(ticker1)==index(ticker2))) #Chequear que las fechas coincidan, ! indica negacion
#Si la suma = 0, coinciden las fechas

stock_data = tibble(date=index(AAPL),
                    AAPL=as.double(AAPL$AAPL.Close),
                    IBM=as.double(IBM$IBM.Close))
View(stock_data)

#Rendimientos---------------
R_AAPL=log(stock_data$AAPL[2:nrow((stock_data))]/stock_data$AAPL[1:(nrow(stock_data)-1)])
(sigma_AAPL=sd(R_AAPL)*sqrt(252)) #Bajo supuesto de dias habiles 252, anualizo los datos
R_IBM=log(stock_data$IBM[2:nrow((stock_data))]/stock_data$IBM[1:(nrow(stock_data)-1)])
(sigma_IBM=sd(R_IBM)*sqrt(252))
stock_data=stock_data%>%
  mutate(R_AAPL=c(NA,R_AAPL),R_IBM=c(NA,R_IBM))

(Rho=cor(R_AAPL,R_IBM))

(matriz_correlaciones=matrix(c(1,Rho,Rho,1),nrow=2))

(matriz_varcov=matrix(c(sigma_AAPL^2,sigma_AAPL*sigma_IBM*Rho,sigma_AAPL*sigma_IBM*Rho
                 ,sigma_IBM^2),nrow=2))


#Grafico--------------
   #Grafico Precios
ggplot(stock_data, aes(x=date))+
  geom_line(aes(y=AAPL, color = "AAPL"))+ 
  geom_line(aes(y=IBM, color = "IBM"))+ 
  scale_color_manual(values = c("AAPL" = "lightblue", "IBM"="pink"))+
  labs(title = "Precios de cierre de acciones",
       x = "Fecha",
       y= "Precio de cierre",
       color = "Acciones")+ 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 90))

  #Grafico Retornos
ggplot(stock_data, aes(x=date))+
  geom_line(aes(y=R_AAPL, color = "AAPL"))+ 
  geom_line(aes(y=R_IBM, color = "IBM"))+ 
  scale_color_manual(values = c("AAPL" = "lightblue", "IBM"="pink"))+
  labs(title = "Rendimiento Diario",
       x = "Fecha",
       y= "Precio de cierre",
       color = "Acciones")+ 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 90))

#Analisis de VaR: cartera con 70 acciones APPL y 100 de IBM----------
c=0.99 #nivel de confianza
z=qnorm(c) #Supongo distrib normal
h=5/252 #horizonte temporal: 5 dias/ 252 habiles

   ##Valuacion----------

W_A=70*stock_data$AAPL[nrow(stock_data)] #Ultimo elemento
W_A #Precio en USD por tener 70 acciones
W_I=100*stock_data$IBM[nrow(stock_data)]
W_I #Precio en USD por tener 100 acciones

(W=W_A+W_I)
(w=c(W_A,W_I)/W) #Observo el pasado en "proporciones", la riqueza que % representa

#En este caso AAPL representa 49% (es el peso relativo del valor total)
#Para IBM representa el 50%

(sigma_p=sqrt(t(w)%*%matriz_varcov%*%w)%>%as.double())

#Verificando con la formula para una cartera con dos activos:
(sqrt(w[1]^2*sigma_AAPL^2+w[2]^2*sigma_IBM^2+2*w[1]*w[2]*sigma_AAPL*sigma_IBM*Rho))

#Ambos deben dar igual!

    ###Enfoque NO PARAMETRICO ----------------
#Calcular con datos historicos. Donde es necesario definir un horizonte temporal.
#Bajo el supuesto que tomo los ultimos seis meses

stock_data=stock_data%>%
  mutate(P=w[1]*stock_data$AAPL+w[2]*stock_data$IBM) #Con sus respectivas proporciones

#Nueva columna con el precio "proporcionado" entre ambas acciones

View(stock_data)

#Rendimiento del portafolio
R_P=log(stock_data$P[2:nrow(stock_data)]/stock_data$P[1:(nrow(stock_data)-1)])
R_P #retorno logaritmico de c/u de los dias

hist(R_P, main = "Histograma Rend", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")


#Calculo el percentil del retorno del portafolio 
quantile(R_P,1-c) #Mis retornos diarios tienen una prob de 1% de tener una perdida MAYOR al 1,887%

#VaR ABSOLUTO ~ riqueza que tengo hoy
VaR_NoParam_1d_ABS=W*-quantile(R_P,1-c)
VaR_NoParam_1d_ABS #VaR estimado de mi portafolio con un horizonte = 1 dia

#Si quiero un horizonte a semanal, se me superponen los precios de los dias intermedios
#Es decir si miro de miercoles a miercoles, existe un overlapping de mi primer rendimiento 
#y mi segundo rendimiento. No son independientes, porque el periodo de tiempo se repite.
#Hay una superposicion temporal que hace que la muestra no sea independiente

plot(R_P, type = "l", xlab = "Frec", ylab = "Rend", main = "Rendimiento del Portafolio")
abline(h = 0, col = "red", lty = 2) 

#VaR RELATIVO~Riqueza que espero tener
VaR_NoParam_1d_REL=W*-quantile(R_P,1-c)
VaR_NoParam_1d_REL=(mean(R_P)-quantile(R_P,1-c)*W)
VaR_NoParam_1d_REL


   ### Enforque PARAMETRICO---------

#Supongo distribucion lognormal
#Debo estimar la media y la volatilidad

#Observacion: VaR ABS aparece media sumando, VaR REL es respecto a la media. 

#APPL~VaR REL
(VaR_A_Normal=z*sigma_AAPL*sqrt(h)*W_A)

#Verificando 
(z*sd(R_AAPL)*sqrt(5)*W_A)

(VaR_A_Normal/W_A)

#Conclusion: si solo poseo AAPL hay un 1% de prob. de que la perdida en un
#horizonte de cinco dias, sea superior a 892 USD. Lo cual representa un 0.067 del valor del activo

ticker1="AAPL"
ticker2="IBM"
conclusion = sprintf("Si solo poseo %s, hay un 1%% de probabilidad 
                   de que la pérdida en un horizonte de %s días sea superior 
                   a %s USD. Esto representa un %s%% del valor del portafolio.", 
                      ticker1, as.character(h * 252), as.character(VaR_A_Normal), as.character(VaR_A_Normal / W_A))
conclusion

#IBM~VaR REL
(VaR_I_Normal=z*sigma_IBM*sqrt(h)*W_I)

#Verificando 
(z*sd(R_IBM)*sqrt(5)*W_I)

(VaR_I_Normal/W_I)


ticker1="AAPL"
ticker2="IBM"
conclusion = sprintf("Si solo poseo %s, hay un 1%% de probabilidad 
                   de que la pérdida en un horizonte de %s días sea superior 
                   a %s USD. Esto representa un %s%% del valor del portafolio.", 
                     ticker2, as.character(h * 252), as.character(VaR_I_Normal), as.character(VaR_I_Normal / W_I))
conclusion

#VaR~Portafolio
   #No diversificado, con correlacion = 1 

(VaR_P_NoDiv=VaR_I_Normal+VaR_A_Normal)

(VaR_P_NoDiv/W) 

#Pero si HAY beneficios por diversificar

(VaR_P=z*sigma_p*sqrt(h)*W)  #VaR REL
(VaR_P/W) 

#Medido en unidades monetarias
benefxdiver=VaR_P_NoDiv-VaR_P
benefxdiver

#VaR Marginal ---------------
(betas=(matriz_varcov%*%w)/sigma_p^2) #beta del activo vs el portafolio

sum(t(w)%*%betas) #El promedio ponderado de los betas == 1

(VaR_Mgal=VaR_P/W*betas)

ticker1= "AAPL"
ticker2= "IBM"

VaR_Mgal_rdos=matrix((VaR_Mgal=VaR_P/W*betas))
VaR_Mgal_rdos

cat("- Si se realiza un cambio infinitesimal en la posición de", ticker1, ", 
    se observa un aumento en el VaR de aproximadamente", VaR_Mgal_rdos[1, 1], "\n\n")

cat("- Por otro lado, un cambio infinitesimal en la posición de", ticker2, 
    "resulta en un aumento en el VaR de alrededor de", VaR_Mgal_rdos[2, 1], 
    "\n\n")

#VaR Incremental -------------
#Supongo que agrego 100 acciones mas de AAPL

W_A_incr=170*stock_data$AAPL[nrow(stock_data)] 
W_A_incr
W_I=100*stock_data$IBM[nrow(stock_data)]
W_I 

(W_incr=W_A_incr+W_I)
(w_incr=c(W_A_incr,W_I)/W) #nuevos ponderadores

(sigma_p_incr=sqrt(t(w_incr)%*%matriz_varcov%*%w_incr)%>%as.double())

#Verificando con la formula para una cartera con dos activos:
(sqrt(w_incr[1]^2*sigma_AAPL^2+w_incr[2]^2*sigma_IBM^2+2*w_incr[1]*w_incr[2]*sigma_AAPL*sigma_IBM*Rho))

(VaR_P_incr=z*sigma_p_incr*sqrt(h)*W_incr)  #VaR REL
VaR_P_Increm=VaR_P_incr-VaR_P
VaR_P_Increm #Por adquirir 100 acciones mas AAPL

#Component VaR-------------------- 
#con cartera original

VaR_Comp=VaR_P*betas*w
VaR_Comp

sum(VaR_Comp)-VaR_P #Chequeo
VaR_Comp/VaR_P

cat("Más allá de que la composición del portafolio es", w, ", la composición en riesgo es", VaR_Comp/VaR_P)


