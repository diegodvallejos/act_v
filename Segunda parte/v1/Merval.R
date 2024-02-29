library(tidyverse)
library(quantmod) #Permite obtener datos de precios historicos de Yahoo Finance!

rm(list = ls())
graphics.off()

#Obtener datos de precios de acciones utilizando el simbolo de ticker de Yahoo Finance!
ticker="M.BA"
desde="2023-01-01"  #Respetar el formato
hasta="2023-06-30"
getSymbols(ticker, from = desde , to= hasta)

stock_data = tibble(date=index(M.BA),
                    M.BA=as.double(M.BA$M.BA.Close),
              )

stock_data = na.omit(stock_data)

View(stock_data)

sum(!(index(stock_data$date)==index(stock_data$M.BA))) 

#Rendimiento

R_MBA=c(NA,log(stock_data$M.BA[2:nrow((stock_data))]/stock_data$M.BA[1:(nrow(stock_data)-1)]))
(sigma_M.BA=sd(R_MBA[2:118])*sqrt(252))

#Grafico--------------
#Grafico Precios
ggplot(stock_data, aes(x=date))+
  geom_line(aes(y=M.BA, color = "M.BA"))+ 
  scale_color_manual(values = c("M.BA" = "purple"))+
  labs(title = "Precios de cierre de acciones",
       x = "Fecha",
       y= "Precio de cierre",
       color = "Indice")+ 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y")+
  theme(axis.text.x = element_text(angle = 90))

#Grafico Retornos
ggplot(stock_data, aes(x = date)) +
  geom_line(aes(y = R_MBA, color = "M.BA")) +
  scale_color_manual(values = c("M.BA" = "purple")) +
  labs(
    title = "Rendimiento Diario",
    x = "Fecha",
    y = "Rendimiento",
    color = "Indice"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle = 90))

c=0.99 #Nivel de confianza
z=qnorm(c) #Supongo distribucion normal
h=5/252 #Horizonte temporal: 5 dias/ 252 habiles

#VaR Individual ~ Supongo cantidad 70 

W_M.BA=70*stock_data$M.BA[nrow(stock_data)] 

(VaR_M_ind=z*sigma_M.BA*sqrt(h)*W_M.BA)

#Verificando 
t=5
(z*sd(R_MBA)*sqrt(5)*W_M.BA)

(VaR_M_ind/W_M.BA)

#Enfoque No Parametrico

perdida=quantile(R_MBA[2:118],1-c) 

cat("Mis retornos diarios tienen 1% de probabilidad de tener una perdida mayor al",perdida 
    ,"\n")

#VaR ABSOLUTO ~ riqueza que tengo hoy

(VaR_NoParam_1d_ABS=W_M.BA*-perdida)

cat("La riqueza que tengo hoy, con horizonte de 1 dia, en el portafolio es", VaR_NoParam_1d_ABS,"\n")

#VaR RELATIVO~Riqueza que espero tener

(VaR_NoParam_1d_REL=(mean(R_MBA[2:118])-perdida)*W_M.BA)

cat("La riqueza que espero tener, con horizonte de 1 dia, 
    en el portafolio es", VaR_NoParam_1d_REL,"\n")

#Enfoque PARAMETRICO---------

(VaR_A_Normal=z*sigma_M.BA*sqrt(h)*W_M.BA)

#Verificando 
t=5
(z*sd(R_MBA[2:118])*sqrt(5)*W_M.BA)

(VaR_A_Normal/W_M.BA)


calcular_ewma=function(retornos, lambda) {
  r_promedio=0
  sumatoria=0
  for (i in length(retornos):1) {
    sumatoria=sumatoria + lambda^(length(retornos) - i) * (retornos[i] - r_promedio)^2
  }
  
  return((sqrt((1 - lambda) * sumatoria))*sqrt(252)) #anualizada
}

(sigma_M.BA_EWMA=calcular_ewma(R_MBA[2:118],0.94))

(comparacion_desvio=tibble(sigma_M.BA,sigma_M.BA_EWMA))
colnames(comparacion_desvio) = c("Desvio M.BA", "EWMA M.BA")
comparacion_desvio
