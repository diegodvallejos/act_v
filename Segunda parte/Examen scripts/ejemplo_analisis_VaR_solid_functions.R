# Función para calcular la matriz de covarianzas
calculate_covariances <- function(sigmas, correlaciones) {
  covarianzas = matrix(rep(NA, 9), nrow = 3)
  for (i in 1:3) {
    for (j in 1:3) {
      covarianzas[i, j] = sigmas[i] * sigmas[j] * correlaciones[i, j]
    }
  }
  return(covarianzas)
}

# Función para calcular el VaR individual
calculate_individual_VaR <- function(sigmas, h, W, z) {
  return(z * sigmas * sqrt(h) * W)
}

# Función para calcular el VaR diversificado
calculate_diversified_VaR <- function(sigma_cartera, z, h, W) {
  return(z * sigma_cartera * sqrt(h) * sum(W))
}

# Función para calcular los ponderadores y matriz de Betas
calculate_weights_and_betas <- function(covarianzas, w_porcentaje) {
  varianza_cartera = as.double(t(w_porcentaje) %*% covarianzas %*% w_porcentaje)
  sigma_cartera = sqrt(varianza_cartera)
  VaR_Diversificado = z * sigma_cartera * sqrt(h) * sum(W)
  Betas = (covarianzas %*% w_porcentaje) / varianza_cartera
  
  return(list(varianza_cartera = varianza_cartera, sigma_cartera = sigma_cartera,
              VaR_Diversificado = VaR_Diversificado, Betas = Betas))
}

# Función para calcular el VaR marginal, componente y porcentaje de VaR
calculate_marginal_component_percent_VaR <- function(VaR_Diversificado, W, w_porcentaje, Betas) {
  VaR_Marginal = VaR_Diversificado / sum(W) * Betas
  VaR_Component = VaR_Diversificado * as.double(w_porcentaje) * Betas
  Porcentaje_VaR = VaR_Component / VaR_Diversificado
  
  return(list(VaR_Marginal = VaR_Marginal, VaR_Component = VaR_Component,
              Porcentaje_VaR = Porcentaje_VaR))
}

# Función para generar la tabla final
generate_final_table <- function(Betas, VaR_Marginal, VaR_Component, Porcentaje_VaR) {
  tabla = cbind(Acciones = c("A", "B", "C"), Betas, VaR_Marginal, VaR_Component, Porcentaje_VaR)
  colnames(tabla)[2] <- "Beta"
  colnames(tabla)[3] <- "VaR Marginal"
  colnames(tabla)[4] <- "Component VaR"
  colnames(tabla)[5] <- "%VaR"
  return(tabla)
}

# Datos iniciales
Precios = c(10, 40, 20)
Cantidades = c(300, 200, 250)
sigmas = matrix(c(25, 15, 20) / 100, nrow = 3)
data = cbind(Precios, Cantidades, sigmas)
colnames(data)[3] <- "Volatilidad"
data = cbind(Acciones = c("A", "B", "C"), data)

correlaciones = matrix(c(1.0, 0.7, 0.5,
                         0.7, 1.0, 0.6,
                         0.5, 0.6, 1.0), nrow = 3)

h = 10 / 252
c = 0.99
z = qnorm(c)

# Cálculos
covarianzas = calculate_covariances(sigmas, correlaciones)
W = Precios * Cantidades
w_porcentaje = matrix(W / sum(W), nrow = 3)

VaR_Ind = calculate_individual_VaR(sigmas, h, W, z)
ponderadores = cbind(W, w_porcentaje, VaR_Ind)
ponderadores = cbind(Acciones = c("A", "B", "C"), ponderadores)
colnames(ponderadores)[2] <- "%W"
colnames(ponderadores)[3] <- "VaR Individual"

resultados <- calculate_weights_and_betas(covarianzas, w_porcentaje)
tabla_resultados <- calculate_marginal_component_percent_VaR(resultados$VaR_Diversificado, W, w_porcentaje, resultados$Betas)
tabla_final <- generate_final_table(resultados$Betas, tabla_resultados$VaR_Marginal, tabla_resultados$VaR_Component, tabla_resultados$Porcentaje_VaR)

# Visualización
print(ponderadores)
print(tabla_final)
