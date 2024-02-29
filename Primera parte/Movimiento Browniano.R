# Movimiento Browniano----------------------------------------------------------
library(tidyverse)

#Geometric Browninan Motion simulation with 10,000 simulations

#Parameters
S0 <- 100 #Initial price
mu <- 0.05 #drift (mean return)
sigma <- 0.2 #Volatility (standard deviation of return)
T <- 1 #Time period
N <- 252 #Number of time steps
dt <- T/N #time increment
num_simulations <- 10000 #number of simulations


#Generate random normal values
set.seed(123)
epsilon <- matrix(rnorm(N * num_simulations, mean=0, sd=1), nrow=N)

#Calculate drift and volatility terms
drift <- (mu - 0.5*sigma^2) * dt
volatility <- sigma * sqrt(dt)

#Generate price series for each simulation
prices <- matrix(nrow=N+1, ncol=num_simulations)
prices[1, ] <- S0

for (i in 2:(N+1)){
  prices[i, ] <- prices[i-1, ] * exp(drift + volatility * epsilon[i-1, ])
}

#Select a random sample of simulations to plot
num_sample <- 100 #number of simulations to plot
sample_indices <- sample(1:num_simulations, num_sample)

#Prepare data for ggplot
data <- data.frame(time=rep(1:(N+1), num_sample),
                   price=c(prices[, sample_indices]),
                   simulation=rep(sample_indices, each= N+1))

#Plot the price series using ggplot
p <- ggplot(data) + geom_line(aes(x = time, y = price, color = as.factor(simulation))) +
  xlab("Time") + 
  ylab("Price") + 
  ggtitle(paste("Geometric Brownian Motion -", num_sample, "Simulations")) + 
  theme_minimal() + 
  theme(legend.position = "none")

p

#Calculate confidence interval for each time step
percentiles = c(0.025, 0.975)

confidence_interval <- tibble(time = 1:(N+1),
                              Mean = apply(prices, 1, mean, na.rm = TRUE),
                              as_tibble(t(apply(prices, 1, quantile, probs = percentiles, na.rm = TRUE))) %>%
  rename_with(~c("LI", "LS")))

p + geom_line(data = confidence_interval, aes(x = time, y = LI), lwd = 1.2, col = "red") + 
  geom_line(data = confidence_interval, aes(x = time, y = LS), lwd = 1.2, col = "red") + 
  geom_line(data = confidence_interval, aes(x = time, y = Mean), lwd = 1.2, col = "black") + 
  ggtitle(paste("Geometric Brownian Motion -", num_sample, "Simulations", "-Mean and Confidence interval", percentiles[1]*100, "-", percentiles[2]*100))


p














