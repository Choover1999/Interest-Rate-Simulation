#Simulation of Cox-Ingersoll-Ross interest rate model

#The CIR model is given as d(R(t)) = Alpha * (Beta - R(t)) * dt + Sigma * sqrt(R(t)) * dW(t)

#Setting Constants

alpha <- 0.1 #this is the speed of mean reversion
beta <- 0.05 #this is the long term interest rate
sigma <- 0.15
days <- 365
dt <- 1/days
initial_rate <- 0.026

set.seed(123)
random_variables <- rnorm(days)

interest_rate_sim_1 <- numeric()
interest_rate_sim_1[1] <- initial_rate

for (i in 1:(days-1)) {
  interest_rate_sim_1[i+1] <- max(interest_rate_sim_1[i] + (alpha * (beta - interest_rate_sim_1[i]) * dt + sigma * sqrt(interest_rate_sim_1[i]) * sqrt(dt) * random_variables[i]), 0)
}

plot(interest_rate_sim_1, type = 'l')

#Let's see how changing the alpha values affects the model 

alpha_seq <- seq(0.1, 1, by = 0.1)

alpha_test_matrix <- matrix(NA, nrow = days, ncol = length(alpha_seq))

for (j in 1:length(alpha_seq)) {
  alpha_test_matrix[1, j] <- initial_rate
  for (i in 1:(days-1)) {
    alpha_test_matrix[i+1, j] <- max(alpha_test_matrix[i, j] + (alpha_seq[j] * (beta - alpha_test_matrix[i, j]) * dt + sigma * sqrt(alpha_test_matrix[i, j]) * sqrt(dt) * random_variables[i]), 0)
  }
}

# Plot each column from alpha_test_matrix with different colors and labels
matplot(1:days, alpha_test_matrix, type = "l", col = rainbow(length(alpha_seq)), lty = 1, xlab = "Days", ylab = "Interest Rate", main = "CIR Model with Different Alpha Values")
legend("topleft", legend = as.character(alpha_seq), col = rainbow(length(alpha_seq)), lty = 1, cex = 0.8, title = "Alpha Values")
abline(h = beta, col = "black", lty = 2)





