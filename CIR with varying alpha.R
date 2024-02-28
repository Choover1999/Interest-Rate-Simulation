#Now let's let alpha itself be a function. We want the interest rate to feel in increase of pull towards the mean the further it gets away

#Again we begin the same way as the first approach
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

#Now we want to change alpha to be a function which increases in value the further away from the mean the interest rate gets

alpha_function <- function(alpha, interest_rate) {
  alpha_t <- alpha + (beta - interest_rate)^2
  return(alpha_t)
}

#Rerun simulation with new alpha function

interest_rate_sim_2 <- numeric()
interest_rate_sim_2[1] <- initial_rate

for (i in 1:(days-1)) {
  interest_rate_sim_2[i+1] <- max(interest_rate_sim_2[i] + (alpha_function(alpha, interest_rate_sim_2[i]) * (beta - interest_rate_sim_2[i]) * dt + sigma * sqrt(interest_rate_sim_2[i]) * sqrt(dt) * random_variables[i]), 0)
}

interest_rate_sim_matrix <- cbind(interest_rate_sim_1, interest_rate_sim_2)

matplot(1:days, interest_rate_sim_matrix, type = 'l', xlab = "Days", ylab = "Interest Rate", main = "CIR Model with Different Alpha and Alpha Function")

#This new function doesn't seem to be functionally all that different than the original value for alpha
#We can confirm that it is actually returning different values than the original alpha value
print(head(interest_rate_sim_matrix))
#These two interest rate values are using the same random innovations but are returning slightly different values. We can see that the alpha function is doing something to our interest rate, but it's not all that different

#Now let's try to improve the results by changing the alpha function to have more of an impact on our results

alpha_function_2 <- function(alpha, alpha_0, interest_rate) {
  alpha_t <- alpha + alpha_0 * (beta - interest_rate)
  return(alpha_t)
}

alpha_function_3 <- function(alpha, alpha_0, interest_rate) {
  alpha_t <- alpha + alpha_0 * (beta - interest_rate)^2
  return(alpha_t)
}

#Now we have two more alpha functions which we can test. What I've done is add in another alpha term to the alpha function. We can think of this as the speed of mean reversion of the mean reversion itself. In one case I've left the mean reverting term squared and in the other case I've taken it off. Let's test the alpha function the same way we tested different alphas in the first example. 

alpha_0_seq <- seq(0.1, 1, by = 0.1)

alpha_0_test_matrix <- matrix(NA, nrow = days, ncol = length(alpha_0_seq))

for (j in 1:length(alpha_0_seq)) {
  alpha_0_test_matrix[1, j] <- initial_rate
  for (i in 1:(days-1)) {
    alpha_0_test_matrix[i+1, j] <- max(alpha_0_test_matrix[i, j] + (alpha_function_2(alpha, alpha_0_seq[j], alpha_0_test_matrix[i, j]) * (beta - alpha_0_test_matrix[i, j]) * dt + sigma * sqrt(alpha_0_test_matrix[i, j]) * sqrt(dt) * random_variables[i]), 0)
  }
}

matplot(alpha_0_test_matrix, type = 'l')

#That didn't really seem to have any significant effect either, let's rerun the results with the 3rd alpha function

alpha_0_test_matrix_2 <- matrix(NA, nrow = days, ncol = length(alpha_0_seq))

for (j in 1:length(alpha_0_seq)) {
  alpha_0_test_matrix_2[1, j] <- initial_rate
  for (i in 1:(days-1)) {
    alpha_0_test_matrix_2[i+1, j] <- max(alpha_0_test_matrix_2[i, j] + (alpha_function_3(alpha, alpha_0_seq[j], alpha_0_test_matrix_2[i, j]) * (beta - alpha_0_test_matrix_2[i, j]) * dt + sigma * sqrt(alpha_0_test_matrix_2[i, j]) * sqrt(dt) * random_variables[i]), 0)
  }
}

matplot(alpha_0_test_matrix_2, type = 'l')

#The third alpha function also didn't really have any effect. Why might this be happening? The differences in the interest rates are quite small so squaring them is actually making them smaller. This isn't really having the desired effect on the interest rate reversion that we want. Let's create a new function which takes the square root of the difference. We expect interest ratest to generally be between 0 and 1 by design. Taking the square root then should always return a higher value.

alpha_function_4 <- function(alpha, interest_rate) {
  alpha_t <- alpha + sqrt(beta - interest_rate)
  return(alpha_t)
}

#Test the alpha 4 function
interest_rate_sim_3 <- numeric()
interest_rate_sim_3[1] <- initial_rate

for (i in 1:(days-1)) {
  interest_rate_sim_3[i+1] <- max(interest_rate_sim_3[i] + (alpha_function_4(alpha, interest_rate_sim_3[i]) * (beta - interest_rate_sim_3[i]) * dt + sigma * sqrt(interest_rate_sim_3[i]) * sqrt(dt) * random_variables[i]), 0)
}

interest_rate_sim_matrix_2 <- cbind(interest_rate_sim_1, interest_rate_sim_3)

matplot(interest_rate_sim_matrix_2, type = 'l')

#This was the first test that has shown some noticeably different results compared to just running the results with a constant alpha. We run into a problem with this though. We are trying to accomplish a sort of 'pull' or gravity of the beta level which grows as the rate gets further from the mean. This is opposite of what we are doing with taking the square root. It feels the pull stronger as it returns to the mean. We observe this behavior happening within our graph which creates those larger spikes as we get closer to our beta value. 
#What if we try to increase the value of the alpha_0 sequence to drastically 

alpha_0_seq_2 <- seq(1, 10, by = 1)
alpha_0_test_matrix_4 <- matrix(NA, nrow = days, ncol = length(alpha_0_seq_2))

for (j in 1:length(alpha_0_seq)) {
  alpha_0_test_matrix_4[1, j] <- initial_rate
  for (i in 1:(days-1)) {
    alpha_0_test_matrix_4[i+1, j] <- max(alpha_0_test_matrix_4[i, j] + (alpha_function_3(alpha, alpha_0_seq_2[j], alpha_0_test_matrix_4[i, j]) * (beta - alpha_0_test_matrix_4[i, j]) * dt + sigma * sqrt(alpha_0_test_matrix_4[i, j]) * sqrt(dt) * random_variables[i]), 0)
  }
}

matplot(alpha_0_test_matrix_4, type = 'l')

#Still no significant impact. Let's increase the alpha 0 values again

alpha_0_seq_3 <- seq(10, 100, by = 10)
alpha_0_test_matrix_5 <- matrix(NA, nrow = days, ncol = length(alpha_0_seq_3))

for (j in 1:length(alpha_0_seq_3)) {
  alpha_0_test_matrix_5[1, j] <- initial_rate
  for (i in 1:(days-1)) {
    alpha_0_test_matrix_5[i+1, j] <- max(alpha_0_test_matrix_5[i, j] + (alpha_function_3(alpha, alpha_0_seq_3[j], alpha_0_test_matrix_5[i, j]) * (beta - alpha_0_test_matrix_5[i, j]) * dt + sigma * sqrt(alpha_0_test_matrix_5[i, j]) * sqrt(dt) * random_variables[i]), 0)
  }
}

matplot(alpha_0_test_matrix_5, type = 'l')

#Finally we are able to see some visual impact of of changing our alpha 0 values it doesn't really seem to have a huge impact when in comparison to changing our alpha values all together.Let's do one final test with really high alpha values and move on

alpha_0_seq_4 <- seq(100, 1000, by = 100)
alpha_0_test_matrix_6 <- matrix(NA, nrow = days, ncol = length(alpha_0_seq_4))

for (j in 1:length(alpha_0_seq_4)) {
  alpha_0_test_matrix_6[1, j] <- initial_rate
  for (i in 1:(days-1)) {
    alpha_0_test_matrix_6[i+1, j] <- max(alpha_0_test_matrix_6[i, j] + (alpha_function_3(alpha, alpha_0_seq_4[j], alpha_0_test_matrix_6[i, j]) * (beta - alpha_0_test_matrix_6[i, j]) * dt + sigma * sqrt(alpha_0_test_matrix_6[i, j]) * sqrt(dt) * random_variables[i]), 0)
  }
}

matplot(alpha_0_test_matrix_6, type = 'l')

#With the extreme alpha_0 values we are finally able to see real visual differentiation between the paths taken. 

#As a final exercise let's plot a long path of the high alpha value vs the normal alpha level with a high alpha_0 value


high_alpha_0 <- 1000
days_2 <- 1000
set.seed(17)
random_variables_2 <- rnorm(days_2)

interest_rate_sim_high_alpha <- numeric()
interest_rate_sim_high_alpha[1] <- initial_rate

for (i in 1:(days_2-1)) {
  interest_rate_sim_high_alpha[i+1] <- max(interest_rate_sim_high_alpha[i] + (alpha * (beta - interest_rate_sim_high_alpha[i]) * dt + sigma * sqrt(interest_rate_sim_high_alpha[i]) * sqrt(dt) * random_variables_2[i]), 0)
}

interest_rate_sim_high_alpha_0 <- numeric()
interest_rate_sim_high_alpha_0[1] <- initial_rate

for (i in 1:(days_2-1)) {
  interest_rate_sim_high_alpha_0[i+1] <- max(interest_rate_sim_high_alpha_0[i] + (alpha_function_3(alpha, high_alpha_0, interest_rate_sim_high_alpha_0[i]) * (beta - interest_rate_sim_high_alpha_0[i]) * dt + sigma * sqrt(interest_rate_sim_high_alpha_0[i]) * sqrt(dt) * random_variables_2[i]), 0)
}

final_simulation <- cbind(interest_rate_sim_high_alpha, interest_rate_sim_high_alpha_0)

matplot(final_simulation, type = 'l')










