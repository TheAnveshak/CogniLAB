# Static and Estimator

n_vals = 2:1000
psi_1_vals = numeric(length = length(n_vals))
psi_2_vals = numeric(length = length(n_vals))
mu_hat_vals = numeric(length = length(n_vals))

for(i in 1:length(n_vals)){
  n = n_vals[i]
  x = rnorm(n = n, mean = 1, sd = 1)
  
  psi_1_vals[i] = (mean(x) + 1) / (n + 1)
  psi_2_vals[i] = (x[1] + x[2]) / 2
  mu_hat_vals[i] = mean(x)
}

par(mfrow = c(2,3))
plot(n_vals, psi_1_vals, col = "red", lwd = 2,
     xlab = "sample size (n)", type = "l",
     ylab = expression(psi[1]), ylim = c(0, 1.2))
abline(h = 1, col = "blue", lwd = 2, lty = 2)

plot(n_vals, psi_2_vals, col = "red", lwd = 2,
     xlab = "sample size (n)", type = "l",
     ylab = expression(psi[2]), ylim = c(-3, 3))  # Corrected ylab
abline(h = 1, col = "blue", lwd = 2, lty = 2)

plot(n_vals, mu_hat_vals, col = "red", lwd = 2,
     xlab = "sample size (n)", type = "l",
     ylab = expression(mu_hat), ylim = c(0, 1.5))  # Corrected ylab
abline(h = 1, col = "blue", lwd = 2, lty = 2)


# Example 2 for poisson dist

par(mfrow = c(1,1))
n_vals = 1:10000
sample_means = numeric(length = length(n_vals))
lambda = 5

for(i in n_vals){
  x = rpois(n = n, lambda = lambda)
  sample_means[n] = mean(x)
}
plot(n_vals, sample_means, col="red",
     lwd = 2, type = "l",
     xlab = "sample size (n)", ylab = expression(X_n))

par(mfrow = c(1,1))
n_vals = 1:10000
sample_means = numeric(length = length(n_vals))
lambda = 5

for(n in n_vals){
  x = rcauchy(n = n)
  sample_means[n] = mean(x)
}
plot(n_vals, sample_means, col="red",
     lwd = 2, type = "l",
     xlab = "sample size (n)", ylab = expression(X_n))



