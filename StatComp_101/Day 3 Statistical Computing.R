# Probability of an Interval
# Example I: Gamma Function
alpha = 2
beta = 3
f = function(x){
  x^(alpha - 1)*exp(-x/beta)/(gamma(alpha)*beta^alpha)*(x>0)
}
curve(f(x), -0.5, 20, col = "red", lwd = 2)
abline(v = 5, col = "blue", lwd = 2, lty = 2)
abline(v = 10, col = "blue", lwd = 2, lty = 2)
abline(h = 0, col = "black", lwd = 2, lty = 1)
text(7.5, 0.04, "P(5<X<10)")

out = integrate(f, lower = 5, upper = 10)
out$value

# Center of Mass
# In probability and statistics, the center of mass refers to the expectation (mean) of a distribution.
# The expectation provides an average or "center" of the distribution.

# Expectation (Mean)
# For a random variable X with probability mass function (PMF) or probability density function (PDF),
# the expectation is the weighted average of the possible values of X, where the weights are the probabilities or densities.

# For discrete random variables:
# E(X) = sum(x * P(X = x))

# For continuous random variables:
# E(X) = integral from -inf to inf of x * f(x) dx

# Variance (Second-Order Expectation)
# The variance describes how much the values of a random variable differ from the expectation:
# Var(X) = E[(X - E(X))^2] = E(X^2) - (E(X))^2

# 3. Higher-Order Moments
# Higher-order moments describe more detailed characteristics of a distribution:
# - Skewness: Measures the asymmetry of the distribution.
# - Kurtosis: Measures the "tailedness" or peakness of the distribution.

# Example II: Discrete Random Variable (Poisson Distribution)
# P(X = x) = (λ^x * e^(-λ)) / x!, where x = 0, 1, 2, ...
# E(X) = λ
# Var(X) = λ

lambda = 3
x = 0:10

prob_x = dpois(x, lambda)

E_X = sum(x * prob_x)  # E(X) = sum(x * P(X=x))
cat("Expectation (Poisson):", E_X, "\n")

E_X2 = sum(x^2 * prob_x)  # E(X^2) = sum(x^2 * P(X=x))

Var_X = E_X2 - E_X^2      # Var(X) = E(X^2) - (E(X))^2
cat("Variance (Poisson):", Var_X, "\n")

plot(x, prob_x, type = "h", col = "blue", lwd = 2, 
     main = "Poisson PMF (lambda = 3)",
     ylab = "P(X=x)", xlab = "X", ylim = c(0, max(prob_x) + 0.02))
points(E_X, dpois(round(E_X), lambda), col = "red", pch = 20, cex = 2)
text(E_X, dpois(round(E_X), lambda) + 0.01, labels = paste("E(X) =", round(E_X, 2)), col = "red")
polygon(c(E_X - sqrt(Var_X), E_X + sqrt(Var_X), E_X + sqrt(Var_X), E_X - sqrt(Var_X)),
        c(0, 0, max(prob_x), max(prob_x)), col = rgb(1, 0, 0, 0.2), border = NA)
legend("topright", legend = c("Expectation", "Variance"), col = c("red", "red"), pch = c(19, NA), 
       fill = c(NA, rgb(1, 0, 0, 0.2)))


# Example III: Continuous Random Variable (Gamma Distribution)
# f(x; α, β) = (x^(α - 1) * e^(-x/β)) / (β^α * Γ(α)), where x > 0
# E(X) = α * β
# Var(X) = α * β^2
alpha = 2
beta = 2

f_gamma = function(x, alpha, beta) {
  ifelse(x < 0, 0, (x^(alpha - 1) * exp(-x / beta)) / (beta^alpha * gamma(alpha)))
}
x = seq(0, 10, by = 0.01)
f_val_gamma = numeric(length(x))
for (i in 1:length(x)) {
  f_val_gamma[i] = f_gamma(x[i], alpha, beta)
}

plot(x, f_val_gamma, type = "l", col = "green", lwd = 2, 
     main = "Gamma PDF (alpha = 2, beta = 1)",
     ylab = "f(x)", xlab = "X")

abline(v = alpha * beta, col = "red", lty = 2)
text(alpha * beta, max(f_val_gamma) * 0.9, labels = paste("E(X) =", round(alpha * beta, 2)), col = "red")
polygon(c(alpha * beta - sqrt(alpha * beta^2), alpha * beta + sqrt(alpha * beta^2), 
          alpha * beta + sqrt(alpha * beta^2), alpha * beta - sqrt(alpha * beta^2)),
        c(0, 0, max(f_val_gamma) * 0.8, max(f_val_gamma) * 0.8), col = rgb(0, 1, 0, 0.2), border = NA)
legend("topright", legend = c("Expectation", "Variance"), col = c("red", "green"), lty = c(2, NA),
       fill = c(NA, rgb(0, 1, 0, 0.2)))

# A function to calculate nth Order Moment of a Probability Distribution
Expectation_calculator <- function(f, type, orderofmoment = 1, x = NULL) {
  # Define function along with their bounds and values of specific parameters.
  n <- orderofmoment
  if (tolower(type) == "pdf") {
    expectation <- integrate(function(x) x^n * f(x), lower = -Inf, upper = Inf)$value
  } else if (tolower(type) == "pmf") {
    if (is.null(x)) stop("For PMF, please provide the values of the random variable (x).")
    expectation <- sum(x^n * f)  
  } else {
    stop("Invalid type. Please specify either 'pdf' for continuous or 'pmf' for discrete distribution.")
  }
  return(expectation)
}

# Normal Distribution or Gaussian Distribution
# The normal distribution is the only distribution whose cumulants beyond the first two (i.e., other than the mean and variance) are zero. 
# It is also the continuous distribution with the maximum entropy for a specified mean and variance.
# Assuming that the mean and variance are finite, that the normal distribution is the only distribution where the 
# mean and variance calculated from a set of independent draws are independent of each other
mu = 0     # mean
sigma = 1  # standard deviation
f_norm = function(x) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-((x - mu)^2) / (2 * sigma^2))
}
f_norm_cdf = function(x) {
  pnorm(x, mean = mu, sd = sigma)
}
x = seq(-4 * sigma, 4 * sigma, by = 0.01)
f_val_norm = numeric(length(x))
cdf_val_norm = numeric(length(x))
for (i in 1:length(x)) {
  f_val_norm[i] = f_norm(x[i])
}
cdf_val_norm = f_norm_cdf(x)
par(mfrow = c(1, 2))
plot(x, f_val_norm, type = "l", col = "red", lwd = 2, 
     main = "Normal Distribution PDF", ylab = "f(x)", xlab = "x")
plot(x, cdf_val_norm, type = "l", col = "blue", lwd = 2, 
     main = "Normal Distribution CDF", ylab = "F(x)", xlab = "x")

# Example IV : Mean and Variance of the Normal Distribution
# f(x; μ, σ) = (1 / (σ * sqrt(2 * π))) * e^(-((x - μ)^2) / (2 * σ^2))
# E(X) = μ
# Var(X) = σ^2
mu = 0     # Mean of the distribution
sigma = 1  # Standard deviation of the distribution
f_norm = function(x, mu, sigma) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-((x - mu)^2) / (2 * sigma^2))
}
mean_norm <- Expectation_calculator(function(x) f_norm(x, mu, sigma), type = "pdf", orderofmoment = 1)
print(paste("Mean (E(X)) =", round(mean_norm, 2)))
second_moment <- Expectation_calculator(function(x) f_norm(x, mu, sigma), type = "pdf", orderofmoment = 2)
print(paste("Second Moment =", round(second_moment, 2)))
variance <- second_moment - mean_norm^2    # Variance = E(X^2) - (E(X))^2
print(paste("Variance =", round(variance, 2)))
x = seq(-4 * sigma, 4 * sigma, by = 0.01)
f_val_norm = numeric(length(x))
for (i in 1:length(x)) {
  f_val_norm[i] = f_norm(x[i], mu, sigma)
}

plot(x, f_val_norm, type = "l", col = "purple", lwd = 2, 
     main = "Normal Distribution PDF (μ = 0, σ = 1)",
     ylab = "f(x)", xlab = "X")
abline(v = mu, col = "royalblue", lty = 2)
text(mu, max(f_val_norm) * 0.9, labels = paste("E(X) =", round(mean_norm, 2)), col = "royalblue")
variance_shaded_area = sqrt(variance)
polygon(c(mu - variance_shaded_area, mu + variance_shaded_area, 
          mu + variance_shaded_area, mu - variance_shaded_area),
        c(0, 0, max(f_val_norm) * 0.8, max(f_val_norm) * 0.8), 
        col = rgb(1, 0.71, 0.75, alpha = 0.2), border = NA)
legend("topright", legend = c("Expectation", "Variance"), col = c("royalblue", "purple"), 
       lty = c(2, NA), fill = c(NA, rgb(1, 0.71, 0.75, alpha = 0.2)))

# The normal distribution is particularly significant because its formula incorporates both the mean (μ) and variance (σ²) directly into its definition.
# This makes it a fundamental distribution in statistics, as it naturally represents data where the mean and variance are essential characteristics. 
# The normal distribution’s unique property of being fully described by just two parameters, μ and σ², underscores its central role in statistical theory and practice.

# In statistics, the 68–95–99.7 rule, also known as the empirical rule, and sometimes abbreviated 3sr,
# is a shorthand used to remember the percentage of values that lie within an interval estimate in a normal distribution: 
# approximately 68%, 95%, and 99.7% of the values lie within one, two, and three standard deviations.

#############################################################

# 3rd Order Moment
f = function(x){
  3*x^2*(x>0)*(x<1)
}

g = function(x){
  3*(1-x)^2*(x>0)*(x<1)
}

integrate(f, 0, 1)
integrate(g, 0, 1)

curve(f(x), -0.5, 1.5, col = "red", lwd = 2, ylab = "Density")
curve(g(x),add = TRUE, -0.5, 1.5, col = "yellow", lwd = 2)
legend("topright", legend = c("f(x)", "g(x)"),
       col = c("red", "blue"), lwd = c(2,2), cex = 1.5, bty = "n")

# Computing Expectation

mu_f = function(x){
  x*f(x)
}

mu_g = function(x){
  x*g(x)
}

integrate(mu_f, 0 ,1)           # Expectation of f
integrate(mu_g, 0, 1)           # Expectation of g

mu1 = integrate(mu_f, 0, 1)$value
mu2 = integrate(mu_g, 0, 1)$value
points(mu1, f(mu1), pch = 19, col="red", cex =1.5)
points(mu2, g(mu2), pch = 19, col="yellow", cex =1.5)

# Computation of Variance

mu2_f = function(x){
  (x-mu1)^2*f(x)
}

mu2_g = function(x){
  (x-mu2)^2*g(x)
}

integrate(mu2_f, 0, 1)
integrate(mu2_g, 0, 1)

var_f = integrate(mu2_f, 0, 1)$value
var_g = integrate(mu2_g, 0, 1)$value

cat("Variences of f and g are:\n", c(var_f, var_g))

# Third Order Central Moments

mu3_f = function(x){
  (x-mu1)^3*f(x)
}

mu3_g = function(x){
  (x-mu2)^3*g(x)
}

# Computing Skewness

skewness_f = integrate(mu3_f, 0, 1)$value/var_f^(3/2)
skewness_g = integrate(mu3_g, 0, 1)$value/var_g^(3/2)

cat("The skewness of f is :", skewness_f,"\n")
cat("The skewness of g is :", skewness_g,"\n")

