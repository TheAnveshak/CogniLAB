# Visualizations for different types of probability distributions and their respective CDFs

# Discrete Random VAriables and Their Distributions  

# Binomial Distribution
# Bin(n,p) is used to model the number of successes in a fixed number of trials (n),
# where each trial has a probability of success p.
n = 6    # number of trials
p = 0.7  # probability of success
x = 0:n  # possible outcomes
par(mfrow = c(1, 2))
# PMF of Bin(n,p)
prob = dbinom(x, size = n, prob = p) 
plot(x, prob, type = "h", col = "red", lwd = 2, ylab = "P(X=x)", main = "Bin(6,0.7)")
# CDF of Binomial
points(x, prob, col = "blue", pch = 19)
plot(x, pbinom(x, size = n, prob = p), type = "s", col = "red", lwd = 2, ylab = "F(X<=x)", main = "Binomial CDF")

help("points")        # know about this function
factorial(x)

# Poisson Distribution
# Poisson(lambda) models the number of events occurring in a fixed interval of time or space,
# where events occur with a known constant mean rate (lambda).
lambda = 4  # rate parameter
x = 0:15    # Domain
par(mfrow = c(1, 2))
# PMF of Poisson(lambda)
prob = dpois(x, lambda)
plot(x, prob, type = "h", col = "red", lwd = 2, ylab = "P(X=x)", main = "Poisson PMF")
# CDF of Poisson(lambda)
points(x, prob, col = "blue", pch = 19)
plot(x, ppois(x, lambda), type = "s", col = "red", lwd = 2, ylab = "F(X<=x)", main = "Poisson CDF")

# Uniform Distribution
# The Uniform(n) models the equal likelihood of outcomes in a finite range.
x = 1:6  # possible outcomes (e.g., a fair dice)
par(mfrow = c(1, 2))
# PMF of Uniform(n)
prob = dunif(x, min = 1, max = 6)
plot(x, prob, type = "h", col = "red", lwd = 2, ylab = "P(X=x)", main = "Uniform Discrete PMF")
# CDF of Uniform(n)
points(x, prob, col = "blue", pch = 19)
plot(x, punif(x, min = 1, max = 6), type = "s", col = "red", lwd = 2, ylab = "F(X<=x)", main = "Uniform Discrete CDF")


# Continuous Distribution

# Beta Distribution
# Beta(m,n) is used to model random variables that are constrained to lie between 0 and 1.
m = 4  # m > 0
n = 3  # n > 0
f_beta = function(x) {
  (x^(m-1) * (1 - x)^(n-1)) / beta(m, n) * (x > 0) * (x < 1)
}
x = seq(-0.5, 1.5, by = 0.01)
par(mfrow = c(1, 2))
# PDF of Beta(m)
f_val_beta = numeric(length(x))  
for (i in 1:length(x)) {
  f_val_beta[i] = f_beta(x[i])  
}
plot(x, f_val_beta, type = "l", col = "red", lwd = 2, main = "Beta PDF (m=4, n=3)", ylab = "f(x)")
# CDF of Beta
cdf_val_beta = pbeta(x, shape1 = m, shape2 = n)
plot(x, cdf_val_beta, type = "l", col = "blue", lwd = 2, main = "Beta CDF (m=4, n=3)", ylab = "F(x)")

# Exponential Distribution 
# Exp(lambda) is  exponential distribution or negative exponential distribution 
# is the probability distribution of the distance between events in a Poisson point process, 
# i.e., a process in which events occur continuously and independently at a constant average rate.
lambda = 1  # rate parameter
f_exp = function(x) {
  lambda * exp(-lambda * x) * (x > 0)
}

x = seq(0, 5, by = 0.01)  # range of x values for plotting
f_val_exp = numeric(length(x))  # initialize an empty vector to store function values
par(mfrow = c(1, 2))
# PDF of Exp(lambda)
for (i in 1:length(x)) {
  f_val_exp[i] = f_exp(x[i])  # evaluate the PDF for each x value
}
plot(x, f_val_exp, type = "l", col = "red", lwd = 2, main = "Exp(lambda)", ylab = "f(x)")
#CDF of exp(lambda)
cdf_val_exp = pexp(x, rate = lambda)
plot(x, cdf_val_exp, type = "l", col = "blue", lwd = 2, main = "Exponential CDF", ylab = "F(x)")

# Gamma Distribution (Continuous)
# Gamma(a,b) has parameterization with α and β is more common in Bayesian statistics,
# where the gamma distribution is used as a conjugate prior distribution for various types of inverse scale (rate) parameters.
# The gamma distribution is the maximum entropy probability distribution for a random variable X.
# The Gamma Distribution models the waiting time until the k-th event occurs in a Poisson process.
alpha = 2  # shape parameter
beta = 1   # scale parameter
f_gamma = function(x, alpha, beta) {
  ifelse(x < 0, 0, (x^(alpha - 1) * exp(-x / beta)) / (beta^alpha * gamma(alpha)))
}
x = seq(0, 5, by = 0.01)
f_val_gamma = numeric(length(x))
par(mfrow = c(1, 2))
f_val_gamma = f_gamma(x, alpha, beta)
plot(x, f_val_gamma, type = "l", col = "green", lwd = 2, main = "Gamma PDF", ylab = "f(x)")
f_val_gamma_cdf = numeric(length(x))

for (i in 1:length(x)) {
  f_val_gamma_cdf[i] = integrate(f_gamma, lower = 0, upper = x[i], alpha = alpha, beta = beta)$value
}
plot(x, f_val_gamma_cdf, type = "l", col = "blue", lwd = 2, main = "Gamma CDF", ylab = "F(x)")

