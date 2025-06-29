# Introduction to PDF and PMF
# Continuous Random Variable: 
# A variable that can take any value within a given range.
# Its probability distribution is described by a Probability Density Function (PDF),
# which gives the relative likelihood of different outcomes.

# Discrete Random Variable: 
# A variable that can take only specific, distinct values (e.g.N, W, Z, discrete R numbers).
# Its probability distribution is given by a Probability Mass Function (PMF), 
# which provides the probability of each distinct value.

# PDF (Probability Density Function) Example
# The PDF must satisfy two conditions:
# 1. The function is non-negative everywhere.
# 2. The area under the entire curve (integral of the PDF) is always 1.

# Example I: PDF for a continuous random variable
f = function(x){
  (4 * x^3) * (0 < x) * (x < 1) 
}
curve(f(x), -1, 2, col = "red", lwd = 2.5, cex.lab = 1.5) 

integrate(f, lower = 0, upper = 1) # = 1

# Example II
par(mfrow = c(2,1))
f = function(x){
  (3 * x^2 + 2) * (0 < x) * (x < 1)  
}
curve(f(x), -1, 2, col = "red", lwd = 2.5, cex.lab = 1.5)

integrate(f, lower = 0, upper = 1)


# Normalizing a Function to Generate a Valid PDF
# Example III
g = function(x){
  f(x)/3  
}
curve(g(x), -1, 2, col = "red", lwd = 2.5, cex.lab = 1.5)
integrate(g, lower = 0, upper = 1)

# Visualizing a continuous distribution and comparing with histogram
par(mfrow = c(2,2))
curve(dnorm(x), col="red", lwd = 2, -4, 4)  

x = rnorm(n = 1000)
points(x, rep(0, length(x)), ylim = c(-0.5, 4), col = "blue", pch = 20, cex = 0.5)

dx = seq(-4, 4, by = 0.1)
count = numeric(length = length(dx) - 1)
for (i in 1:length(count)){
  count[i] = sum((x > dx[i]) * (x < dx[i + 1]))
}
print(count)

mid_dx = numeric(length = length(count))
for(i in 1:length(mid_dx)){
  mid_dx[i] = (dx[i] + dx[i + 1]) / 2
}

plot(mid_dx, (count / (sum(count) * 0.1)), pch = 19, col = "blue", cex = 1)
curve(dnorm(x), col = "red", lwd = 2, -4, 4, add = TRUE)

# Histogram with normal curve
hist(x, probability = TRUE)
curve(dnorm(x), col = "red", lwd = 2, -4, 4, add = TRUE)

# PMF (Probability Mass Function)
# The PMF must satisfy these conditions:
# 1. The function assigns probabilities to each possible value, and these probabilities are non-negative.
# 2. A PMF gives the probability of each possible value of a discrete random variable.
# 3. The sum of all probabilities across all possible values of the discrete random variable is always 1.

# Example IV : discrete random variable
x = c(0, 1, 2)  
p_x = c(0.25, 0.5, 0.25)  
plot(x, p_x, type = "h", col = "red", lwd = 2, ylab = "P(X=x)", ylim = c(0, 1))
points(x, p_x, col = "blue", pch = 19, cex = 1.5)

# Continuous vs Discrete Random Variables:
# A continuous random variable takes an infinite number of possible values (domain).
# A discrete random variable takes a countable number of possible values (domain).

# Discrete CDF plot
par(mfrow = c(1, 2))
x = c(0, 1, 2)
p_x = c(0.25, 0.5, 0.25)
plot(x, p_x, type = "h", col = "red", lwd = 2, ylab = "P(X=x)", ylim = c(0, 1))
points(x, p_x, col = "blue", pch = 19, cex = 1.5)

# CDF (Cumulative Distribution Function) Example
# The CDF must satisfy three conditions:
# 1. The function is non-decreasing; that is, it never decreases as the value of the random variable increases.
# 2. The CDF is right-continuous, which means it does not have jumps from the right-hand side.
# 3. The CDF ranges from 0 to 1, where:
#    - The value approaches 0 as the random variable goes to negative infinity.
#    - The value approaches 1 as the random variable goes to positive infinity.
#
# For discrete random variables, the CDF can be used to calculate the PMF (Probability Mass Function)
# by measuring the jumps in the CDF. Specifically, the PMF can be found as the difference between the 
# CDF values at successive points: PMF(x) = F(x) - F(x-).
#
# For continuous random variables, if the CDF F(x) is differentiable, the PDF (Probability Density Function)
# can be obtained by differentiating the CDF: f(x) = dF(x)/dx.
#
# A random variable X with cumulative distribution function F_X(x) is continuous if there exists a function
# f_X(x) such that:
# - f_X(x) ≥ 0 for all x,
# - Integral of f_X(x) over the entire range = 1,
# - For every a ≤ b, P(a < X < b) = Integral of f_X(x) from a to b.
# The function f_X is called the probability density function (PDF). We have that:
# F_X(x) = Integral of f_X(t) from -∞ to x
# and f_X(x) = dF_X(x)/dx at all points x where F_X is differentiable.

# Example V:
F = function(x){
  if(x < 0)
    return(0)
  if(0 <= x & x < 1)
    return(p_x[1])
  if(1 <= x & x < 2)
    return(p_x[1] + p_x[2])
  if(2 <= x)
    return(p_x[1] + p_x[2] + p_x[3])
}
# Plotting CDF
x_vals = seq(-1, 3, by = 0.01)
F_vals = numeric(length = length(x_vals))
for(i in 1:length(x_vals)){
  F_vals[i] = F(x_vals[i])
}
plot(x_vals, F_vals, col = "red", ylab = expression(F[X](x)), pch = 19, cex = 0.4)
points(c(0, 1, 2), cumsum(p_x), col = "red", pch = 19, cex = 1.5)