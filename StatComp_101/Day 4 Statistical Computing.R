# Example I: Joint distribution for two independent uniform variables over [0, 1].
g = function(x, y) {
  (x + y) * (0 < x) * (x < 1) * (0 < y) * (y < 1)
}
x_vals = seq(-0.5, 2, by = 0.05)
y_vals = seq(-0.5, 2, by = 0.05)
g_vals = matrix(data = NA, nrow = length(x_vals), ncol = length(y_vals))
for(i in 1:length(x_vals)){
  for(j in 1:length(y_vals)){
    g_vals[i, j] = g(x_vals[i], y_vals[j])
  }
}
persp(x_vals, y_vals, g_vals, theta = -50, phi = 30, col = "skyblue", main = "Joint PDF for Bivariate Uniform Distribution")

# Example II: Joint distribution of two dependent variables where their product represents their joint probability.
h = function(x, y) {
  (4 * x * y) * (0 < x) * (x < 1) * (0 < y) * (y < 1)
}
x_vals = seq(-0.5, 2, by = 0.05)
y_vals = seq(-0.5, 2, by = 0.05)
h_vals = matrix(data = NA, nrow = length(x_vals), ncol = length(y_vals))
for(i in 1:length(x_vals)){
  for(j in 1:length(y_vals)){
    h_vals[i, j] = h(x_vals[i], y_vals[j])
  }
}
persp(x_vals, y_vals, h_vals, theta = 20, phi = 45, col = "orange", main = "Joint PDF with Linear Relationship")

# Example III: Joint distribution of two independent exponential variables.
f = function(x, y) {
  exp(-x - y) * (x > 0) * (y > 0)
}
F = function(x, y) {
  (1 - exp(-x)) * (1 - exp(-y)) * (x > 0) * (y > 0)
}
x_vals = seq(-0.5, 4, by = 0.1)
y_vals = seq(-0.5, 4, by = 0.1)
f_vals = matrix(data = NA, nrow = length(x_vals), ncol = length(y_vals))
F_vals = matrix(data = NA, nrow = length(x_vals), ncol = length(y_vals))
for(i in 1:length(x_vals)){
  for(j in 1:length(y_vals)){
    f_vals[i, j] = f(x_vals[i], y_vals[j])
    F_vals[i, j] = F(x_vals[i], y_vals[j])
  }
}
persp(x_vals, y_vals, f_vals, theta = 50, phi = 30, col = "#FFBBA3", main = "Joint PDF for Exponential Variables")
persp(x_vals, y_vals, F_vals, theta = 50, phi = 30, col = "yellow", main = "Joint CDF for Exponential Variables")


################################################################################

par(mfrow = c(1,2))
f = function(x, y) {
  exp(-x - y) * (0 <= x) * (x < Inf) * (0 <= y) * (y < Inf)
}
x_vals = seq(-0.5, 5, by = 0.1)
y_vals = seq(-0.5, 5, by = 0.1)
f_vals = matrix(data = NA, nrow = length(x_vals), ncol = length(y_vals))
for (i in 1:length(x_vals)) {
  for (j in 1:length(y_vals)) {
    f_vals[i, j] = f(x_vals[i], y_vals[j])
  }
}
print(f_vals)
dim(f_vals)
persp(x_vals, y_vals, f_vals, theta = 50, col = "yellow",
      main = "Joint PDF")

f = function(x, y) {
  exp(-y) * (0 < x) * (x < y) * (y < Inf)
}
f_vals = matrix(data = NA, nrow = length(x_vals), ncol = length(y_vals))
for (i in 1:length(x_vals)) {
  for (j in 1:length(y_vals)) {
    f_vals[i, j] = f(x_vals[i], y_vals[j])
  }
}
print(f_vals)
dim(f_vals)
persp(x_vals, y_vals, f_vals, theta = 70, col = "yellow",
      main = "Joint PDF for X < Y")

###################################################################################

f_X = function(x) {
  exp(-x) * (0 <= x) * (x < Inf)
}
f_Y = function(x) {
  x * exp(-x) * (0 <= x) * (x < Inf)
}
curve(f_X(x), -0.5, 5, col = "red", lwd = 2, 
      xlab = "x", ylab = expression(f[X](x)), cex.lab = 1.2,
      main = "Marginal PDF of X")
curve(f_Y(x), -0.5, 8, col = "red", lwd = 2,
      xlab = "y", ylab = expression(f[Y](y)), cex.lab = 1.2,
      main = "Marginal PDF of Y")
par(mfrow = c(1,2))
f_Yx = function(y) {
  exp(-(y - x)) * (x <= y) * (0 < x)
}
x = 0.5
curve(f_Yx(x), 0, 10, col = "red", lwd = 2, 
      ylab = expression(f(y~"|"~x)),
      xlab = "y", main = "Conditional PDF f(y | x)")
x = 2
curve(f_Yx(x), add = TRUE, col = "magenta", lwd = 2)
x = 4.5
curve(f_Yx(x), add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("x = 0.5", "x = 2.0", "x = 4.5"),
       col = c("red", "magenta", "blue"), lwd = c(2,2,2),
       bty = "n")
f_Xy = function(x) {
  (1 / y) * (0 <= x) * (x <= y)
}
y = 1
curve(f_Xy(x), 0, 8, col = "red", lwd = 2, 
      ylab = expression(f(x~"|"~y)),
      xlab = "x", main = "Conditional PDF f(x | y)")
y = 3
curve(f_Xy(x), add = TRUE, col = "magenta", lwd = 2)
y = 6
curve(f_Xy(x), add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("y = 1", "y = 3", "y = 6"),
       col = c("red", "magenta", "blue"), lwd = c(2,2,2), bty = "n")

