par(mfrow = c(1,2))

x = c(2,0,2,4,7,0,1,1,3,10)

psi_lambda = function(lambda){
  exp(-10*lambda)*lambda^30*(0<lambda)
}

lambda_vals = seq(0.01, 10, by = 0.001)
psi_vals = numeric(length=length(lambda_vals))
for(i in 1:length(lambda_vals)){
  psi_vals[i] = psi_lambda(lambda = lambda_vals[i])
}

plot(lambda_vals, psi_vals, type ="l", col="red", lwd=2,
     xlab = expression(lambda), yvals = expression(psi(lambda)))

log_psi_vals = log(psi_vals)

plot(lambda_vals, log_psi_vals, type ="l", col="red", lwd=2,
     xlab = expression(lambda), yvals = expression(log(psi(i(lambda)))))
