# Likelihood
par(mfrow = c(1,2))

L = function(p){
  (1-p)^(19)*p^5*(0<p)*(p<1)
}
curve(L(x), 0,1, col='red', lwd=2, ylab='L(p)')
points(5/24,0, pch =19, col="blue",cex=1.5)

l = function(p){
  log((1-p)^(19)*p^5*(0<p)*(p<1))
}
curve(l(x), 0,1, col='red', lwd=2, ylab='l(p)',ylim=c(-80,1))
points(5/24,0, pch =19, col="blue",cex=1.5)

lambda = 5
f = function(x){
  lambda*exp(-lambda*x)*(x>0)
}
curve(f(x),)
