require(mvtnorm)
require(ca)

results <- matrix(0, nrow=100, ncol=3)
r <- 0.3333
corr <- diag(2)
corr[1,2] <- corr[2,1] <- r                       # bivariate correlation matrix
ind <- 0

for(rowcat in seq(10, 1000, 10)) {
  
  ind <- ind+1
  colcat <- rowcat                                # number of row and column categories
  rowcuts <- qnorm(seq(0,1,length=rowcat+1))
  colcuts <- rowcuts
  X12exact <- matrix(0, nrow=rowcat, ncol=colcat)
  
  for(i in 1:rowcat) {
    for(j in 1:colcat) {
      lower <- c(rowcuts[i], colcuts[j])
      upper <- c(rowcuts[i+1], colcuts[j+1])
      prob <- pmvnorm(lower, upper, 0, corr)[1]   # integrating bivariate normal between lwoer and upper
      X12exact[i,j] <- prob
    
    }
  }
  
  X12exact.ca <- ca(X12exact)
  results[ind,1] <- X12exact.ca$sv[1]^2           # first eigenvalue    
  results[ind,2] <- sum(X12exact.ca$sv[-1]^2)     # sum of other eigenvalues
  results[ind,3] <- sum(X12exact.ca$sv^2)         # total inertia
  
}

par(mar=c(4.2,4,1,1), mgp=c(2,0.7,0), font.lab=2, mfrow = c(3,1))
plot(results[,1], 100*results[,1]/results[,3], xlim=c(0.102, 0.1111), xlab="first eigenvalue", ylab="% of total inertia")
abline(v=0.3333^2, col="gray", lty=2, lwd=2)

plot(results[,1], results[,3], xlim=c(0.102, 0.1111), xlab="first eigenvalue", ylab="total inertia")
abline(v=0.3333^2, col="gray", lty=2, lwd=2)

plot(results[,1], results[,3], xlim=c(0.102, 0.1111), xlab="first eigenvalue and inertia/(1+inertia)", ylab="total inertia")
points(results[,3]/(1+results[,3]), results[,3], col="red")
abline(v=0.3333^2, col="gray", lty=2, lwd=2)
