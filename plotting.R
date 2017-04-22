
#' Plot of inertias
plot_inertias <- function(first, total, r = 1/3) {
  par(mar=c(4.2,4,1,1), mgp=c(2,0.7,0), font.lab=2, mfrow = c(2,2))
  
  plot(first, 100*first/total, xlim=c(0.102, 0.1111), xlab="first eigenvalue",  ylab="% of total inertia")
  abline(v=r^2, col="gray", lty=2, lwd=2)
  
  plot(first, total, xlim=c(0.102, 0.1111), xlab="first eigenvalue", ylab="total inertia")
  abline(v=r^2, col="gray", lty=2, lwd=2)
  
  plot(first, total, xlim=c(0.102, 0.1111), xlab="first eigenvalue and inertia/(1+inertia)", ylab="total inertia")
  points(total/(1+total), total, col="red")
  abline(v=r^2, col="gray", lty=2, lwd=2)
}


#' 3D plot
#'
#' Under construction
#' 
name <- function(variables) {
  # see rgl.demo.bivar
  r <- 0.9
  corr <-  matrix(c(1, r, r, 1), ncol = 2)
  x <- y <- seq(-4,4,length = 1000)
  f <- function(x, y) mvtnorm::dmvnorm(matrix(c(x, y), ncol = 2), sigma = corr)
  z <- outer(x, y, f)
  rgl::persp3d(x, y, z,  aspect = c(1, 1, 0.5), col = "lightblue", xlab = "X", ylab = "Y", zlab = "binorm")  
}
