#' Compute inertia summaries for different sized frequency tables
#' 
#' Computes inertia summaries for frequency tables of varying sizes, as defined by the categories argument
#' 
#' @param categories A vector of integers giving the numbers of row and column categories in the tables
#' @param r Correlation between the row and column variables. 
#' The default is 1/3.
#' 
#' @details The category frequencies are computed using a bivariate normal distribution 
#' with correlation equal to the r argument.
#' 
#' @return A list with 4 symmetric matrices:\ct 
#' - first: the first eigenvalues\ct
#' - other: sum of the remaining eigenvalues\ct
#' - variance: The variance of the eigenvalues\ct
#' - skewness: The skewness of the eigenvalues\ct
#' 
#' The row and column names of the matrices correspond to the number of categories in the tables.
#' 
#' @examples
#' 
#' # small example
#' compute_inertias(3:5)
#' 
#' # big example
#' categories <- c(3:9, seq(10, 90, 10), seq(100, 1000, 100))
#' inertias <- compute_inertias(categories)
compute_inertias <- function(categories,  r = 1/3) {
  
  N <- length(categories)
  
  # store results in matrices  
  m1 <- matrix(0, nrow = N, ncol = N)
  rownames(m1) <- colnames(m1) <- categories
  m4 <- m3 <- m2 <- m1
  
  # bivariate correlation matrix
  corr <- matrix(c(1, r, r, 1), ncol = 2)
  
  # use symmetry and compute only half of the result matrices
  for(nr in 1:N) {
    for(nc in nr:N) { # start from current nr
      
      rowcat <- categories[nr]
      colcat <- categories[nc]
      
      # update progress on the console
      prc_done <- round(100*(sum(categories[1:nr]) - nr*(N - nc)/N) / sum(categories))
      cat("\r Computing results for table", rowcat, "x", colcat, "(done:", prc_done, "%)  ")
      
      X12exact <- compute_binorm_table(rowcat, colcat, corr)
      
      # results of CA
      X12exact.ca <- ca::ca(X12exact)
      m1[nr, nc] <- m1[nc, nr] <- X12exact.ca$sv[1]^2           # first eigenvalue 
      m2[nr, nc] <- m2[nc, nr] <- sum(X12exact.ca$sv[-1]^2)     # sum of other eigenvalues
      m3[nr, nc] <- m3[nc, nr] <- var(X12exact.ca$sv^2)         # variance of eigenvalues
      m4[nr, nc] <- m4[nc, nr] <- e1071::skewness(X12exact.ca$sv^2) # skewness
      
    }
  }
  cat("\n") # linechange on the console
  list(first = m1, others = m2, variance = m3, skewness = m4)
}

#' compute a frequency table using the binormal distribution
#' 
#' @param rowcat The number of row categories
#' @param colcat The number of column categories
#' @param The correlation matrix for bivariate normal 
#' Default is 1/3 correlation
#' 
#' @return A matrix of size rowcat x colcat
#' 
#' @examples 
#' size <- 2^11
#' M <- compute_binorm_table(rowcat = size, colcat = size, corr = matrix(c(1, -1/3, -1/3, 1), ncol = 2))
compute_binorm_table <- function(rowcat, colcat, corr = matrix(c(1, 1/3, 1/3, 1), ncol = 2)) {
  
  rowcuts <- qnorm(seq(0,1,length=rowcat+1))
  colcuts <- qnorm(seq(0,1,length=colcat+1))
  
  # a matrix for the frequency table
  X12exact <- matrix(0, nrow=rowcat, ncol=colcat)
  
  # use symmetry and compute only half of the bivariate normal probabilities
  cat("\n")
  for(i in 1:rowcat) {
    for(j in i:colcat) { # start from current i
      lower <- c(rowcuts[i], colcuts[j])
      upper <- c(rowcuts[i+1], colcuts[j+1])
      # integrate bivariate normal between lower and upper
      prob <- mvtnorm::pmvnorm(lower, upper, 0, corr)[1]  
      X12exact[i,j] <- X12exact[rowcat-i + 1, colcat-j + 1] <- prob # symmetry
    }
  }
  X12exact
}

#' Compute CA summaries
compute_ca_summaries <- function(X) {
  X.ca <- ca::ca(X)
  data.frame("first" = X.ca$sv[1]^2, 
    "others" = sum(X.ca$sv[-1]^2),
    "variance" = var(X.ca$sv^2), 
    "skewness" = e1071::skewness(X.ca$sv^2))
}

#' collapse a matrix by summing every other row or column
#' 
#' @param X A matrix
#' @param by Collapse by rows or columns?
#' 
#' @examples 
#' X <- matrix(rep(2, 16), nrow = 4)
#' X
#' collapse_table(X)
#' collapse_table(X, by = "col")
#' 
#' X2 <- matrix(sample(1:100, 16), nrow = 4)
#' X2
#' collapse_table(X2)
#' collapse_table(X2, by = "col")
collapse_table <- function(X, by = "row") {
  if(by == "col") X <- t(X)
  nr <- nrow(X)
  if(nr %% 2 != 0) stop("X is not collapsable")
  even <- 2*(1:(nr/2))
  odd <- even -1
  X_new <- X[even, ] + X[odd, ]
  if(by == "col") return(t(X_new))
  X_new
}
