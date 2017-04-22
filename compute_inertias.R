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
#' compute_inertias(3:5)
#' 
compute_inertias <- function(categories, r  = 1/3) {
  
  N <- length(categories)
  
  # store results in matrices  
  m1 <- matrix(0, nrow = N, ncol = N)
  rownames(m1) <- colnames(m1) <- categories
  m4 <- m3 <- m2 <- m1
  
  # bivariate correlation matrix
  corr <- matrix(c(1, r, r, 1), ncol = 2)
  
  # use symmetry and compute only half of the result matrices
  for(r in 1:N) {
    for(c in r:N) { # start from current r
      
      rowcat <- categories[r]
      colcat <- categories[c]
      rowcuts <- qnorm(seq(0,1,length=rowcat+1))
      colcuts <- qnorm(seq(0,1,length=colcat+1))
      
      # a matrix for the frequency table
      X12exact <- matrix(0, nrow=rowcat, ncol=colcat)
      
      # update progress on the console
      prc_done <- round(100*(sum(categories[1:r]) - r*(N - c)/N) / sum(categories))
      cat("\r Computing results for table", rowcat, "x", colcat, "(done:", prc_done, "%)  ")
      
      # use symmetry and compute only half of the bivariate normal probabilities
      for(i in 1:rowcat) {
        for(j in i:colcat) { # start from current i
          lower <- c(rowcuts[i], colcuts[j])
          upper <- c(rowcuts[i+1], colcuts[j+1])
          # integrate bivariate normal between lower and upper
          prob <- mvtnorm::pmvnorm(lower, upper, 0, corr)[1]  
          X12exact[i,j] <- X12exact[rowcat-i + 1, colcat-j + 1] <- prob # symmetry
        }
      }
      
      # results of CA
      X12exact.ca <- ca::ca(X12exact)
      m1[r, c] <- m1[c, r] <- X12exact.ca$sv[1]^2           # first eigenvalue 
      m2[r, c] <- m2[c, r] <- sum(X12exact.ca$sv[-1]^2)     # sum of other eigenvalues
      m3[r, c] <- m3[c, r] <- var(X12exact.ca$sv^2)         # variance of eigenvalues
      m4[r, c] <- m4[c, r] <- e1071::skewness(X12exact.ca$sv^2) # skewness
      
    }
  }
  cat("\n") # linechange on the console
  list(first = m1, others = m2, variance = m3, skewness = m4)
}

# categories <- c(3:9, seq(10, 90, 10), seq(100, 1000, 100))
# inertias <- compute_inertias(categories)
# save(file = "inertias.Rda", inertias)
