# Inertia computations
# Tuomo A. Nieminen 2017

##################################################
# Compute inertia summaries for tables of sizes:
# 3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000
##################################################

# compute results and save
# -----------------------
source("compute_inertias.R")
# categories <- c(3:9, seq(10, 90, 10), seq(100, 1000, 100))
# inertias <- compute_inertias(categories)
# save(file = "data/inertias.Rda", inertias)

# Some plots of results
# -------------------
source("plotting.R")
inertias <- get(load("data/inertias.Rda"))

### Diagonals of the result matrices
sq_first <- diag(inertias$first) # first eigenvalues
sq_total <- diag(inertias$others) + sq_first # sum of eigenvalues
plot_inertias(sq_first, sq_total, r = 1/3)

### Contour plot of first inertias of rectangular tables
first <- inertias$first
contour(first, main = "Countour plot of first inertias of rectangular tables", sub = paste(colnames(first), collapse = " "))


########################################
# Powers of two
# --------------
########################################

# Compute the 2048 x 2048 table
source("compute_inertias.R")
# size <- 2^11
# M <- compute_binorm_table(rowcat = size, colcat = size, corr = matrix(c(1, -1/3, -1/3, 1), ncol = 2))
# save(file = "data/2048Matrix.Rda", M)

# load the 2048 x 2048 frequency matrix (2^11)
M <- get(load("data/2048Matrix.Rda"))

# repeadetly collapse the table and compute all rectangular tables (for powers 11:1)
# and their inertia summaries
summaries <- data.frame()
matrices <- list()

for(p1 in 11:1) {
  m <- M
  for(p2 in p1:1) {
    cat("Computing summaries for table ", dim(m), "\n")
    matrices <- append(matrices, list(m))
    summaries <- rbind(summaries, compute_ca_summaries(m))
    m <- collapse_table(m, by = "col")
  }
  if(p1 == 1) break
  M <- collapse_table(M, by = "row")
  M <- collapse_table(M, by = "col")
}

# check that the last table is as it should
P <- compute_binorm_table(2, 2, corr=matrix(c(1,-1/3, -1/3, 1), ncol = 2))
M;P # ok!

# set names
dims <-  unlist(sapply(11:1, function(p) paste0(2**p,"x", 2**(p:1))))
rownames(summaries) <- dims
matrices <- setNames(matrices, dims)

# save results
save(file = "data/power_results2.Rdata", summaries, matrices)

##############################
# Instructions for power_results2.Rdata usage
##############################
# load data
rm(list = ls())
load("data/power_results2.Rdata")
dimnames(summaries)
str(matrices)
M <- matrices[["8x4"]]
##############################
