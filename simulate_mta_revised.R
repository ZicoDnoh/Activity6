## Load Packages
library(dplyr)

## Set seed so the results are constant
set.seed(8888)

## Generate parameters

N <- 1000                               # Population size
T <- 3                                 # Number of occasions
p <- rbeta(T,2,4)                       # Capture probabilities
alpha <- .9

## Simulate true capture histories
Wtrue <- sapply(1:T,function(t){
    (runif(N) < p[t]) * 2^(runif(N) < (1-alpha))
})

## Convert to observed histories

## 1) Add new rows for errors
W <- rbind(Wtrue,
           do.call("rbind",lapply(1:T,function(t){
               nerr <- sum(Wtrue[,t]==2)
               Wtmp <- matrix(0,nerr,T)
               Wtmp[,t] <- 1
               Wtmp
           })))

## 2) Remove unobserved individuals
W <- subset(W,apply(W,1,sum)>0)
index.no.2 <- which(apply(W, 1, function(r) any(r %in% c(2))) == FALSE)
W <- W[index.no.2,]

## Convert to indexes
index <- W %*% 2^(0:(T-1))

## Count number of individuals with each unique observed history
f.obs <- table(index)

## Create vector of counts
f <- rep(0,2^T-1)
f[as.integer(names(f.obs))] <-  f.obs

## Write data file
save(W,f,file="simulate_mta_data.RData")
