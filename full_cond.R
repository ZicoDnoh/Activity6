#----------------------------------------------------------#
## Function 1: generate p1,...,pT from each full conditional
#----------------------------------------------------------#
full.cond.p <- function(prior.alp, prior.beta, T, n, N) {
  
  # parameters for the full conditional
  new.alpha <- prior.alp + n
  new.beta <- prior.beta + N - n
  
  # generate new candidates for p1,...,pt
  result <- rbeta(T, new.alpha, new.beta)
  
  # return a vector of new p1,...,pT
  return(result)
  
}

#----------------------------------------------------------#
## Function 2: generate alpha from its full conditional
#----------------------------------------------------------#
full.cond.alpha <- function(prior.alp, prior.beta, sum.nt1, sum.nt2) {
  
  # parameters for the full conditional
  new.alpha <- prior.alp + sum.nt1
  new.beta <- prior.beta + sum.nt2
  
  # generate new candidate for alpha
  result <- rbeta(1, new.alpha, new.beta)
  
  #return a value of new alpha
  return(result)
  
}

#----------------------------------------------------------#
## Function 3: generate x0 from its full conditional
#----------------------------------------------------------#
full.cond.x0 <- function(x, p) {
  
  # parameters for the full conditional
  new.p <- 1 - prod(1 - p)
  
  # generate new candidate for x0
  result <- rnbinom(1, sum(x), new.p)
  
  # return a value of new x0
  return(result)

}

#----------------------------------------------------------#
## Function 4: generate Num. of errors for each occasion from
##             their full conditionals
#----------------------------------------------------------#
full.cond.E <- function(alpha, f, T) {
  
  #draw each number of errors
  result <- rbinom(T, f[2^(1:T - 1)], 1-alpha)
  
  #return a value
  return(result)
  
}

#----------------------------------------------------------#
## Function 5: generate x candidiate
#----------------------------------------------------------#
gen.x <- function(E, T, AL, x, x0, W) {
  
  x.full <- c(x0, x) #frequencies for the entire latent history
  for (t in 1:T) {
    
    #for the occasion t, we add up error
    for (l in 1:E[t]) {
      
      # we do not need this loop when E[t] == 0
      if (E[t] == 0) break
      
      #STEP 1: select a history from tmp (no capture at t and no 2's thereafter)
      j <- sample(intersect(W[[t]], which(x.full > 0)), 1) #index of selected history
      
      #STEP 2: identify corresponding history if an error is added up
      new.hist <- AL[j,] #selected history
      new.hist[t] <- 2 #indetify history after adding one error
      k <- sum(new.hist * 3^(1:T - 1)) + 1 #index of indetified history
      
      #STEP 3: adjust x
      x.full[3^(t-1) + 1] <- x.full[3^(t-1) + 1] - 1 #subtract 1 from original history
      x.full[j] <- x.full[j] - 1 #subtract 1 from selected history
      x.full[k] <- x.full[k] + 1 #add 1 to indentified history
      
    } #end of for(l)
    
  } #end of for(t)
  
  #return x0 value and x vector
  result <- list()
  result$x0 <- x.full[1]; result$x <- x.full[-1]
  return(result)
  
}
