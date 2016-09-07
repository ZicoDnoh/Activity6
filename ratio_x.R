#----------------------------------------------------------#
## Function 1: get ratio of Xcandidate and Xcurrent
#----------------------------------------------------------#
ratio.X <- function(log = TRUE, x01, x02, x1, x2, p, alpha, nt_1, nt_2, N1, N2, 
                    E1, E2, f, T) {
  
  if (all(x1 == x2)) { #if there is no change in X candidate
    result <- 0
  } else {
    
    #norm term
    prod.x <- apply(cbind(c(x01, x1), c(x02, x2)), 1, function(a) prod((min(a)+1):max(a)))
    x.frac <- sum(log(prod.x[c(x01, x1) - c(x02, x2) > 0])) - 
    		  sum(log(prod.x[c(x01, x1) - c(x02, x2) < 0]))
    
    prod.N <- c(1, 1, prod((min(N2, N1) + 1): max(N2, N1)))
    N.frac <- sum(log(prod.N[c(1, -1, N2 - N1) > 0])) - 
    		  sum(log(prod.N[c(1, -1, N2 - N1) < 0]))
    
     
    #p term
    p.term <- sum( (nt_2$nt - nt_1$nt) * log(p) ) +
              sum( (N2 - N1 - nt_2$nt + nt_1$nt) * log(1 - p) )
    
    #alpha term
    alpha.term <- (nt_2$nt1.sum - nt_1$nt1.sum) * log(alpha) +
                  (nt_2$nt2.sum - nt_1$nt2.sum) * log(1 - alpha)
    
    #porposal dens term
    jump.term <- sum(log(dbinom(E1, f[2^(1:T - 1)], 1-alpha))) -
                 sum(log(dbinom(E2, f[2^(1:T - 1)], 1-alpha)))
    
    result <- x.frac + N.frac + p.term + alpha.term + jump.term
    
  }
  
  #return the ratio
  if (log) {return(result)}
  else {return(exp(result))}
  
}