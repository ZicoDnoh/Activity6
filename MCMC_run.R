source(full_cond.R)
source(count.R)
source(ratio_x.R)

#----------------------------------------------------------#
## Function 1: MCMC algorithm
#----------------------------------------------------------#
run_MCMC <- function(niter, prior.alp.vec, prior.beta.vec, num.sample, all.lhist, Wlist, obs.freq) {
  
  #initialize the result matrix
  theta.mat <- matrix(rep(0, niter * (num.sample + 2)), nrow = niter, ncol = num.sample + 2)
  
  #initialize x current
  x.initial <- rep(0, 3^num.sample - 1)
  x.initial[which(apply(all.lhist[-1,], 1, max) < 2)] <- obs.freq
  
  #initial values
  p.cur <- rep(0.5, num.sample)
  x0.cur <- 200
  x.cur <- x.initial
  N.cur <- x0.cur + sum(x.cur)
  nt.cur <- count.nt(x = x.cur, AL = all.lhist, T = num.sample)
  alpha.cur <- 1
  theta.mat[1,] <- c(p.cur, N.cur, alpha.cur) #order: p1,...,pT,N,alpha
  
  accept <- 0
  #main MCMC loop
  for (iter in 2:niter) {
    
    message(iter)
    
    #part 1: update x0 and so N, nt, nt1 and nt2
    x0.cur <- full.cond.x0(x = x.cur, p = p.cur)
    N.cur <- x0.cur + sum(x.cur)
  	  
    #part 2: update p1,...,pt
    p.cur <- full.cond.p(prior.alp = prior.alp.vec[-1], prior.beta = prior.beta.vec[-1], 
                         T = num.sample, n = nt.cur$nt, N = N.cur)
    
    #part 3: update alpha
    alpha.cur <- full.cond.alpha(prior.alp = prior.alp.vec[1], prior.beta = prior.beta.vec[1], 
                                 sum.nt1 = nt.cur$nt1.sum, sum.nt2 = nt.cur$nt2.sum)
    
    #part 4: update x
    #generate number of errors
    E.cur <- full.cond.E(alpha = alpha.cur, f = obs.freq, T = num.sample)
    
    #generate x candidate
    x0.x.can <- gen.x(E = E.cur, T = num.sample, AL = all.lhist, x = x.initial, 
                   x0 = x0.cur, W = Wlist)
    x0.can <- x0.x.can$x0
    x.can <- x0.x.can$x
    N.can <- x0.can + sum(x.can)
    nt.can <- count.nt(x = x.can, AL = all.lhist, T = num.sample)
    
    #get MH ratio
    log.r <- ratio.X(log = TRUE, x01 = x0.cur, x02 = x0.can, x1 = x.cur, x2 = x.can, 
                     p = p.cur, alpha = alpha.cur, nt_1 = nt.cur, nt_2 = nt.can, N1 = N.cur, N2 = N.can)
    
    #make decision for x candidate
    if (!is.na(log.r) && runif(1) < exp(log.r)) { #if we accept x candidate
      
      x0.cur <- x0.can
      x.cur <- x.can
      N.cur <- N.can
      nt.cur <- nt.can
      
      accept <- accept + 1
    }
    
    #update the result matrix
    theta.mat[iter,] <- c(p.cur, N.cur, alpha.cur)
    
  } #end of for loop
  
  #return MCMC result matrix
  return(theta.mat)
  
}