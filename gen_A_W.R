#----------------------------------------------------------#
## Function 1: generate AL (list of all latent histories)
#----------------------------------------------------------#
gen.AL <- function(T) {
    
  #generate all latent histories in order of indices
  AL <- expand.grid(rep(list(0:2), times = T))
  
  #return A
  return(AL)
  
}

#----------------------------------------------------------#
## Function 2: generate W: indices for latent hsitories
##             with 0 at t and no 2 thereafter
#----------------------------------------------------------#
gen.W <- function(T, AL) {
  
  #initialize W
  W <- list()
  
  for (t in 1:T) {
    
    #identify latent histories with no capture at the occasion t and no 2's thereafter
    if (t == T) {
    	W[[t]] <- which(AL[,t] == 0)
    } else if (t == T - 1) {
    	W[[t]] <- intersect(which(AL[,t] == 0), which(AL[T] < 2))
    }
    else {
      W[[t]] <- intersect(which(AL[,t] == 0),
                       which(apply(AL[,(-t:-1)], 1, max) < 2))
    }
    
  } #end of for loop
  
  #return list W
  return(W)
  
}