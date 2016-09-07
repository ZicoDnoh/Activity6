#----------------------------------------------------------#
## Function 1: count nt, nt1, nt2 for given x
#----------------------------------------------------------#
count.nt <- function(x, AL, T) {
  
  #initialize nt1 and nt2
  nt1 <- c(); nt2 <- c()
  
  for (t in 1:T) {
    
    nt1[t] <- sum(x[which(AL[-1,t] == 1)]) #get nt1
    nt2[t] <- sum(x[which(AL[-1,t] == 2)]) #get nt1
    
  } #end of for loop
  
  nt <- nt1 + nt2 #get nt
  
  #return nt1, nt2 and nt
  result <- list()
  result$nt1.sum <- sum(nt1); result$nt2.sum <- sum(nt2); result$nt <- nt
  return(result)
  
}