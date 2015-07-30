# Many of the calculation rely on knowing the cumnulative proportion of the population in the mid-point of each 
# ordered group.  That is the "Mid-Point Proportion".  This function returns that value.

midPointProp <- function(w){
  # This function returns the cumulative mid point proportion of each group:
  # Usage
  # w -- a vector of numbers of the population in each group
  # returns a vector representing the cumulative mid-point proportion
  #
  if(!is.numeric(w)){
    stop('This function operates on vector of numbers')
  }
  if(all(w==0)){
    stop('The population is of size 0 in all cells')
  }
  p <- w/sum(w)  # Calculate the pop. proportion in each group 
  p.mid <- p/2   # Calculate the mid-point proportion in each group
  p.cumsum <- cumsum(p) # Calculate the cumulative proprtion
  p.mid.cumsum <- p.mid + c(0, p.cumsum)[1:length(w)]  # Calculate the cumulative mid point proportion of each group
  return(p.mid.cumsum)  # Return the answer
}