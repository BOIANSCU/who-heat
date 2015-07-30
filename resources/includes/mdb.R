######### Mean Difference from the Best performing Subgroup (mdb)
## 
# 
#
#
#########

wrap.mdb <- function(x, w, maxopt=0){
  # (Weighted) Mean difference from the best performing subgroup 
  refx <- ifelse(maxopt==1, max(x), min(x))  # The reference is the most desirable outcome among the subgroups
  p <- w/sum(w)
  inequal.mdb <- sum(p * abs(refx-x))
  return(inequal.mdb)
}


mdb <- function(x, w=-1, se=-1, bs=F, maxopt=0){
  # This function returns the mean difference between each groups rate and the best performing group
  #
  # Usage
  # x -- a vector of numbers (the rate)
  # returns the mean difference between and the se
  #  
  if(any(is.na(w))){
    w <- -1
  }
  
  if(any(is.na(se))){
    se <- -1
  }
  
  if(!is.numeric(x) | !is.numeric(w) | !is.numeric(se)){
    stop('This function operates on vector of numbers')
  }
  
  if(length(w)==1 & w[1]==-1){  # i.e., if no population numbers are given assume each group has a n of 1,000
    w <- rep(1000, length(x))
  }
  
  if(length(se)==1){
    if(se==-1){  # i.e., if there are no standard errors provided, make the se's=0
      se <- rep(0, length(x))
    }
  }
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  
  inequal.mdb <- wrap.mdb(x, w, maxopt)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){
    mdb.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      mdb.boot <- c(mdb.boot, wrap.mdb(nx, w, maxopt))  # calculate the MDB on the new data
    } 
    se.boot <- sd(mdb.boot)  # Estimate the standard error of IDisp as the SD of all the bootstrap IDisp's 
  }
  
  se.formula <- NA  
  
  
  # Return the results as a list
  return(list(inequal.mdb=inequal.mdb, se.mdb.boot=se.boot,  se.mdb.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
