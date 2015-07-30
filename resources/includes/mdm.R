######### Mean Difference from the Mean (mdm)
##
# 
#
#
#########

wrap.mdm <- function(x, w, national_est=NULL){
  # Mean difference from the best performing subgroup 
  p <- w/sum(w)  # Propo
  if(is.null(national_est)){  # Calculate the population average from the data if a national average is unavailable
    p <- w/sum(w)
    w.mean <- weighted.mean(x, p)
  } else {
    w.mean <- national_est
  }
 
  w.mean <- sum(p * x)  # Weighted mean
  inequal.mdm <- sum(p * abs(x - w.mean))
  return(inequal.mdm)
}


mdm <- function(x, w=-1, se=-1, bs=F, national_est=NULL){
  # This function returns the mean difference between each groups rate and the mean
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
  
  
  inequal.mdm <- wrap.mdm(x, w)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){
    mdm.boot <- c()  # Start with an empty vector of estimated MDMs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      mdm.boot <- c(mdm.boot, wrap.mdm(nx, w))  # calculate the MDBPS on the new data
    } 
    se.boot <- sd(mdm.boot)  # Estimate the standard error of IDisp as the SD of all the bootstrap IDisp's 
  }
  
  se.formula <- NA  
  
  
  # Return the results as a list
  return(list(inequal.mdm=inequal.mdm, se.mdm.boot=se.boot,  se.mdm.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
