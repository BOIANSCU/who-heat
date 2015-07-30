######### Mean Log Deviation (MLD)
# Mean Log Deviation is a measures of general disproportionality, developed 
# by the economist Henri Theil (10)
# 
# 
#
#
#########

wrap.mld <- function(x, w, national_est=NULL){
  # The Mean Log Deviation (MLD)
  if(is.null(national_est)){  # Calculate the population average from the data if a national average is unavailable
    prop.pop <- w/sum(w)  # Calculate proportion of the population in each group
    meanx <- weighted.mean(x, prop.pop)  # Calculate the weighted mean 
  } else {
    meanx <- national_est
  }
  
  # Protect against log(0) by adding a tiny positive value to any 0
  x[which(x==0)]  <- .000001
  
  
  pop.prop <- w/sum(w)  
  rj <- x/meanx
  inequal.mld <- sum(pop.prop * -log(rj))
  return(inequal.mld)
}


mld <- function(x, w=-1, se=-1, bs=F, national_est=NULL){
  # This function returns the difference (i.e., the range) between the maximum and minimum values in a vector of rates
  #
  # Usage
  # x -- a vector of numbers
  # returns the difference between max(x) and min(x) ($rd) and the rate difference standard error (se.rd)
  #  
  if(any(is.na(x))){
    return(NULL)
  }
  
  
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
  
  if(length(se)==1 & se[1]==-1){
      se <- rep(0, length(x))
  }
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
      
  
  inequal.mld <- wrap.mld(x, w)
  
  #   # Bootstrap SE  DISABLED
  #   # The bootstrap for MLD is not quite right.  The process keeps looping if a negative value is generated
  #   # because log(-ve) is NaN
  #   if(bs==T){
  #     mld.boot <- c()  # Start with an empty vector of estimated BGVs
  #     for(i in 1:200){  # Run 200 bootstraps 
  #       nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
  #       mld.boot <- c(mld.boot, wrap.mld(nx, w))  # calculate the MLD on the new data
  #     } 
  #     se.boot <- sd(mld.boot)  # Estimate the standard error of MLD as the SD of all the bootstrap MLDs 
  #   }
  #   else{
  #     se.boot <- NA
  #   }
  se.boot <- NA
  
  # The SE of MLD
  
  if(length(se)==length(x)){
    # The xls formula for the se component of each group's estimate:
    # =(((H2^2)*(Y2^2))/(SUMPRODUCT(G2:G6,Y2:Y6)^2))*((1-(1/CF2))^2)
    wgt.mean <- weighted.mean(x, w) # Calculate the weighted mean, because it is re-used
    
    # Element 1: ((H2^2)*(Y2^2))
    pop.prop <- w/sum(w) # Y is the proportion weight of each group
    el1 <- se^2 * pop.prop^2 # H is the vector of se's
    
    # Element 2: (SUMPRODUCT(G2:G6,Y2:Y6)^2))  # The sumproduct is the weighted mean
    el2 <- (wgt.mean)^2  # square of the weighted mean
    # Element 3: ((1-(1/CF2))^2)  # CF2 is the ratio of each x/weighted.mean(x)
    rj <- x/wgt.mean
    el3 <- (1-(1/rj))^2
    # Element 4: combine elements 1..3 according to the xls formula
    el4 <- (el1/el2)*el3
    # Return the combined se elements
    se.formula <- sqrt(sum(el4))   
  }
  else{
    print(length(x))
    print(length(se))
    print(length(se)==length(x))
    se.formula <- NA
  }
  if(is.nan(se.formula)){
    se.formula <- NA
  }
  
  # Return the results as a list
  return(list(inequal.mld=inequal.mld, se.mld.boot=se.boot,  se.mld.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
