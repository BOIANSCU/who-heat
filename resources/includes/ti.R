######### Theil Index
# http://en.wikipedia.org/wiki/Theil_index
#########


wrap.theil <- function(y, w){
  # Theil Index wrapper function
  # Protect against log(0) by adding a tiny positive value to any 0
  y[which(y==0)]  <- .000001
  
  prop.pop <- w/sum(w)
  mu <- sum(prop.pop * y)
  rj <- y / mu
  if(any(rj <=0)){
    return(NULL)
  }  
  inequal.ti <- sum(prop.pop * rj * log(rj))
  return(inequal.ti)
}


ti <- function(y, w=-1, se=-1, bs=F){ 
  # This function returns the Theil Index of inequality:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  #
  # Protect against log(0) by adding a tiny positive value to any 0
  y[which(y==0)]  <- .000001

  if(any(is.na(w))){
    w <- -1
  }
  
  if(any(is.na(se))){
    se <- -1
  }

  
  if(!is.numeric(y) | !is.numeric(w) | !is.numeric(se)){
    stop('This function operates on vector of numbers')
  }
  
  if(length(w)==1 & w[1]==-1){  # i.e., if no population numbers are given assume each group has a weight of 1
    w <- rep(1, length(y))
  }
  
  if(length(se)==1 & se[1]==-1){  # i.e., if there are no standard errors provided, make the se's=0
    se <- rep(0, length(y))
  }
  
  if( !(length(y) == length(w)) | !(length(y)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  if(all(w==0)){  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    w <- rep(1, length(w))
  }
  
  inequal.ti <- wrap.theil(y, w)
  
  
  # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
  prop.pop <- w/sum(w)
  mu <- sum(prop.pop * y)
  rj <- y / mu
  s_u <- prop.pop * rj * (1 + log(rj))
  ti.se.indiv <- ((1 + log(rj) - sum(s_u))^2)*((prop.pop^2)*(se^2)/(mu^2))
  se.formula <- sqrt(sum(ti.se.indiv))
  
  
  # Bootstrap SE
  if(bs==T){
    ti.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 1000 bootstraps
      ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=y, sd=se)
      ti.boot <- c(ti.boot, wrap.theil(ny, w))  # calculate the RCI on the new data
    } 
    se.boot <- sd(ti.boot)  # Estimate the standard error of RCI as the SD of all the bootstrap BGVs 
  }
  else{
    se.boot <- NA
  }
  return(list(inequal.ti=inequal.ti, se.ti.boot=se.boot, se.ti.formula=se.formula))
}
