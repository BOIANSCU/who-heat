######### Slope Index of Inequality (SII)

wrap.sii <- function(y, w){
  # Slope Index of Inequality wrapper  
  x <- midPointProp(w)
  if(x[1] == 'Fail'){  # 
    return(x)
  }
  inequal.sii <- lm(y~x, weights=w)$coefficients[2]
  names(inequal.sii) <- NULL
  
  ### An alternative coding from Sam Harper producing identical results
  #   x <- midPointProp(w)
  #   prop <- (w/sum(w))
  #   
  #   numerator <- sum(prop*x*y) - (sum(prop*x) * sum(prop*y))
  #   denominator <- sum(prop * x^2) - ((sum(prop * x))^2)
  #   inequal.sii <- numerator/denominator
  
  return(inequal.sii)
}


sii <- function(y, w=-1, se=-1, bs=F, rankorder=NULL, maxopt=1){
  # This function returns the slope index of inequality and is calculated as the beta-coefficient in the regression
  # of the mean health variable on the mean relative rank variable.:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  # rankorder -- for sii, the subgroups must be oderable (e.g., quintiles of wealth)
  # maxopt -- if higher indicators are better, maxopt is 1, if lower is better, 0
  # returns the beta-coefficient and its standard error.
  #  
  if(is.na(maxopt)){
    return(NULL)
  }
  
  if(!is.rank(rankorder)){  # If these are not rankordered, then SII does not apply
    return(NULL)
  }
  
  
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
  
  
  if(maxopt == 0){  # If decreasing indicator is better (e.g., U5MR)
    decrease <- T
  }
  if(maxopt == 1){  # If increasing indicator is better (e.g., Antenatal visits)
    decrease <- F
  }
  
  # Make sure the vectors are ordered from appropriately, given maxopt
  y <- y[order(rankorder, decreasing=decrease)]
  w <- w[order(rankorder, decreasing=decrease)]
  se <- se[order(rankorder, decreasing=decrease)]
  rankorder <- rankorder[order(rankorder, decreasing=decrease)]
  
  inequal.sii <- wrap.sii(y, w)
  
  
  # Bootstrap SE
  if(bs==T){
    sii.boot <- c()  # Start with an empty vector of estimated SIIs
    for(i in 1:200){  # Run 200 bootstraps 
      ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      sii.boot <- c(sii.boot, wrap.sii(ny, w))  # calculate the SII on the new data
    } 
    se.boot <- sd(sii.boot)  # Estimate the standard error of SII as the SD of all the bootstrap SIIs 
  }
  else{
    se.boot <- NA
  }
  
  #se.boot <- NA
  
  se.formula <- NA
  if(!all(se==0)){
    # Formula-based SE: provided by Sam Harper
    p <- w/sum(w)
    x <- midPointProp(w)
    
    se.formula <- sqrt((sum(p^2 * x^2 * se^2)+((sum(p * x)^2)*sum(p^2 * se^2))-(2*sum(p * x)*sum(p^2 * x * se^2)))/((sum(p * x^2)-(sum(p * x)^2))^2))
  }
  
  # Return the results as a list
  return(list(inequal.sii=inequal.sii, se.sii.boot=se.boot,  se.sii.formula=se.formula))  # return a list of the inequality measure and the standard error 
}
