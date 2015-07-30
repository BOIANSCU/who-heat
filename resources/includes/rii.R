######### Relative Index of Inequality (RII)


wrap.rii <- function(y, w){
  # Relative Index of Inequality wrapper  
  prop.pop <- w/sum(w)
  inequal.sii <- wrap.sii(y, w)
  inequal.rii <- inequal.sii/(sum(prop.pop * y))
  return(inequal.rii)
}


rii <- function(y, w=-1, se=-1, bs=F, rankorder=NULL, maxopt=1){
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
  
  if(!is.rank(rankorder)){  # If these are not rankordered, then RII does not apply
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
  
  if(all(w==0)){  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    w <- rep(1, length(w))
  }
   
  if(length(se)==1 & se[1]==-1){  # i.e., if there are no standard errors provided, make the se's=0
    se <- rep(0, length(y))
  }
  
  if( !(length(y) == length(w)) | !(length(y)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
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
  
  
  inequal.rii <- wrap.rii(y, w)
  
  
  # Bootstrap SE
  if(bs==T){
    rii.boot <- c()  # Start with an empty vector of estimated BGVs
    for(i in 1:200){  # Run 200 bootstraps 
      ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      rii.boot <- c(rii.boot, wrap.rii(ny, w))  # calculate the RII on the new data
    } 
    se.boot <- sd(rii.boot)  # Estimate the standard error of RII as the SD of all the bootstrap RIIs 
  }
  else{
    se.boot <- NA
  }
  

  # Formula-based SE: provided by Sam Harper
  prop.pop <- (w/sum(w))
  midpoint <- midPointProp(w)
  weighted_mu <- sum( prop.pop * y)
  pXy <- (y * prop.pop * midpoint)
  pX_mu <- prop.pop* midPointProp(w) * weighted_mu
  psum__pXy__ <-  prop.pop * sum( pXy )
  
  pX_mu__psum__pXy__ <- se^2 * (pX_mu - psum__pXy__)^2 
  pX2 <- prop.pop * midpoint^2
  pX <- prop.pop * midpoint
  
  se.formula <- (sqrt((sum(pX_mu__psum__pXy__)/(((weighted_mu^4)*((sum(pX2)-(sum(pX)^2))^2))))))
  
  # Return the results as a list
  return(list(inequal.rii=inequal.rii, se.rii.boot=se.boot,  se.rii.formula=se.formula))  # return a list of the inequality measure and the standard error 
}





