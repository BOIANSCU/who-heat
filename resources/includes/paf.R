######### Population Attributable Fraction (PAF)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

wrap.paf <- function(x, w, maxopt=F, rankorder, national_est=NULL){
  # The population attributable fraction
  
  if(is.null(national_est)){  # Us the weighted mean of the data if there is no national estimate
    pop.prop <- w/sum(w)  
    w.mean <- weighted.mean(x, pop.prop)
  } else {
    w.mean <- national_est
  }
  
  inequal.par <- wrap.par(x, w, maxopt, rankorder, national_est)
  
  inequal.paf <- (inequal.par/w.mean) * 100
  return(inequal.paf)
}


paf <- function(x, w=-1, se=-1, bs=F, maxopt=F, rankorder, national_est=NULL){
  # This function returns the percentage of the PAR over the population rate
  # Usage
  # x -- a vector of numbers
  # w -- population weights
  # se -- standard error
  # bs -- whether to use the bootstrap to calculate confidence intervals
  # maxopt -- the highest vakue is the optimum value
  # rankorder -- the rank order of the subgroups (0 mean no rank order)
  #
  # returns the percentage of the PAR over the population rate
  # 
  if(is.na(maxopt)){
    return(NULL)
  }
  
  if(all(rankorder>0) & all(!rankorder==1)){
    # The data are ordered by subgroup, but the base subgroup is missing
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
  
  if(length(se)==1){
    if(se==-1){  # i.e., if there are no standard errors provided, make the se's=0
      se <- rep(0, length(x))
    }
  }
  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ){
    stop('the rates, population-size, and standard errors must all be of the same length')
  }
  
  
  
  inequal.paf <- wrap.paf(x, w, maxopt, rankorder, national_est)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){  ## Problem with the bootstrap SE
    paf.boot <- c()  # Start with an empty vector of estimated PARs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      paf.boot <- c(paf.boot, wrap.paf(nx, w, maxopt, rankorder, national_est))  # calculate the PAR on the new data
    } 
    se.boot <- sd(paf.boot)  # Estimate the standard error of PAR as the SD of all the bootstrap PARs 
  }
  
  if(length(se)==length(x)){
    se.formula <- NA
  }
  else{
    se.formula <- NA    
  }
  
  
  
  
  # Return the results as a list
  return(list(inequal.paf=inequal.paf, se.paf.boot=se.boot,  se.paf.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
