######### Population Attributable Risk (PAR)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

wrap.par <- function(x, w, maxopt=F, rankorder, national_est=NULL){
  # The population attributable fraction
  # For ranked subgroups:
  # par is the difference between the overall average and the reference subgroup
  # For unranked subgroups:
  # par is the difference between the overall average and the worst off subgroup
  #
  # If the data subgroups are ranked but the reference siubgroup is missing, then it is treated as unranked subgroups
  
  if(is.null(national_est)){  # Calculate the population average from the data if a national average is unavailable
    pop.prop <- w/sum(w)
    w.mean <- weighted.mean(x, pop.prop)
  } else {
    w.mean <- national_est
  }
  
  if(all(rankorder>0) & any(rankorder==1)){  # The data are ranked by subgroup and there is a base group (==1)
    refx <- x[rankorder==max(rankorder)]  # The reference group is the highest ranked subgroup
  } else {
    if( maxopt==T ){  # The data are NOT ranked by subgroup and the maximum value is optimum
      refx <- max(x)
    } else {  # The data are NOT ranked by subgroup and the minimum value is optimum
      refx <- min(x)
    }
  }
  
  inequal.par <- refx - w.mean
  
  if( maxopt==T & inequal.par < 0){ 
    inequal.par <- 0
  }
  if( maxopt==F & inequal.par > 0){ 
    inequal.par <- 0
  }
  
  
  return(inequal.par)
}

               
PAR <- function(x, w=-1, se=-1, bs=F, maxopt=F, rankorder, national_est=NULL){
  # This function returns the difference between the population rate and the most desirable group rate
  # Usage
  # x -- a vector of numbers
  # w -- population weights
  # se -- standard error
  # bs -- whether to use the bootstrapo to calculate confidence intervals
  # refx -- the reference category
  # national_est -- the national average for the indicator of interest
  # returns the difference between the reference group and the mean
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
  
  inequal.par <- abs(wrap.par(x, w, maxopt, rankorder, national_est))
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){  ## Problem with the bootstrap SE
    par.boot <- c()  # Start with an empty vector of estimated PARs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      par.boot <- c(par.boot, wrap.par(nx, w, maxopt, rankorder, national_est))  # calculate the PAR on the new data
    } 
    se.boot <- sd(par.boot)  # Estimate the standard error of PAR as the SD of all the bootstrap PARs 
  }
  
  if(length(se)==length(x)){
    pop.prop <- w/sum(w)  
    if(is.null(national_est)){
      w.mean <- weighted.mean(x, pop.prop)
    } else {
      w.mean <- national_est
    } 
    
    if(all(!rankorder==0)){  # The data are ranked by subgroup 
      refx <- x[rankorder==1]
    } else {
      if( maxopt==T ){  # The data are NOT ranked by subgroup and the maximum value is optimum
        refx <- max(x)
      } else {  # The data are NOT ranked by subgroup and the minimum value is optimum
        refx <- min(x)
      }
    }
    
    # The xls formula for the se component of each group's estimate:
    #     =SQRT(SUM(BL1682:BL1686) + (((OFFSET(G1682,MATCH(MIN(G1682:G1686),G1682:G1686,0)-1,1)))^2))
    # Element 1: each group rate multiplied by the squared difference between the group rate and the mean
    # SUM(BL1682:BL1686)
    el1 <- pop.prop * (x - w.mean)^2
    # Element 2: Squared SE for the reference group SE 
    # (((OFFSET(G1682,MATCH(MIN(G1682:G1686),G1682:G1686,0)-1,1)))^2))
    el2 <- (se[which(x == refx)[1]])^2  # 
    # Return the combined se elements
    se.formula <- sqrt(sum(el1) +  el2)
  }
  
  # Return the results as a list
  #  print(list(inequal.par=inequal.par, se.par.boot=se.boot,  se.par.formula=se.formula))
  return(list(inequal.par=inequal.par, se.par.boot=se.boot,  se.par.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
