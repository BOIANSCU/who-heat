######### Rate Ratio (RR)
# The rate Ratio is used to measure the extent to which one group is relatively better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########


which.hilo <- function(x, rankorder=NULL, maxopt=NULL){
  # Work out which of two values represents the 'High' and 'Low' rate; given ordered and unordered groups
  if(is.rank(rankorder)){  # Identify most and least advantaged group
    g1 <- which(rankorder==min(rankorder))
    g2 <- which(rankorder==max(rankorder))
    if(maxopt[1]==0){
      if(x[g2]==0){
        return(NULL)
      }
      lo.rate <- g2
      hi.rate <- g1
    } else if(maxopt[1]==1){
      lo.rate <- g1
      hi.rate <- g2
    }
  } else {  # Identify best off and worst off group on the health indicator
    g1 <- which(x==min(x))
    g2 <- which(x==max(x))
    if(x[g1]==0){
      return(NULL)
    }
    lo.rate <- g1
    hi.rate <- g2
  }  
  return(list(lo=lo.rate, hi=hi.rate))
}



rr <- function(x, se=-1, bs=F, rankorder=NULL, maxopt=1){
  # This function returns the difference (i.e., the range) between the maximum and minimum values in a vector of rates
  #
  # Usage
  # x -- a vector of numbers
  # returns the ratio of max(x) and min(x) ($rr) and the rate ratio standard error (se.rd)
  #    
  
  if(is.na(maxopt)){
   return(NULL)
  }
  
  if(any(is.na(se))){
    se <- -1
  }
  
  if(!is.numeric(x) | !is.numeric(se)){
    stop('This function operates on vector of numbers')
  }
    
  if(length(se)==1 & se[1]==-1){  # i.e., if there are no standard errors provided, make the se's=0
    se <- rep(0, length(x))
  }
    
  hilo <- which.hilo(x=x, rankorder=rankorder, maxopt=maxopt)
  if(is.null(hilo)){
    return(NULL)
  }
  inequal.rr <- x[hilo$hi]/x[hilo$lo]
  
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){
    rr.boot <- c()  # Start with an empty vector of estimated RRs
    for(i in 1:200){  # Run 200 bootstraps 
      rr.est <- rnorm(n=1, x[hilo$hi], se[hilo$hi])/rnorm(n=1, x[hilo$lo], se[hilo$lo])  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      rr.boot <- c(rr.boot, rr.est)  # calculate the RII on the new data
    } 
    se.boot <- sd(rr.boot)  # Estimate the standard error of RRR as the SD of all the bootstrap RRs 
  }
  
  
  
  # The SE of the Rate Ratio
  se.formula <- sqrt((1/(x[hilo$lo]^2))*((se[hilo$hi]^2)+(inequal.rr^2)*((se[hilo$lo]^2))))
  
  
#  Return the results as a list
  return(list(inequal.rr=inequal.rr, se.rr.boot=se.boot,  se.rr.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}
