######### Rate Difference (RD)
# The rate difference is used to measure the extent to which one group is better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########


rd <- function(x, w=-1, se=-1, bs=F, rankorder=NULL){
  # This function returns the difference (i.e., the range) between the maximum and minimum values in a vector of rates
  #
  # Usage
  # x -- a vector of numbers
  # rankorder -- for rd, if subgroups are oderable compare highest and lowest subgroup, otherwise max against min
  # maxopt -- if higher indicators are better, maxopt is 1, if lower is better, 0
  # returns the difference between max(x) and min(x) ($rd) and the rate difference standard error (se.rd)
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
  
  
  if(is.rank(rankorder)){  # Identify most and least advantaged group
    g1 <- which(rankorder==min(rankorder))
    g2 <- which(rankorder==max(rankorder))
  } else {  # Identify best off and worst off group on the health indicator
    g1 <- which(x==min(x))
    g2 <- which(x==max(x))
  }
 
  inequal.rd <- x[g2] - x[g1]    # The Rate Difference
  
  
  
  # The SE of the Rate Difference
  lo.rate <- x[g2]  # Identify the element of x that is the low rate
  hi.rate <- x[g1]  # Identify the element of x that is the high rate
  se.formula <- sqrt( se[g2]^2 + se[g1]^2 )  # The SE of the difference is the squareroot of the sum of the squared SEs     
  
  # Boot strap SE
  se.boot <- NA
  if(bs==T){
    se.boot <- c()  # Start with an empty vector of estimated RDs
    for(i in 1:200){  # Run 200 bootstraps 
      boot.rd <- abs(rnorm(n=1, lo.rate, se[g2]) - rnorm(n=1, hi.rate, se[g1]))  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      se.boot <- c(se.boot, boot.rd)  # calculate the MLD on the new data
    } 
    se.boot <- sd(se.boot)  # Estimate the standard error of MLD as the SD of all the bootstrap MLDs 
  }
  
  # Return the results as a list
  return(list(inequal.rd=inequal.rd, se.rd.boot=se.boot, se.rd.formula=se.formula))
}



