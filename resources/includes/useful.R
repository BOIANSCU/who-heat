#############################################################################
#
#
############################################################################
require(stringr)

my.which.min <- function(lizt, na.rm=F){
  # Which elements of lizt are minimum values
  # The problem with the original which.min() is that it only returns the position
  # of the first occuring minimum value.  so in c(1,2,3,1), which.min returns 1, 
  # my.which.min returns 1 and 4
  # na.rm: remove missing values.  If they exist and na.rm=F, return(NA)
  
  if(na.rm==F & sum(is.na(lizt))!=0){
    return(NA)
  }
  min.lizt <- min(lizt, na.rm=T)
  which(min.lizt==lizt)
}


nearest <- function(anchor, lizt, limit=1, all=F){
  # anchor: the number against which nearness is tested
  # lizt: the vector of candidate numbers
  # limit: the limit on how far away the number can be
  print('----nearest()-------')
  if(!is.na(anchor) & !is.null(lizt)){
    
    anchor <- as.integer(anchor)
    diff <- abs(anchor - lizt)  # The absolute difference between the anchor and the lizt
    if(min(diff, na.rm=T) > limit){  # If no number in lizt is nearer to anchor than limit ... fail (FALSE)
      return (FALSE)
    }
    else{
      locasi <- my.which.min(diff, na.rm=T)
      if(length(locasi)>1 & all==T){  # If there is more than one candidate and 'all'==T ...
        return(locasi)  # Return all
      }
      else{  # Otherwise ...
        return(locasi[1])  # Return the first
      }
    }
    print ('Why am I here?')
    return(NULL) 
  } else {
    return(NULL)
  }
}

resetAll <- function(){
  
} 


findCommon <- function(vectora, vectorb){
  # Find the health indicators that are common to every year.
  # vectora: the list of heath indicators
  # vectorb: the years in which the indicators occur
  if(length(vectora)!=length(vectorb)){
    stop("Vectors must be of the same length")
  }
  names(which(rowSums(table(vectora, vectorb))==length(unique(vectorb)))) 
}






geoOnly <- function(DF){
  # A function which determines whether geography is on eof the included dimensions of interest
  # returns T/F
  
  the_dimensions <- unique(DF$dimension)
  
  return("Geographic region" %in% the_dimensions & 
           sum(c("Economic status", "Mother's education", "Place of residence", "Sex") %in% the_dimensions)==0)
}



