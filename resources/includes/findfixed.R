findFixedFactors <- function(dataDF){
  # This function takes a dataframe, isolates 'country', 'year', indic', 'dimension'
  # For plotting purposes, onthly three dimnensions can be sensibly handled and the 
  # fourth needs to be fixed.  This function returns a list of fixed and variable columns
  
  # Fail if the dataftame is empty
  if(is.null(dataDF)){
    return(NULL)  # Change this later to a FAIL
  }
  
  # Select from the dataframe the four columns of interest
  country <- unique(dataDF[,'country'])   # return a list of the countries in the loaded data
  year <- unique(dataDF[,'year'])   # return a list of the years in the loaded data
  indic <- unique(dataDF[,'indic'])   # return a list of the equity indicators in the loaded data
  dimension <- unique(dataDF[,'dimension'])   # return a list of the equity indicators in the loaded data
  
  # Set up empty lists and vectors to store in the list results
  fixedList <- list()
  varList <- list()
  fcountry <- c()
  fyear <- c()
  findic <- c()
  fdimension <- c() 
  vcountry <- c()
  vyear <- c()
  vindic <- c()
  vdimension <- c()
  
  
  # Go through the unique values in each column of interest and make a list of those with
  # only a single unique value (a fixed factor) and those with many unique values (a variable factor)
  # Country
  if(length(country) == 1){
    fcountry <- c(country)
  }
  if(length(country) > 1){
    vcountry <- c(country)
  }
  # Year
  if(length(year) == 1){
    fyear <- c(year)
  }
  if(length(year) > 1){
    vyear <- c(year)
  }
  # indic
  if(length(indic) == 1){
    findic <- c(indic)
  }
  if(length(indic) > 1){
    vindic <- c(indic)
  }
  # dimension
  if(length(dimension) == 1){
    fdimension <- c(dimension)
  }
  if(length(dimension) > 1){
    vdimension <- c(dimension)
  }
  # subgroups
  
  # Create a list of the Fixed Factors
  if(!is.null(fcountry)){
    fixedList <- c(fixedList, 'country'=list(fcountry))
  }
  if(!is.null(fyear)){
    fixedList <- c(fixedList, 'year'=list(fyear))
  }
  if(!is.null(findic)){
    fixedList <- c(fixedList, 'indic'=list(findic))
  }
  if(!is.null(fdimension)){
    fixedList <- c(fixedList, 'dimension'=list(fdimension))
  }
  
  # Create a list of the variable factors 
  if(!is.null(vcountry)){
    varList <- c(varList, 'country'=list(vcountry))
  }
  if(!is.null(vyear)){
    varList <- c(varList, 'year'=list(vyear))
  }
  if(!is.null(vindic)){
    varList <- c(varList, 'indic'=list(vindic))
  }
  if(!is.null(vdimension)){
    varList <- c(varList, 'dimension'=list(vdimension))
  }
  
  return(list(fixed=fixedList, variab=varList))  # Try this out before returning anything
}
