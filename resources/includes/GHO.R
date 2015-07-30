#############################################################################
#
#
############################################################################


setupGHOdata <- function(indicator, stratifier, countries, years){
  if(is.null(stratifier)){return(NULL)}
  
  # Change country names to iso3c designations
  countries <- countrycode(countries, "country.name", "iso3c", warn = FALSE)
  
  # Return a vecotr of strings where each string represents the GHO API search
  search.list <- c()
  
  for(stratloop in stratifier){
    
    # The base URL for downloading GHO data at a file of comma serparated values
    base.gho.str <- "http://apps.who.int/gho/athena/data/data-text.csv?target=GHO/"
    
    # Add the Health Indicators
    indic.str <-paste(indicator, collapse=',')
    
    # Join indicator and base URL
    base.gho.str <- paste0(base.gho.str,indic.str)
    
    # Join the selected years up in to a string
    year.str <- c(';')
    for(i in years){
      year.str <- c(year.str, paste('YEAR:', toString(i), ';', sep=''))
    }
    year.str <- paste(year.str, collapse='')
    
    # Join the selected countries up in to a string
    country.str <- c('&profile=text&apikey=HEALTH-EQUITY-TOOLKIT-TEST-1&filter=')  # Note the insertion of the api-key here
    for(i in countries){
      country.str <- c(country.str, paste('COUNTRY:', i, ';', sep=''))
    }
    country.str <- paste(country.str, collapse='')
    
    # Sort out the stratifier  //MISSING GEOGRAPHY
    if('Sex' %in% stratloop){strat.str <- 'SEX:FMLE;SEX:MLE;'}
    if( "Economic status" %in% stratloop){strat.str <- 'WEALTHQUINTILE:*;'}
    if("Mother's Education" %in% stratloop){strat.str <- 'EDUCATIONLEVEL:*;'}
    if('Place of residence' %in% stratloop){strat.str <- 'RESIDENCEAREATYPE:*;'}
    
    gho.str <- paste0(base.gho.str, country.str, strat.str, year.str)
    search.list <- c(search.list, gho.str)
    gho.str <- c()
  }
  print(search.list)
  return(search.list)
}




getGHOdata <- function(search, indicator){
  
  mergedDF <- data.frame(country=character(), year=integer(), source=character(), indic=character(), 
                         dimension=character(), subgroup=character(), lower_95ci=integer(),
                         upper_95ci=integer(), se=integer(), pop=integer(), stringsAsFactors=FALSE) 
  
  empty.test <- 0
  for(i in search){
    if(!is.null(i)){empty.test <- 1}
  }
  if(empty.test == 0){ return(NULL)}
  
  counter <- 0
  
  for(http.str in search){
    counter <- counter + 1  # Keeping track of the iterations so the correct stratifier/dimension is included in the DF
    if(grepl('COUNTRY', http.str)){  # Test to see if the user has made any choices
      # Read the tmp.data from the Global Health Obervatory
      tmp.data <- read.csv(http.str, header=T, stringsAsFactors=F)
      print(tmp.data)
      # If there is data to download, then ...
      if(nrow(tmp.data)>0){
        names(tmp.data)[6] <- 'country'
        names(tmp.data)[4] <- 'year'
        names(tmp.data)[2] <- 'source'
        names(tmp.data)[1] <- 'indic'
        names(tmp.data)[7] <- 'subgroup'
        names(tmp.data)[9] <- 'estimate'
        # Add an NA for the standard error
        tmp.data$se <- NA
        # Add an NA for the population size
        tmp.data$pop <- NA 
        # Add lower 95%ci the data
        tmp.data$lower_95ci <- NA
        # Add upper 95%ci the tmp.data
        tmp.data$upper_95ci <- NA
        # Add upper 95%ci the tmp.data
        tmp.data$dimension <- indicator[counter]  #
        
        
        tmp.data <- tmp.data[, c('country', 'year', 'source', 'indic', 'dimension', 'subgroup', 'estimate', 'lower_95ci',
                                 'upper_95ci', 'se', 'pop')]       
      }
    }
    if(nrow(tmp.data)>0){
      mergedDF <- rbind(mergedDF, tmp.data)    
    }
  }
  if(nrow(tmp.data)==0){
    return(NULL)    
  }
  else{
    return(mergedDF)
  }
}