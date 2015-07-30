#############################################################################
#
#
############################################################################

getHETKdata <- function(indicator, stratifier, countries, years, mostrecent=F, datasource='All'){
  require('RSQLite')
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, "../resources/data/HEMTK.db")
  
  
  #   ## First make sure that the years are correct
  #   if(length(years)==2){
  #     years <- years[1]:years[2]
  #   }
  
  if(mostrecent){
    # Determine the most recent year for 'country' data in the database
    mostrecentStr <- paste("SELECT  MAX(year), COUNT(*) FROM maindata WHERE country='", countries, "'", sep='')
    if(datasource!='All'){
      # Take account of MICS/DHS choices
      mostrecentStr <- paste(mostrecentStr, "AND source='", datasource, "';", sep='')
    }
    else{
      mostrecentStr <- paste(mostrecentStr, ";", sep='')
    }   
    years <- as.list(dbGetQuery(con, mostrecentStr ))[[1]]
  }
  ###
  
  baseStr <- 'SELECT country, year, source, indic, dimension, subgroup, r, r_lower, r_upper, se, pop, iso3, rankable, maxoptimum, popshare, flag, rankorder FROM maindata WHERE country IN ('  
  baseStr_nationalData <- 'SELECT country, year, source, indic, r FROM nationaldata WHERE country IN ('  
  
  countryStr <- c()
  for(i in countries){
    countryStr <- c(countryStr, paste('"', i, '"', sep=''))
  }
  countryStr <- paste(countryStr, collapse=', ')
  
  yearStr <- paste(years, collapse=', ') 
  
  indicStr <- c()
  for(i in indicator){
    indicStr <- c(indicStr, paste('"', i, '"', sep=''))
  }
  indicStr <- paste(indicStr, collapse=', ')
  
  stratStr <- c()
  for(i in stratifier){
    stratStr <- c(stratStr, paste('"', i, '"', sep=''))
  }
  stratStr <- paste(stratStr, collapse=', ')
  
  
  
  selectStr <- paste(baseStr, countryStr, ') AND year IN (', yearStr, ') AND indic IN (', indicStr, ') AND dimension IN (', stratStr, ');', sep='', collapse='')
  print(selectStr)
  selectNationalStr <- paste(baseStr_nationalData, countryStr, ') AND year IN (', yearStr, ') AND indic IN (', indicStr, ');', sep='', collapse='')
  
  hetk.data <- dbGetQuery(con, selectStr)
  national.data <- dbGetQuery(con, selectNationalStr)
  dbDisconnect(con)
  if(mostrecent==T){
    names(hetk.data)[2] <- 'year'
    names(national.data)[2] <- 'year'
  } 
  
  names(national.data)[5] <- 'national'
  hetk.data <- merge(hetk.data, national.data, by=c('country', 'year', 'source', 'indic'))
  
  
  hetk.data$year <- as.integer(hetk.data$year)
  hetk.data$r <- as.numeric(hetk.data$r)
  hetk.data$se <- as.numeric(hetk.data$se)
  hetk.data$pop <- as.integer(hetk.data$pop)
  hetk.data$r_lower <- as.numeric(hetk.data$r_lower)
  hetk.data$r_upper <- as.numeric(hetk.data$r_upper)
  hetk.data$rankable <- as.integer(hetk.data$rankable)
  names(hetk.data)[which(names(hetk.data)=='r')] <- 'estimate'
  names(hetk.data)[which(names(hetk.data)=='r_lower')] <- 'lower_95ci'
  names(hetk.data)[which(names(hetk.data)=='r_upper')] <- 'upper_95ci'
  
  hetk.data <- orderHetkFactors(hetk.data)
  return(hetk.data)
}


orderHetkFactors <- function(DF){
  geo_levels <- unique(DF$subgroup[which(DF$dimension == 'Geographic region')])
  factor_order <- c('Quintile 1 (poorest)', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5 (richest)', 'No education', 'Primary school', 'Secondary school+', 'Urban', 'Rural', 'Male', 'Female')
  if(length(geo_levels>0)){  # Test to see that there are any geo_levels
    factor_order <- c(factor_order, geo_levels)
  }
  DF$subgroup <- factor(DF$subgroup, levels = factor_order)
  DF$subgroup <- factor(DF$subgroup)
  return(DF)
}
