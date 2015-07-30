
getFilteredCountries <- function(WBgroup, WHOregion, database)
{
  # This function filters a list of countries according to their WHO Region and
  # World Bank income group
  
  if (database == "HETK")
  {
    # Connect to the local GHO (HEMTK) SQLite database
    require("RSQLite")
    drv <- dbDriver("SQLite")
    con <- dbConnect(drv, "../resources/data/HEMTK.db")
    
    strWHOregion <- ""
    if (!is.null(WHOregion))
    {
      for(i in WHOregion){  # Loop through all the regions to concatenate them in the SQL statement
        strWHOregion <- paste("whoreg6 IN", sqlIn(WHOregion), sep = "")
      }
    }
    strWBgroup <- ""
    if (strWHOregion != "" & !is.null(WBgroup))
    {
      strWBgroup <- paste("AND wbincome2014_4cat IN ", sqlIn(WBgroup), sep = "")
    }
    if (strWHOregion == "" & !is.null(WBgroup))
    {
      strWBgroup <- paste("wbincome2014_4cat IN ", sqlIn(WBgroup), sep = "")
    }
    
    strWHERE <- ""
    if (strWBgroup != "" | strWHOregion != "")
    {
      strWHERE <- " WHERE "
    }
    
    selectStr <- paste("SELECT DISTINCT country FROM maindata", strWHERE, 
                       strWHOregion, strWBgroup, ";", sep = "")
    
    if(is.null(WBgroup) & is.null(WHOregion)){
      selectStr <- "SELECT DISTINCT country FROM maindata"
    }
    
    print(selectStr)
    data <- dbGetQuery(con, selectStr)$country
    dbDisconnect(con)
    return(data)
  } else {
    return(NULL)
  }
}



getFilteredIndim <- function(country, years=NULL, mostrecent=F, datasource='All', database, hindicator=NULL, option='hlth_indic', scope='All'){
  # This function filters the Health Indicators and Health Dimensions based on earlier choices about the Country
  # the Year(s), the Datasource and the Database 
  
  require('RSQLite')
  drv <- dbDriver('SQLite')
  con <- dbConnect(drv, '../resources/data/HEMTK.db')
  
  
  # if(length(years)==2){
  #   years <- years[1]:years[2]
  # }
  
  if(mostrecent){
    # Determine the most recent year for 'country' data in the database
    mostrecentStr <- paste('SELECT  MAX(year), COUNT(*) FROM maindata WHERE country="', country, '"', sep='')
    if(datasource!='All'){
      # Take account of MICS/DHS choices
      mostrecentStr <- paste(mostrecentStr, 'AND source="', datasource, '";', sep='')
    }
    else{
      mostrecentStr <- paste(mostrecentStr, ';', sep='')
    }   
    years <- as.list(dbGetQuery(con, mostrecentStr ))[[1]]
  }
  
  if(database=='HETK'){    
    
    if(option=='hlth_indic'){
      baseStr <- 'SELECT DISTINCT indic FROM maindata WHERE country="'
    }
    if(option=='equity_dim'){
      baseStr <- 'SELECT DISTINCT dimension FROM maindata WHERE country="'
    }
    
    # base string now includes the country    
    baseStr <- paste(baseStr, country, '"', sep="")
    
    print(baseStr)
    
    # base string now includes years
    if(mostrecent){
      baseStr <- paste(baseStr, 'AND year IN ("', years, '"); ', sep="")
    }
    if(!mostrecent & !is.null(years)){
      baseStr <- paste(baseStr, 'AND year IN ', sqlIn(years), sep='')
    }
    
    # base string now includes survey source (DHS/MICS)
    if(datasource != 'All'){
      baseStr <- paste(baseStr, 'AND source="', datasource, '" ', sep='')
    }
    
    # Include Health Indicator if necessary
    if(!is.null(hindicator) & option=='equity_dim'){
      baseStr <- paste(baseStr, 'AND indic IN ', sqlIn(hindicator), sep='')
    }
    
    selectStr <- paste(baseStr, ';', sep='')
    print(selectStr)
    data <- as.list(dbGetQuery(con, selectStr))
    
    if(option=='hlth_indic'){
      if(scope=='All'){
        data <- sort(unique(data$indic))
      }
      if(scope=='Common'){
        data <- sort(findCommon(data$indic, data$year))
      }
    }
    if(option=='equity_dim'){
      if(scope=='All'){
        data <- sort(unique(data$dimension))
      }
      if(scope=='Common'){
        data <- sort(findCommon(data$dimension, data$indic))
      }
    }
    
    dbDisconnect(con)
    return(data)
  }
  else{
    return(NULL)
  }
}



getFilteredYear <-  function(country, datasource='All', database){
  # This function filters the Years of surveys based on earlier choices about the Country
  # the Datasource and the Database 
  
  require('RSQLite')
  drv <- dbDriver('SQLite')
  con <- dbConnect(drv, '../resources/data/HEMTK.db')
  
  
  if(database=='HETK'){    
    
    
    baseStr <- 'SELECT DISTINCT year FROM maindata WHERE country="'
    
    
    # base string now includes the country    
    baseStr <- paste(baseStr, country, '" ', sep='')
    
    print(baseStr)
    
    
    # base string now includes survey source (DHS/MICS)
    if(datasource != 'All'){
      baseStr <- paste(baseStr, 'AND source="', datasource, '" ', sep='')
    }
    
    baseStr <- paste0(baseStr, ';')
    
    data <- as.list(dbGetQuery(con, baseStr))
    
    dbDisconnect(con)
    return(data$year)
  }
  else{
    return(NULL)
  }
  
}