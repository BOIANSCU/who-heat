#############################################################################
#
#
#
############################################################################
source('../resources/includes/inequal.R')
source('../resources/includes/is_rank.R')
source('../resources/includes/list_append.R')
source('../resources/includes/not_in.R')
source('../resources/includes/sql_in.R')
source('../resources/includes/aci.R')  # Absolute concentration index
source('../resources/includes/bgv.R')  # Between groups variance
source('../resources/includes/idis.R')  # Index of disparity
source('../resources/includes/riikm.R')  # Kunst Mackenbach Index
source('../resources/includes/mdb.R')  # Mean difference between best subgroup
source('../resources/includes/mdm.R')  # Meand difference between mean
source('../resources/includes/mld.R')  # Mean log difference
source('../resources/includes/paf.R')  # Population attributable fraction
source('../resources/includes/par.R')  # population attributable risk (capitalised so as not to confuise par())
source('../resources/includes/rci.R')  # Relative concentration index
source('../resources/includes/rd.R')  # Rate (or Range) Difference
source('../resources/includes/rii.R')  # Relative Index of Inequality
source('../resources/includes/rr.R')  # Rate (or Range) Ratio
source('../resources/includes/sii.R')  # Slope index of inequality
source('../resources/includes/ti.R')  # Theil index
source('../resources/includes/midpointprop.R')
require(RSQLite)


makeInequalTable <- function(){
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, "../resources/data/HEMTK.db")
  flag <- 1
 
  if(dbExistsTable(con, 'inequals')){
    # Check to see if there is an old inequals table
    dbRemoveTable(con, 'inequals')  # Delete the table
    flag <- 0
  }
  

  theStr <- 'SELECT DISTINCT country FROM maindata;'
  listOfCountries <- dbGetQuery(con,theStr)$country
    for(i in listOfCountries){
      theStr <- paste0('SELECT DISTINCT year FROM maindata WHERE country = "', i, '";')
      listOfYears <- dbGetQuery(con, theStr)$year
        for(j in listOfYears){
          theStr <- theStr <- paste0('SELECT DISTINCT indic FROM maindata WHERE country = "', i, '" AND year="', j,'";')
          listOfIndicators <- dbGetQuery(con, theStr)$indic
            for(k in listOfIndicators){
              theStr <- paste0('SELECT DISTINCT dimension FROM maindata WHERE country = "', i, '" AND year="', j, '" AND indic ="', k, '";')
              nationalStr <- paste0('SELECT country, year, source, indic, r FROM nationaldata WHERE country  = "', i, '" AND year="', j, '" AND indic ="', k, '";')
              national.data <- dbGetQuery(con, nationalStr)
              names(national.data)[5] <- 'national'
              listOfDimensions <- dbGetQuery(con, theStr)$dimension
              for(l in listOfDimensions){
                theStr <- paste0('SELECT DISTINCT source FROM maindata WHERE country = "', i, '" AND year="', j,'" AND indic ="', k, '" AND dimension = "', l, '";')
                listOfSources <- dbGetQuery(con, theStr)[,1]
                  for(m in listOfSources){
                    theStr <- paste0('SELECT country, year, source, indic, dimension, subgroup, r, r_lower, r_upper, se, pop, iso3, rankable, maxoptimum, popshare, flag, rankorder FROM maindata WHERE country = "', i, '" AND year="', j,'" AND indic ="', k, '" AND dimension = "', l, '" AND source = "', m, '";')
                    tmp.data <- dbGetQuery(con, theStr) 
                    tmp.data <- merge(tmp.data, national.data, by=c('country', 'year', 'source', 'indic'))
                    tmp.data$year <- as.integer(tmp.data$year)
                    tmp.data$r <- as.numeric(tmp.data$r)
                    tmp.data$se <- as.numeric(tmp.data$se)
                    tmp.data$pop <- as.integer(tmp.data$pop)
                    tmp.data$rankable <- as.integer(tmp.data$rankable)
                    names(tmp.data)[which(names(tmp.data)=='r')] <- 'estimate'     
                    names(tmp.data)[which(names(tmp.data)=='iso3')] <- 'ccode'
                    tmp.data <- orderFactors(tmp.data)  
                    inequal.data <- calcInequal(tmp.data)
                    if(flag==1){
                      dbWriteTable( conn = con, name = "inequals", value =  inequal.data, row.names=FALSE, append=TRUE )
                    }
                    if(flag==0){
                      dbWriteTable( conn = con, name = "inequals", value =  inequal.data, row.names=FALSE )
                      flag <- 1
                    }
                  }  
              }
            }
        }
    }
  dbDisconnect(con)
}


orderFactors <- function(DF){
  geo_levels <- unique(DF$subgroup[which(DF$dimension == 'Geographic region')])
  factor_order <- c('Quintile 1 (poorest)', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Quintile 5 (richest)', 'No education', 'Primary school', 'Secondary school+', 'Urban', 'Rural', 'Male', 'Female')
  if(length(geo_levels>0)){  # Test to see that there are any geo_levels
    factor_order <- c(factor_order, geo_levels)
  }
  DF$subgroup <- factor(DF$subgroup, levels = factor_order)
  DF$subgroup <- factor(DF$subgroup)
  return(DF)
}

