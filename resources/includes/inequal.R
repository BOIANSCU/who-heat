######### All the measures of inequality
# This manages the return of the inequalities measures inequality measures
#########


calcInequal <- function(data_frame,  inequal.types='all'){
  # df is a dataframe of the health indicator data with the equity dimension
  # inequal.types is a vector of the inequality measures to be calculated and defaults to all
  #  print('calcInequal()')  
  tmpDF <- data_frame
  
  if('national' %in% names(tmpDF)){  # Later rely on national average data if available
    national_flag <- T
  } else {
    national_flag <- F
    national_est <- NULL
  }
  
  if(inequal.types=='all'){
    inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
                       'paf', 'par', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')
  }
  
  ineqDF <- data.frame(country=character(),
                       year=integer(), 
                       indic=character(),
                       dimension=character(),
                       measure=character(),
                       inequal=numeric(),
                       boot.se=numeric(),
                       se=numeric(),
                       ccode=character(),
                       #                       typeordered=logical(),
                       stringsAsFactors=FALSE)
  
  countrylist <- unique(tmpDF$country)
  yearlist <- unique(tmpDF$year)
  indiclist <- unique(tmpDF$indic)
  dimensionlist <- unique(tmpDF$dimension)
  
  for(i in countrylist){
    tmp_ccode <- unique(tmpDF$ccode[tmpDF$country==i])
    for(j in yearlist){
      for(k in indiclist){
        for(l in dimensionlist){
          for(m in inequal.types){
            #            print(m)
            relevant.rows <- which(tmpDF$country==i & tmpDF$year==j & tmpDF$indic==k & tmpDF$dimension==l)
            ineq.result <- NULL
            if(length(tmpDF$estimate[relevant.rows]) > 1 & all(!is.na(tmpDF$estimate[relevant.rows]))){  
              # If there are 1 or 0 rows, there can be no inequality.
              # If there is missing data it is impossible to estimate the inequality
              
              if(c('rankorder') %in% names(tmpDF)){
                # If order information is provided about the ranking of equity dimension, record this
                rankorder <- tmpDF$rankorder[relevant.rows]
              }
              else{
                rankorder <- 0
              }
              
              if(national_flag){
                national_est <- unique(tmpDF$national[relevant.rows])
              }
              
              maxopt <- tmpDF$maxoptimum[relevant.rows][1]==1
              
              # If the equity dimenstion is rankable, note that T/F
              ranked <- tmpDF$rankable[relevant.rows][1]==1
              
              
              starttime <- proc.time()
              # Calculate the ACI (Absolute Concentration Index)
              if((m=='aci') & ranked){
                ineq.result <- aci(x=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], bs=T, rankorder=rankorder)
              }
              
              
              
              # Calculate the BGV (Between Groups Variance)
              if(m=='bgv'){
                ineq.result <- bgv(x=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], bs=T)        
              }
              
              
              
              # Calculate IDis (Index of Disparity)
              if(m=='idis'){
                ineq.result <- idis(x=tmpDF$estimate[relevant.rows], 
                                    w=tmpDF$pop[relevant.rows], 
                                    se=tmpDF$se[relevant.rows], 
                                    bs=T, 
                                    national_est=national_est)                
              }
              
              
              
              # Calculate KMI (Kunst Mackenbach (Relative) Index)
              if((m=='riikm') & ranked){
                ineq.result <- riikm(y=tmpDF$estimate[relevant.rows], 
                                     w=tmpDF$pop[relevant.rows], 
                                     se=tmpDF$se[relevant.rows], 
                                     bs=T,
                                     rankorder=rankorder,
                                     maxopt=maxopt)    
              }
              
              
              
              # Calculate mdb (Mean Difference from the Best performing Subgroup)
              if(m=='mdb'){
                ineq.result <- mdb(x=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], bs=T, maxopt=maxopt)                
              }
              
              
              
              # Calculate MDM (Meand Difference from the Mean)
              if(m=='mdm'){
                ineq.result <- mdm(x=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], bs=T,
                                   national_est=national_est)    
              }
              
              
              
              
              # Calculate MLD (Mean Log Deviation)
              if(m=='mld'){
                ineq.result <- mld(x=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], bs=T,
                                   national_est=national_est)  
                
              }
              
              
              # Calculate PAF (Population Attributable Fraction)
              if(m=='paf'){
                ineq.result <- paf(x=(tmpDF$estimate[relevant.rows]),  # This reverses the percentages on, say ANC visits 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows],
                                   bs=T, maxopt=maxopt,
                                   rankorder = tmpDF$rankorder[relevant.rows],
                                   national_est = national_est)
              }
              
              
              
              
              # Calculate PAR (Population Attributable Risk)
              if(m=='par'){
                ineq.result <- PAR(x=(tmpDF$estimate[relevant.rows]),  # This reverses the percentages on, say ANC visits 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows],
                                   bs=T, maxopt=maxopt,
                                   rankorder = tmpDF$rankorder[relevant.rows],
                                   national_est = national_est)
              }
              
              
              
              
              
              #               # Calculate RCI (Relative Concentration Index)
              if((m=='rci') & ranked){
                ineq.result <- rci(y=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], 
                                   bs=T, 
                                   rankorder=rankorder)
                
              }
              
              
              
              # Calculate RD (Rank/Rate Difference) 
              if(m=='rd'){
                ineq.result <- rd(x=tmpDF$estimate[relevant.rows], 
                                  w=tmpDF$pop[relevant.rows], 
                                  se=tmpDF$se[relevant.rows], 
                                  bs=T,
                                  rankorder=rankorder)               
              }
              
              
              
              
              
              # Calculate RII (Relative Index of Inequality) 
              if((m=='rii')  & ranked){
                ineq.result <- rii(y=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], 
                                   bs=T, 
                                   rankorder=rankorder,
                                   maxopt=maxopt)
              }
              
              
              
              # Calculate RR (Rank/Rate Ratio) 
              if(m=='rr'){
                ineq.result <- rr(x=tmpDF$estimate[relevant.rows], 
                                  se=tmpDF$se[relevant.rows], 
                                  rankorder=rankorder,
                                  maxopt=maxopt,
                                  bs=T)
                
              }
              
              
              
              # Calculate SII (Slope Index of Inequality)
              if((m=='sii')  & ranked){  
                ineq.result <- sii(y=tmpDF$estimate[relevant.rows], 
                                   w=tmpDF$pop[relevant.rows], 
                                   se=tmpDF$se[relevant.rows], 
                                   bs=T, 
                                   rankorder=rankorder,
                                   maxopt=maxopt)
              }
              
              
              # Calculate TI (Theil Index)
              if(m=='ti'){
                ineq.result <- ti(y=tmpDF$estimate[relevant.rows], 
                                  w=tmpDF$pop[relevant.rows], 
                                  se=tmpDF$se[relevant.rows], bs=T)
              }
              
              print(paste('country=',i,' year=', j, ' indic=', k, ' dimension=', l, ' measure=', m, ' inequal=', ineq.result[[1]],
                          ' boot.se=', ineq.result[[2]], ' se=', ineq.result[[3]], tmp_ccode))
              
              # Append the last calculated measure to the end of the dataframe 
              if(!is.null(ineq.result)){
                ineqDF <- rbind(ineqDF, data.frame(country=i,
                                                   year=j, 
                                                   indic=k,
                                                   dimension=l,
                                                   measure=m,
                                                   inequal=ineq.result[[1]],
                                                   boot.se=ineq.result[[2]],
                                                   se=ineq.result[[3]],
                                                   ccode=tmp_ccode,
                                                   #                                     typeordered=logical(),
                                                   stringsAsFactors=FALSE))
              }
            }
          }          
        }        
      }  
    }  
  }
  ineqDF <- ineqDF[!duplicated(ineqDF), ]  #*****  For some reason rows are being duplicated.  This is a lazy stop-gap
  return(ineqDF)
}



getInequal <- function(indicator, stratifier, countries, years,  inequal_types='all'){
  
  # Fetch the inequalities data from the inbuilt database  
  
  # Return NULL if any of the function-parameters are missing
  if(is.null(indicator)){
    return(NULL)
  }
  if(length(indicator)==0){
    return(NULL)
  }
  if(is.null(stratifier)){
    return(NULL)
  }
  if(length(stratifier)==0){
    return(NULL)
  }
  if(is.null(countries)){
    return(NULL)
  }
  if(length(countries)==0){
    return(NULL)
  }
  if(is.null(years)){
    return(NULL)
  }
  if(length(years)==0){
    return(NULL)
  }
  
  
  
  require('RSQLite')
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, "../resources/data/HEMTK.db")
  
  if(is.null(inequal_types)){
    return(NULL)
  }
  
  if(inequal_types=='all'){
    inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
                       'paf', 'par', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')
  }
  else{
    inequal.types <- inequal_types
  }
  
  baseStr <- 'SELECT country, year, indic, dimension, measure, inequal, [boot.se], se FROM inequals WHERE country IN ('
  
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
  
  measureStr <- c()
  for(i in inequal.types){
    measureStr <- c(measureStr, paste('"', i, '"', sep=''))
  }
  measureStr <- paste(measureStr, collapse=', ')
  
  
  selectStr <- paste(baseStr, countryStr, ") AND year IN (", yearStr, ") AND indic IN (", indicStr, ") AND dimension IN (", stratStr, ") AND measure IN (", measureStr, ");", sep="", collapse="")
  
  print(selectStr)
  
  ineqDF <- dbGetQuery(con, selectStr)
  dbDisconnect(con)
  if(is.null(ineqDF)){
    return(NULL)
  }
  
  ineqDF$year <- as.integer(ineqDF$year)
  ineqDF$boot.se <- as.numeric(ineqDF$boot.se)
  ineqDF$boot.se[ineqDF$boot.se==0] <- NA
  ineqDF$se <- as.numeric(ineqDF$se)
  ineqDF$se[ineqDF$se == 0] <- NA
  ineqDF$combo.se <- ineqDF$se
  ineqDF$combo.se[is.na(ineqDF$se)] <- ineqDF$boot.se[is.na(ineqDF$se)]
  
  ineqDF$boot.lowerci <- ineqDF$inequal - (1.96 * ineqDF$boot.se) 
  ineqDF$boot.upperci <- ineqDF$inequal + (1.96 * ineqDF$boot.se) 
  ineqDF$se.lowerci <- ineqDF$inequal - (1.96 * ineqDF$se) 
  ineqDF$se.upperci <- ineqDF$inequal + (1.96 * ineqDF$se) 
  ineqDF$combo.lowerci <- ineqDF$inequal - (1.96 * ineqDF$combo.se) 
  ineqDF$combo.upperci <- ineqDF$inequal + (1.96 * ineqDF$combo.se) 
  
  
  
  ineqDF$combo.se[is.na(ineqDF$combo.se)] <- ineqDF$boot.se[is.na(ineqDF$combo.se)]  #  Make an se that is analytic if it exists, otherwise a boostrap
  
  return(ineqDF)
}



