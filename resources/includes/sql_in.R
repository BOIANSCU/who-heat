sqlIn <- function(a_vector){
  # Take a character or numeric vector and return it in a form suitable for using after the 'IN'
  # subcommand in a SQLite SELECT statement.
  # eg: SELECT a FROM table WHERE c IN ("xx", "yy", "zz");
  
  if(is.numeric(a_vector)){
    num.str <- gsub(", ", ", ", toString(a_vector))
    returnStr <- paste0(' (', num.str, ') ')
  }
  else if(is.character(a_vector)){
    returnStr <- ''
    vlength <- length(a_vector)
    for(i in 1:vlength){
      returnStr <- paste0(returnStr, '"', a_vector[i], '"')
      if(i != vlength){
        returnStr <- paste0(returnStr, ', ')
      }
      else{
        returnStr <- paste0(' (', returnStr, ') ')
      } 
    }
  }
  else{
    returnStr <- NULL
  }
  return(returnStr)
}
