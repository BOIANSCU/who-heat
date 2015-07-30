notin <- function (vector1, vector2){
  # Return the elements in vector1 that are not in vector2
  el <- which(!(vector1 %in% vector2))  # svae in 'el' the element-locations on v1 not in v2
  if(length(el)==0){  # If there aren't any, return NULL
    return(NULL)
  }
  else{  # Else return the elements
    return (vector1[el])
  }
} 