lappend <- function (lst, ...){
  # Append an item to a list; e.g.:
  # vara <- list("axislimit"=T)
  # vara <- lappend(vara, "print"=F)
  lst <- c(lst, list(...))
  return(lst)
}
