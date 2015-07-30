is.rank <- function(x){
  ranked <- T
  if(any(x < 1)){
    # The data are not an ordered subgroup
    ranked <- F
  }
  if(all(!x==1)){
    # The data are ordered by subgroup, but the base subgroup is missing
    ranked <- F
  }
  return(ranked)
}