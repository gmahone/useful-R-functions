get_rank <- function(x, decreasing=FALSE){
  if(!is.numeric(x)){
    return(cat("Input not numeric!"))
  }
  blank_vec <- rep(NA, length(x))
  tmp_order <- order(x, decreasing=decreasing)
  tmp_rank <- blank_vec
  tmp_rank[tmp_order] <- 1:length(x)
  return(tmp_rank)
}


