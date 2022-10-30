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

do.load <- function(rdata){
  current.env <- ls()
  load(rdata)
  new.in <- ls()[!(ls() %in% c(current.env, "current.env"))]
  out <- eval(as.name(new.in))
  rm(list=new.in)
  return(out)
}
