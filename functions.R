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

rename.column <- function(x, y, data){
  colnames(data)[which(colnames(data) %in% x)] <- y
  return(data)
}


### arguments
# means_df - dataframe with means, 1st column id, 2nd column means, other columns ignored
# lsd - lsd value
# decreasing - trait directionality. should the values decrease (default) or increase?
create_lsd_table <- function(means_df, lsd, decreasing=TRUE){
  means_df <- means_df[order(means_df[,2], decreasing=decreasing),]
  diff_mat <- outer(means_df[,2], means_df[,2], "-")
  init_mat <- matrix(NA, nrow=nrow(diff_mat), ncol=ncol(diff_mat))
  cur_letter <- 1
  for(i in 1:nrow(diff_mat)){
    not_sig_diff <- diff_mat[i,] < lsd
    if(i == 1){
      last_length <- sum(not_sig_diff)
      init_mat[i, not_sig_diff] <- LETTERS[cur_letter]
    } else {
      cur_length <- sum(not_sig_diff)
      if(cur_length != last_length){
        cur_letter <- cur_letter + 1
        init_mat[i, not_sig_diff] <- LETTERS[cur_letter]
        last_length <- cur_length
      }
    }
  }
  init_mat[lower.tri(init_mat)] <- NA
  means_df$Groups <- apply(init_mat, 2, function(x) paste(as.vector(na.omit(x)), sep="", collapse=","))
  return(means_df)
}
