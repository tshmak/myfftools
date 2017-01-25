ffcbind <- function(...) {
  
  ## A function for cbind-ing ff objects
  ## This is really inefficient -- needs to rewrite
  ## by converting everything to ff_vector first 
  ## and c() them together... 
  require(ffbase)
  objs <- list(...)
  
  objs[sapply(objs, is.null)] <- NULL
  
  test <- sapply(objs, nrow)
  if(!is.numeric(test) != "numeric") 
    stop("Something wrong with the input. Maybe not all are matrices/ff_matrix")
  
  if(!all(test == test[1])) 
    stop("The matrices/ff_matrices are of different dimensions.")

  for(i in 1:length(objs)) {
    colnames.i <- colnames(objs[[i]])
    dim(objs[[i]]) <- NULL
    if(i == 1) {
      res <- objs[[1]]
      colnames <- colnames.i
    }
    else {
      res <- c(res, objs[[i]])
      colnames <- c(colnames, colnames.i)
    }
  }
  res.length <- length(res)
  ncol <- res.length / test[1]
  dim(res) <- c(test[1],ncol)
  colnames(res) <- colnames
  
  return(res)
}

