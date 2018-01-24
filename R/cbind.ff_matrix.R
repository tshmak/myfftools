cbind.ff_matrix <- function(..., .rbind=FALSE) {
  
  #' A function for c/rbind-ing ff objects
  #' Memory inefficient. Use ffcbind for a memory efficient option. 
  
  objs <- list(...)
  
  objs[sapply(objs, is.null)] <- NULL
  
  test <- sapply(objs, ifelse(.rbind, ncol, nrow))
  if(!is.numeric(test) != "numeric") 
    stop("Something wrong with the input. Maybe not all are matrices/ff_matrix")
  
  if(!all(test == test[1])) 
    stop("The matrices/ff_matrices are of different dimensions.")
  
  result <- do.call(ifelse(.rbind, "rbind", "cbind"), 
                    lapply(objs, function(x) x[]))
  

  return(as.ff(result))
}

