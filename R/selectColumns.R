selectColumns <- function(list.of.ff, columns, rows=NULL, ff=F, nocbind=F) {
  ## Select columns from a list of ff objects
  ## columns: A vector of column numbers or logical
  ## ff: convert to ff object
  
  if(is.logical(columns)) columns <- (1:length(columns))[columns]
  getLocations.obj <- getLocations(list.of.ff, columns)

  result <- NULL
  for(i in 1:length(list.of.ff)) {
    if(length(getLocations.obj[[i]]) > 0) {
      if(!is.null(rows)) {
        extracted <- list.of.ff[[i]][rows,getLocations.obj[[i]], drop=F]
      } else {
        extracted <- list.of.ff[[i]][,getLocations.obj[[i]], drop=F]
      }
      if(ff) extracted <- ff(extracted, dim=dim(extracted))

      if(nocbind) result <- c(result, list(extracted))
      else {
        if(ff) result <- ffcbind(result, extracted)
        else result <- cbind(result, extracted)
      }

    }
  }
  if(nocbind) names(result) <- names(list.of.ff)
  return(result)  
  
}