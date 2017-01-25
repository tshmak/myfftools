getLocations <- function(list.of.ff, columns=NULL) {
  ## Function to convert column numbers for entire genome
  ## to chromosome number and chromosome-specific column
  ## list.of.ff: A list of ff's
  ## columns: Columns by entire chromosome
  
  start <- 0
  result <- list()
  for(i in 1:length(list.of.ff)) {
    end <- start + ncol(list.of.ff[[i]])
    for.this <- columns[columns >= (start + 1) & columns <= end]
    for.this <- for.this - start
    result <- c(result, list(for.this))
    start <- end
  }
  names(result) <- names(list.of.ff)
  return(result)
}