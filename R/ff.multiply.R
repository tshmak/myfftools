ff.multiply <- function(x,y, chunk.x=nrow(x), chunk.y=ncol(y)) {
  ## Function to perform matrix multiplication for
  ## large matrices by doing it by chunks
  ## - supports ff matrices
  ## chunk.x gives the chunking by row (with reference to x)
  ## chunk.y gives the chunking by col (with reference to y)
  
  ncol.x <- ncol(x)
  nrow.y <- nrow(y)
  
  if(is.null(ncol.x) || is.null(nrow.y))
    stop("x and y must have 2 dimensions")
  if(ncol.x != nrow.y)
    stop("Dimensions of and x and y don't match")
  
  require(bit)
  
  ncol.y <- ncol(y)
  nrow.x <- nrow(x)
  result.mat <- matrix(NA, nrow=nrow.x, ncol=ncol.y)
  chunks.y <- chunk(1, ncol.y, by=chunk.y)
  chunks.x <- chunk(1, nrow.x, by=chunk.x) 
  
  for(i in chunks.x) {
    for(j in chunks.y) {
      result.mat[i[1]:i[2],j[1]:j[2]] <- 
        x[i[1]:i[2], ] %*% y[,j[1]:j[2]]
    }
  }
 
  return(result.mat)
}