rbind.ff_matrix <- function(...) {
  if("rbind" %in% names(list(...))) {
    stop("There shouldn't be an object called .rbind in ...")
  }
  return(cbind.ff_matrix(..., .rbind=TRUE))
}
