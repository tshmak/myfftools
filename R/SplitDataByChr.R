SplitDataByChr <- function(data, Chr, rs, ff2matrix=F) {
  
  ## Split whole-genome data by Chromosome
  ## Returns a list of data 
  ## Supports ff and (ffdf?) objects
  ## data: Genome data in ff/data.frame/ffdf?/matrix form
  ##       (must have column names)
  ## Chr: A vector of chromosome codes (as character)
  ## rs: A vector of RS numbers (as character)
  ## data.frame(Chr,rs) should correspond to look-up table
  ## Convert ff to matrix
  
  levels.ann <- unique(Chr)
  n.levels <- length(levels.ann)
  if(n.levels < 2) stop("Hang on --- there are less than two levels in Chr")
  lookup.table <- data.frame(Chr=Chr,rs=rs)
  
  SNPs <- lapply(levels.ann, function(i) {
                              temp <- subset(lookup.table, Chr == i)
                              return(temp$rs)
                            })

  require(ff)
  
  for(i in 1:n.levels) {
    selected <- !is.na(match(colnames(data), SNPs[[i]]))
    if(any(selected)) {

      if(any(class(data)=="ff_matrix") && ff2matrix == F) {
        selected <- as.bitwhich(selected)
        selected.data <- as.ff(data[,selected], vmode=vmode(data))
      }
      else if(any(class(data)=="ffdf") && ff2matrix == F) {
        stop("Method not yet implemented for ffdf")
      }
      else {
        selected.data <- data[, selected]
      }
    }
    else selected.data <- NULL
    
    ## Get names
    if(i==1) data.by.chr <- list(selected.data)
    else data.by.chr <- c(data.by.chr, list(selected.data))
    
    name <- as.character(levels.ann[i])
    if(grepl("^[0-9MmXxYy]", name)) 
      name <- paste("Chr",name,sep="")
    names(data.by.chr)[i] <- name
    
  }
  
  not.selected <- is.na(match(colnames(data), rs))
  if(any(not.selected)) {
    
    if(any(class(data)=="ff_matrix") && ff2matrix == F) {
      not.selected <- as.bitwhich(not.selected)
      not.selected.data <- as.ff(data[,not.selected], vmode=vmode(data))
    }
    else if(any(class(data)=="ffdf") && ff2matrix == F) {
      stop("Method not yet implemented for ffdf")
    }
    else {
      not.selected.data <- data[, not.selected]
    }
  }
  else not.selected.data <- NULL
  
  data.by.chr <- c(data.by.chr, list(not.annotated=not.selected.data))
  return(data.by.chr)
}
