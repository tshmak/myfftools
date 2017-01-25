MakePermanent <- function(ff_object, filename, readonly=T) {
  ## Change the filename of an ff object to make it permanent
  ## ff_object can be a list of ff objects, in which case
  ## filename should be a directory name (new or existing)

  require(ff)
  if(class(ff_object)[1] == "list") {
    currentdir <- getwd()
    test <- try(setwd(filename), silent=T)
    if(class(test)=="try-error") {
      dir.create(filename)
      setwd(filename)
    }
    
    for(i in 1:length(ff_object)) {
      name <- names(ff_object)[i]
      if(is.null(name)) name <- paste("Part", i, sep="")
      
      if(!is.null(ff_object[[i]])) {
        MakePermanent(ff_object[[i]], paste(name,".ff", sep=""), readonly)
      }
    }
    setwd(currentdir)
    what <- deparse(substitute(ff_object))
    assign(what, ff_object, envir=parent.frame())
  }
  else {
    what <- deparse(substitute(ff_object))
    
    if(substr(filename,max(nchar(filename)-2,0),nchar(filename)) != ".ff")
      filename <- paste(filename, ".ff", sep="")
    if(substr(filename,1,2) != "./" && length(grep("/", filename)) == 0)
      filename <- paste("./", filename, sep="")
    
    filename(ff_object) <- filename
    physical(ff_object)$readonly <- readonly
    finalizer(ff_object) <- "close"
    assign(what, ff_object, envir=parent.frame())
    # print(ls(parent.frame()))
    # print(what)
    # print(filename(ff_object))
    # print(filename)
    # print(ff_object$finalizer)
    # print(ff_object$filename)
    # print(class(ff_object))
    # print(physical(ff_object))
    
  }
}
