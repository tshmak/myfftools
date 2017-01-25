load.ff.RData <- function(filename) {
  ## Function to load a .RData file which has as its contents
  ## an ff object, or a list of ff object
  ## It automatically performs ff.changerootdir on the object
  
  objects <- load(filename, envir=.GlobalEnv)
  for(obj in objects) {
    class <- class(get(obj, envir=.GlobalEnv, inherits=F))
    if(class[1] == "list" && length(class) == 1) {
      for(ff.obj in get(obj, envir=.GlobalEnv, inherits=F)) {
        if(any(class(ff.obj) == "ff")) {
          ff.changerootdir(ff.obj, force=T)
        }
      }
    }
    else {
      if(any(class(get(obj, envir=.GlobalEnv, inherits=F)) == "ff")) {
        ff.changerootdir(get(obj, envir=.GlobalEnv, inherits=F), force=T)
      }
    }
  }
}
