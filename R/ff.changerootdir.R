ff.changerootdir <- function(obj, force=F) {

  if(is.null(obj)) return(NULL)
  name.in.parent.frame <- deparse(substitute(obj))
  
  windowsroot <- getOption(".rootdir.windows")
  serverroot <- getOption(".rootdir.server")

  
  if(!any(!is.na(match(class(obj), "ff")))) {
    stop("object must be of class ff")
  }
  is.windows <- substr(filename(obj),1,nchar(windowsroot)) == windowsroot
  is.server <- substr(filename(obj),1,nchar(serverroot)) == serverroot
  if(!any(is.windows, is.server)) stop("Cannot identify root dir of object")
  
  if(force==F) {
    if((is.windows && .rootdir == windowsroot) || 
         (is.server && .rootdir == serverroot)) {
           stop("object seems to have the right rootdir...")
         }
  }
  
  am.in.windows <- NA
  if(.rootdir == windowsroot) am.in.windows <- T
  if(.rootdir == serverroot) am.in.windows <- F
  
  
  if(is.windows && !am.in.windows) newfilename <- sub(windowsroot, serverroot, filename(obj))
  else if(is.server && am.in.windows) newfilename <- sub(serverroot, windowsroot, filename(obj))
  else return(invisible(NULL))

  physical(obj)$filename <- newfilename
  
  assign(name.in.parent.frame, obj, envir=parent.frame())
  
}
