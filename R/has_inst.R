#' has_inst
#' 
#' Checks that the directory given has a subdirectory 'inst'
#' 
#' @param adir a directory
#' @return boolean true or false
#' @export
#' @author Reinhard Simon
has_inst <- function(adir = ".") {
  file.exists(file.path(adir, "inst"))
} 
