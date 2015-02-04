#' has_description
#' 
#' Checks that the directory given has a DESCRIPTON file
#' 
#' @param dir a directory
#' @return boolean true or false
#' @export
#' @author Reinhard Simon
has_description <- function(dir = "."){
  file.exists(file.path(dir, "DESCRIPTION"))
}
