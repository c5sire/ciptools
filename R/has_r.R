#' has_r
#' 
#' Checks that the directory given has an R subdirectory
#' 
#' @param dir a directory
#' @return boolean true or false
#' @export
#' @author Reinhard Simon
has_r <- function(dir = ".") {
  file.exists(file.path(dir, "R"))
} 
