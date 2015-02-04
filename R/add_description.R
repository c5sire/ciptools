#' add_description
#' 
#' Adds a description file in CIP format
#' 
#' @param adir a directory
#' @return logical success
#' @author Reinhard Simon
#' @export
add_description <- function(adir = "."){
  x = devtools::create_description(adir)
}