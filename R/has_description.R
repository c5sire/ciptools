#' has_description
#' 
#' Checks that the directory given has a DESCRIPTON file
#' 
#' @param dir a directory
#' @return boolean true or false
#' @export
#' @author Reinhard Simon
has_description <- function(dir = "."){
  dir %>%
    file.path("DESCRIPTION") %>%
      file.exists()
}
