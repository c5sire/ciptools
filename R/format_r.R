#' format_r
#' 
#' formats R code
#' 
#' @param adir a directory
#' @return boolean true or false
#' @export
#' @author Reinhard Simon
format_r <- function(adir = "."){
  formatR::tidy_source("R", indent=2, arrow=TRUE, blank = TRUE)
}
