#' format_r
#' 
#' formats R code
#' 
#' @param adir a directory
#' @return boolean true or false
#' @export
#' @author Reinhard Simon
format_r <- function(adir = ".") {
  if (has_r(adir)) {
    ff <- file.path(adir, "R", list.files("R"))
    for (i in 1:length(ff)) {
      formatR::tidy_source(ff[i], indent = 2, arrow = TRUE, blank = TRUE, file = ff[i])
    }
    return(TRUE)
  } else {
    message("No R sub-directory!")
  }
  
  
  FALSE
} 
