do_knit <- function(afile, adir) {
  devtools::in_dir(
    adir,
    rmarkdown::render("tutorial.Rmd", "html_vignette")  
  )
  
}

#' build
#' 
#' Takes special care of vignettes
#' 
#' @author Reinhard Simon
#' @family
#' @export
build <- function(){
  # build vignettes
  vig_lst <- list.files("vignettes", pattern = "*.Rmd")
  sapply(vig_lst, do_knit, "vignettes")
  # copy over to inst/doc
  fls_lst <- file.path("vignettes",list.files("vignettes"))
  if(!file.exists("inst/doc")) {
    dir.create("inst/doc")
  }
  file.copy(fls_lst, file.path("inst", "doc"), over = TRUE, rec = TRUE)

  # devtools build
  devtools::build()
  
  # format
  format_code()
  
}
