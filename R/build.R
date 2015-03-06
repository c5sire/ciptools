do_knit <- function(afile, adir) {
  devtools::in_dir(adir, rmarkdown::render(afile, "html_vignette"))
  
}

undo_instdoc_gi <- function() {
  file_name <- ".gitignore"
  file_size <- file.info(file_name)[["size"]]
  content <- readChar(file_name, file_size)
  content <- stringr::str_replace_all(content, "inst/doc\n", "")
  writeChar(content, file_name, (nchar(content)), eos = NULL)
}

#' build
#' 
#' Takes special care of vignettes
#' 
#' @author Reinhard Simon
#' @family convenience
#' @export
build <- function() {
  # build vignettes
  vig_lst <- list.files("vignettes", pattern = "*.Rmd")
  sapply(vig_lst, do_knit, "vignettes")
  # copy over to inst/doc
  fls_lst <- file.path("vignettes", list.files("vignettes"))
  if (!file.exists("inst/doc")) {
    dir.create("inst/doc")
  }
  file.copy(fls_lst, file.path("inst", "doc"), overwrite = TRUE, recursive = TRUE)
  
  # devtools build devtools::build()
  
  # format
  format_code()
  undo_instdoc_gi()
} 
