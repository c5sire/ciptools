#' hello
#' 
#' The typical hello world example.
#' 
#' @param name a name; default is 'world'
#' @return character a string; default is 'Hello, world!'
#' @author Reinhard Simon
#' @family example
#' @example inst/examples/ex_hello.R
#' @export
hello <- function(name = "world"){
  stopifnot(is.character(name))
  paste0("Hello, ", name, "!")
}
