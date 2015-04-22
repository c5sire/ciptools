#' {{title}}
#'
#' A one liner.
#' 
#' Here more details
#'
#' @param name a name; default is 'world'
#' @return character a string; default is 'Hello, world!'
#' @author author
#' @family example
#' @example inst/examples/ex_{{title}}.R
#' @export
{
  {
    title
  }
} <- function(name = "world") {
  stopifnot(is.character(name))
  paste0("Hello, ", name, "!")
} 
