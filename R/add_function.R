#' add_function
#' 
#' Adds a new user visible function
#' 
#' @param title a name for the function
#' @author Reinhard Simon
#' @family convenience
#' @export
add_function <- function(title) {
  use_template("tpl_fun.R", file.path("R", paste0(title, ".R")), title = title)
  try(use_template("tpl_exp.R", file.path("inst", "examples", paste0("ex_", title, ".R")), title = title))
  try(use_template("tpl_tst.R", file.path("tests", "testthat", paste0("test_", title, ".R")), title = title))
} 
