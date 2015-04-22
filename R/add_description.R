adjust_author <- function(pkg = ".") {
  fp <- file.path(pkg, "DESCRIPTION")
  fs <- file.info(fp)[["size"]]
  ch <- readChar(fp, fs)
  if (!stringr::str_detect(ch, "@R")) {
    ch <- stringr::str_replace(ch, "Author", "Authors@R")
    writeChar(ch, fp, fs + 3, eos = NULL)
  }
}

adjust_readme <- function(title, description, pkg = ".") {
  if (file.exists("README.Rmd")) {
    message("README.Rmd exists already.\nConsider deleting and consolidating to start from scratch!")
    return()
  }
  devtools::use_readme_rmd()
  package <- get_pkg_name(pkg)
  gh <- github_info(pkg)
  github_user <- gh$username
  use_template("README.Rmd", dest = "README.Rmd", 
               package = package, 
               title = title, 
               description = description, 
               github_user = github_user)
  
}

to_author <- function(persons) {
  txt <- "as.person(c(\n"
  n <- length(persons)
  for (i in 1:n) {
    txt <- paste(txt, "   \"", persons[i], "\"", sep = "")
    if (i < n) {
      txt <- paste0(txt, ",")
    }
    txt <- paste0(txt, "\n")
  }
  paste0(txt, "  ))")
}
# Add to description
# 
# Add a field to the description file. This function is a wrappe around a function 
# from the devtools package.
# 
# @param field the field name
# @param name the content of the field
# @param pkg the package directory
# export
add_description <- function(field = "Package", name = basename(getwd()), pkg = ".") {
  add_desc_package(pkg, field, name)
}

# Replace a description
# 
# Replace a field content in he description file. 
# 
# @param field the field name
# @param name the content of the field
# @param pkg the package directory
# @export
replace_description <- function(field, name, pkg = ".") {
  replace_desc_package(pkg, field, name)
}

use_inst <- function(pkg) {
  if (!file.exists(file.path(pkg, "inst"))) {
    dir.create(file.path(pkg, "inst"))
    dir.create(file.path(pkg, "inst", "examples"))
  }
}

#' use_citation
#' 
#' Adds a CITATION file to the inst sub-directory. The template
#' creates a citation similary to the auto-generated one. However, 
#' this file may be customized.
#' 
#' @param pkg the package directory
#' @return boolean
#' @author Reinhard Simon
#' @export
use_citation <- function(pkg = ".") {
  dest <- file.path(pkg, "inst", "CITATION")
  if (file.exists(dest)) {
    message("The file 'CITATION' already exists.\nEdit or delete it manually.")
    return(FALSE)
  }
  use_inst(pkg)
  file.copy(system.file("templates/tpl_CITATION", package = "ciptools"), dest)
}

adjust_package_desc <- function(pkg, title, description) {
  package <- basename(normalizePath(pkg))
  pnm <- paste(package, "_package.R", sep = "")
  out <- file.path("R", pnm)
  if (file.exists(out)) {
    message(paste(out, "already exists! Adjust manually.\n"))
    return(FALSE)
  }
  tpl <- readChar(system.file("templates/package.R", package = "ciptools"), nchars = 200)
  txt <- whisker::whisker.render(tpl)
  txt <- stringr::str_replace_all(txt, "\\n", "")
# print(str(txt)) print(txt)
  cat(paste(txt, collapste = ""), file = out, fill = TRUE)
}


#' new_description
#'
#' Adds a description file in CIP format
#'
#' @param adir a directory
#' @param title for DESCRIPTION and README files
#' @param description for DESCRIPTION and README files
#' @param persons a vector of persons
#' @param copyright Copyright holder.
#' @param license Standard license.
#' @return logical success
#' @author Reinhard Simon
#' @export
new_description <- function(
  adir = ".", 
  title = "My Productivity Tool (Use Title Case)", 
  description = "Be a bit more specific!", 
  persons = c(
    person("First", "Last", "M", "first.last@email.com", 
      role = c("aut", "cre"))), 
  copyright = "International Potato Center", 
  license = "MIT + file LICENSE") {
  # x <- devtools::create_description(adir, extra=list('Version: 0.0.1.9000'))
  ayear <- format(Sys.time(), "%Y")
  adjust_author(adir)
  replace_description("Version", "0.0.1.9000")
  replace_description("Title", title)
  replace_description("Description", description)
  
  replace_description("License", license)
  if (license == "MIT + file LICENSE") {
    file.copy(system.file("templates/LICENSE", package = "ciptools"), adir)
    tpl <- readChar("LICENSE", nchars = 100)
    txt <- whisker::whisker.render(tpl)
    writeChar(txt, "LICENSE", nchars = nchar(txt), eos = NULL)
  }
  #replace_description("Copyright", paste0(copyright, " (", ayear, ")"))
  
  
  replace_description("Authors@R", to_author(persons))
  
  txt <- paste0(persons[1]$given[1], " ", persons[1]$family, " <", persons[1]$email, ">")
  cat(txt)
  #replace_description("Maintainer", txt)
  
  replace_description("Date", format(Sys.time(), "%Y-%m-%d"))
  
  use_citation(adir)
  
  adjust_package_desc(adir, title, description)
  
  update_git()  # also adds support for continuous integration and testing
  gh <- github_info()
  github_user <- gh$username
  if (!is.null(github_user)) {
    packageName <- basename(getwd())
    tpl <- "http://github.com/{{github_user}}/{{packageName}}"
    txt <- whisker::whisker.render(tpl)
    add_description("URL", txt)
    
    tpl <- "http://github.com/{{github_user}}/{{packageName}}/issues"
    txt <- whisker::whisker.render(tpl)
    add_description("BugReports", txt)
    adjust_readme(title, description, adir)
  }
  use_template("NEWS.md")
  devtools::use_build_ignore("NEWS.md")
}

#' format_code
#' 
#' A convenience wrapper to format R code according to CIP standards for R 
#' (largely based on H. Wickham/GNU C coding) conventions.
#' 
#' @param indent integer; 2 spaces indentation
#' @param arrow logical; TRUE default means use arrows for assignments
#' @param recursive logical; TRUE default means that anything in R code files will be formatted.
#' @author Reinhard Simon
#' @export
format_code <- function(indent = 2, arrow = TRUE, recursive = TRUE) {
  formatR::tidy_dir(recursive = recursive, arrow = arrow, indent = indent)
}

# add_tests
# 
# Adds testthat files, examples and vignettes - all different means of 
# checking correctness.
# 
# @param pkg the path to the package
# @author Reinhard Simon
# export
add_tests <- function(pkg = ".") {
  # vignette example
  try({
    devtools::use_vignette("tutorial", pkg)
    # code example
    use_inst(pkg)
    dest <- file.path(pkg, "R", "hello.R")
    file.copy(system.file("templates/hello.R", package = "ciptools"), dest, overwrite = TRUE)
    file.copy(system.file("templates/hello_world.R", package = "ciptools"), "R/hello_world.R", overwrite = TRUE)
    dest <- file.path(pkg, "inst", "examples", "ex_hello.R")
    file.copy(system.file("templates/ex_hello.R", package = "ciptools"), dest)
    dest <- file.path(pkg, "inst", "examples", "ex_hello_world.R")
    file.copy(system.file("templates/ex_hello_world.R", package = "ciptools"), dest)
    
    # tests
    devtools::use_testthat()
    # testthat
    dest <- file.path(pkg, "tests", "testthat", "test_hello.R")
    file.copy(system.file("templates/test_hello.R", package = "ciptools"), dest)
    dest <- file.path(pkg, "tests", "testthat", "test_hello_world.R")
    file.copy(system.file("templates/test_hello_world.R", package = "ciptools"), dest)
    format_code()
  })
}

#' use_cip
#' 
#' Add CIP standards
#' 
#' @param pkg path to package
#' @param persons a vector of persons
#' @param title a title
#' @param description a description
#' @author Reinhard Simon
#' @export
use_cip <- function(
  pkg = ".",
  persons = c(
    person("Reinard", "Simon", , "r.simon@cgiar.org", 
      role = c("aut", "cre"))
    
  ), 
  title = "A Good Title",
  description = "A thorough description.") {
  # try(devtools::create(pkg))
  persons = c(persons, person("International Potato Center", "(CIP)", 
                              role = c("cph")))
  setwd(pkg)
  package <- get_pkg_name(pkg)
  if(!file.exists("DESCRIPTION")){
    use_template("tpl_DESCRIPTION", dest = "DESCRIPTION", package=package)#, 
#                  package = pkg, 
#                  title = title, 
#                  description = description, 
#                  github_user = github_user
  }
  if(!file.exists("R")){
    dir.create("R")
  }
  if(!file.exists(".Rbuildignore")){
    use_template("Rbuildignore", ".Rbuildignore")
  }

  new_description(".", title = title, description = description, persons = persons)
  add_tests(pkg = pkg)
  build()
  first_commit(pkg)
}

# Remaining todo retrieve authorR consolidate author names in one
# add all copyight holders to list MIT license file
#
# Add NEWS.md template; add it to Rignore:
# devtools::use_build_ignore('NEWS.md') 
# Add some text of usage to README.Rmd shiny app to make a 
# form to edit docs

# handle case from github with no files: 
# add R directory and DESCRIPTION file
