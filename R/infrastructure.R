# This fragment is from Jim Heesters pull request
# https://github.com/jimhester/devtools/blob/65041349864cae5205036572afd9ee0f7b8a0272/R/infrastructure.R
#

#' @rdname infrastructure
#' @section \code{use_coveralls}:
#' Add coveralls to basic travis template to a package.
#' @export
use_coveralls <- function(pkg = ".") {
  pkg <- as.package(pkg)
  path <- file.path(pkg$path, ".travis.yml")
  if (!file.exists(path)) {
    stop(".travis.yml does not exist, please run `use_travis()` to create it", call. = FALSE)
  }
  travis_content <- readLines(file.path(pkg$path, ".travis.yml"))
  if (any(grepl("covr::coveralls()", travis_content))) {
    stop("coveralls information already added to .travis.yml", call. = FALSE)
  }
  gh <- github_info(pkg)
  message("Adding coveralls information into .travis.yml for ", pkg$package, ". Next: \n",
          " * Turn on coveralls for this repo at https://coveralls.io/repos/new\n",
          " * Add a coveralls shield to your README.md:\n",
          "[![Coverage Status]",
          "(https://img.shields.io/coveralls/", gh$username, "/", gh$repo, ".svg)]",
          "(https://coveralls.io/r/", gh$username, "/", gh$repo, "?branch=master)"
  )
  install_loc <- grep("^install:$", travis_content)
  travis_content <- append(travis_content, " - ./travis-tool.sh github_package jimhester/covr", after = install_loc)
  after_failure_loc <- grep("^after_failure:$", travis_content)
  travis_content <- append(travis_content,
                           c("after_success:",
                             " - Rscript -e 'covr::coveralls()'"),
                           after = after_failure_loc - 1)
  writeLines(travis_content, file.path(pkg$path, ".travis.yml"))
}


add_desc_package <- function(pkg = ".", field, name) {
  pkg <- as.package(pkg)
  desc_path <- file.path(pkg$path, "DESCRIPTION")
  desc <- read_dcf(desc_path)
  old <- desc[[field]]
  if (is.null(old)) {
    new <- name
    changed <- TRUE
  } else {
    if (!grepl(name, old)) {
      new <- paste0(old, ",\n ", name)
      changed <- TRUE
    } else {
      changed <- FALSE
    }
  }
  if (changed) {
    desc[[field]] <- new
    write_dcf(desc_path, desc)
  }
  invisible(changed)
}

