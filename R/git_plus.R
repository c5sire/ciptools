

do_git <- function(full, path = ".", quiet = TRUE) {
  result <- devtools::in_dir(path, system(full, intern = TRUE, ignore.stderr = quiet))
  status <- attr(result, "status") %||% 0
  if (!identical(as.character(status), "0")) {
    stop("Command failed (", status, ")", call. = FALSE)
  }
  result
}

add_github_user <- function(user, email, path = ".") {
  do_git(paste0("git config --global user.name \"", user, "\""), path)
  do_git(paste0("git config --global user.email \"", email, "\""), path)
}

get_git_user <- function() {
  x <- do_git("git config user.name")
  stringr::str_replace_all(x, "'", "")
}

add_github <- function() {
  remote_list <- do_git("git remote -v")
  if (length(remote_list) > 0 & stringr::str_detect(remote_list[1], "origin")) {
    message("This package has already remote site(s) registered.\n
            Use git command line tools to manage details.")
    return(remote_list)
  }
  template <- "git remote add origin https://github.com/{{user}}/{{package}}.git"
  cmd <- whisker::whisker.render(template,
                                 list(user = get_git_user(),
                                      package = basename(getwd()))
                                 )
  do_git(cmd)
}

# Initiate local git repository
# 
# Executes git init
# 
# @param path to package
# @author Reinhard Simon
# export
add_git <- function(path = ".") {
  do_git("git init", path)
}

update_git <- function(pkg = ".") {
  # if no git: add a new one
  if (!uses_git(pkg)) {
    add_git(pkg)
  }
  # if has git: add a standard remote repo on github
  add_github()
  try(devtools::use_travis())
  try(devtools::use_appveyor())
  try(use_coveralls())
}

#' first_commit
#'
#' Does a first round of adding all files to git and pushing to github.
#'
#' @param pkg path to pkg
#' @param msg a message
#' @author Reinhard Simon
#' @export
first_commit <- function(pkg = ".", msg = "Initial commit.") {
  try(knitr::knit("README.Rmd"))
  try(devtools::in_dir("vignettes",
    knitr::knit("tutorial.Rmd", "tutorial.html")
    ))
#   txt <- readLines("README.md")
#   txt <- paste(txt, collapse = "\n")
#   txt <- stringr::str_replace(txt, "---(.*?)---", "")
#   writeLines(txt, "README.md", sep = "")
  try(roxygen2::roxygenise(pkg))
  do_git("git add -A", pkg)
  do_git(paste0("git commit -m \"", msg, "\""), pkg)
  # Check if internet connection 
  # Check if github repository exists
  # do_git('git push -u origin master', pkg, quiet=FALSE)
}
