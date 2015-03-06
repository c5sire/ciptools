# Assume a new R project has been created including git and user is under this working directory Check if files present
# (including git dir); if not create them!  Make adjustements to DESCRIPTION file etc when finished make a commit to git
# and try to push to git repository. Check for internet connectivity, check for presence of repository on github
# https://developer.github.com/v3/repos/#list-your-repositories GET /users/:username/repos POST /user/repos and push.
# Remaining things to do for user: register on three additional sites and manage package

use_template <- function(tpl, dest = tpl, github_user = get_git_user(), package = get_pkg_name("."), title = "A Title In Title Case", 
  description = "A thorugh description", adate = format(Sys.time(), "%Y-%m-%d")) {
  txt <- readLines(system.file(file.path("templates", tpl), package = "ciptools"))
  txt <- paste(txt, collapse = "\n")
  txt <- whisker::whisker.render(txt)
  write(txt, file = dest)
}

get_pkg_name <- function(pkg = ".") {
  basename(normalizePath(pkg))
} 
