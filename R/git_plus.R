do_git <- function (full, path = ".",  quiet=TRUE) {
  result <- in_dir(path, system(full, intern = TRUE, ignore.stderr = quiet))
  status <- attr(result, "status") %||% 0
  if (!identical(as.character(status), "0")) {
    stop("Command failed (", status, ")", call. = FALSE)
  }
  result
}

add_github_user <- function(user, email, path = "."){
  x <- do_git(paste0('git config --global user.name "', user, '"'), path  )
  x <- do_git(paste0('git config --global user.email "', email, '"'), path  )
}

get_git_user <- function() {
  x = do_git("git config user.name")
  stringr::str_replace_all(x, "'", "")
}



add_github <- function(){
  remote_list <- do_git("git remote -v")
  if(stringr::str_detect(remote_list[1], "origin")) {
    message("This package has already remote site(s) registered. 
         Use git command line tools to manage details.")
    return(remote_list)
  }
  template <- "git remote add origin https://github.com/{{user}}/{{package}}.git"
  cmd <- whisker.render(template, list(user=get_git_user(), package=basename(getwd())))
  x <- do_git(cmd)
}

add_git <- function(path = ".") {
  x <- do_git("git init", path)  
}




