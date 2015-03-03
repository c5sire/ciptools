adjust_author <- function(pkg=".") {
  fp <- file.path(pkg,"DESCRIPTION")
  fs <- file.info(fp)[['size']]
  ch <- readChar(fp, fs) 
  if(!stringr::str_detect(ch,"@R")){
    ch <- stringr::str_replace(ch, "Author", "Authors@R")
    writeChar(ch, fp, fs+3, eos=NULL)
  }
}


adjust_readme <- function(title, description, pkg="."){
  devtools::use_readme_rmd()
  fp <- file.path(pkg,"README.Rmd")
  fs <- file.info(fp)[['size']]
  ch <- readChar(fp, fs) 
  
  td <- paste0("-->\n\n# ", title,"\n\n", description)
  ch <- stringr::str_replace(ch, "-->", td)
  
  # Add continuous integration and testing banners
  travis <- "[![Travis-CI Build Status](https://travis-ci.org/{{github_user}}/{{package}}.png?branch=master)](https://travis-ci.org/{{github_user}}/{{package}})"
  appveyor <- "[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/{{github_user}}/{{package}}?branch=master)](https://ci.appveyor.com/project/{{github_user}}/{{package}})"
  covrall <- "[![Coverage Status](https://img.shields.io/coveralls/{{github_user}}/{{package}}.svg)](https://coveralls.io/r/{{github_user}}/{{package}}?branch=master)"
  tpl = paste(travis, appveyor, covrall)
  github_user <- get_git_user()
  package <- basename(getwd())
  
  txt <- whisker::whisker.render(tpl)
  
  td <- paste0(txt, "\n\n<--")
  ch <- stringr::str_replace(ch, "<--", txt)
  
  nc <- nchar(ch)
  writeChar(ch, fp, nc, eos=NULL)
}

#' Add to description
#' 
#' Add a field to the description file. This function is a wrappe around a function 
#' from the devtools package.
#' 
#' @param field the field name
#' @param name the content of the field
#' @param pkg the package directory
#' @export
add_description <- function(field="Package", 
                            name=basename(getwd()), 
                            pkg="."){
  add_desc_package(pkg, field, name)
}

#' Replace a description
#' 
#' Replace a field content in he description file. 
#' 
#' @param field the field name
#' @param name the content of the field
#' @param pkg the package directory
#' @export
replace_description <- function(field, name, pkg="."){
  replace_desc_package(pkg, field, name)
}



#' new_description
#' 
#' Adds a description file in CIP format
#' 
#' @param adir a directory
#' @param title for DESCRIPTION and README files
#' @param description for DESCRIPTION and README files
#' @param name_first of author
#' @param name_last of author
#' @param email reference email. Must match.
#' @param copyright Copyright holder.
#' @param license Standard license.
#' @return logical success
#' @author Reinhard Simon
#' @export
new_description <- function(adir = ".", 
                            title = "My Productivity Tool (Use Title Case)",
                            description = "Be a bit more specific",
                            name_first="First",
                            name_last = "Last",
                            email = "first.last@email.com",
                            role = c("aut", "cre"),
                            copyright = "International Potato Center",
                            license = "MIT + file LICENSE"
                                  ) {
  #x <- devtools::create_description(adir, extra=list("Version: 0.0.1.9000"))
  adjust_author(adir)
  replace_description("Version", "0.0.1.9000")
  replace_description("Title", title)
  replace_description("Description", description)
  
  replace_description("License", license)
  if(license == "MIT + file LICENSE") {
    file.copy(system.file("templates/LICENSE", package="ciptools"), adir)
  }
  add_description("Copyright", 
                  paste0(copyright," (",format(Sys.time(),"%Y"),")"))
  
  
  # Replace the following with a routine to work with a person object!
  tpl <- "c(person('{{name_first}}', '{{name_last}}', '{{email}}', role = c('aut', 'cre')))"
  txt <- whisker::whisker.render(tpl)
  replace_description("Authors@R", txt)
  
#   tpl <- "{{name_first}} {{name_last}} <{{email}}>"
#   txt <- whisker::whisker.render(tpl)
#   replace_description("Maintainer", txt)
  
  add_description("Date", format(Sys.time(),"%Y-%m-%d"))
  
  update_git() # also adds support for continuous integration and testing
  github_user <- get_git_user()
  if(!is.null(github_user)){
    packageName <- basename(getwd())
    tpl <- "http://github.com/{{github_user}}/{{packageName}}"
    txt <- whisker::whisker.render(tpl)
    add_description("URL", txt)
    
    tpl <- "http://github.com/{{github_user}}/{{packageName}}/issues"
    txt <- whisker::whisker.render(tpl)
    add_description("BugReports", txt)
  }
} 





