# has_description
# 
# Checks that the directory given has a DESCRIPTON file
# 
# @param adir a directory
# @return boolean true or false
# @export
# @author Reinhard Simon
has_description <- function(adir = ".") {
  file.exists(file.path(adir, "DESCRIPTION"))
} 
