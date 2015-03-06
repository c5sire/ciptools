# This is from H. Wickhams devtools package utils.r
# https://github.com/hadley/devtools/blob/fa1fda4f2003644dc4c52d90db256f85a1a823ea/R/utils.r

"%||%" <- function(a, b) if (!is.null(a)) a else b

read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}
write_dcf <- function(path, desc) {
  text <- paste0(names(desc), ": ", desc, collapse = "\n")
  if (substr(text, nchar(text), 1) != "\n") {
    text <- paste0(text, "\n")
  }
  cat(text, file = path)
} 
