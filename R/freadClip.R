#' Read delimited data from the clipboard.
#'
#' \code{freadClip()} takes data from the clipboard and reads it to a data.table.
#'
#' @keywords read clipboard
#' @export
#' @examples
#' freadClip()

freadClip <- function(...) {
  if (grepl('windows', Sys.info()[['sysname']], ignore.case = TRUE)) {
    clip <- "clipboard"
  } else if (grepl('mac', Sys.info()[['sysname']], ignore.case = TRUE)) {

  } else if (grepl('linux', Sys.info()[['sysname']], ignore.case = TRUE)) {
    clip <- pipe("xclip -selection clipboard -o", open = "r")
  }
  X <- tempfile()
  writeLines(readLines(clip), X)
  X <- data.table::fread(X, ...)
  if (dim(X)[2] == 1) {
    return(X[[1]])
  } else {
    return(X)
  }
}
