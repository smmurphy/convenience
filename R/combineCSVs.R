#' Combine CSVs
#' 
#' This function takes a directory of csv's with similar structure
#' and combines them into a single data table.  
#' @param path Path where files to be read can be found.
#' @param files Specific files to be read at the specified path.
#' @keywords Column Reduction
#' @export 
#' @examples 
#' combineCSVs()

combineCSVs <- function(path = NULL, files = NULL) {
  if (is.null(files)) {
    files <- dir(path = path, pattern = '.*\\.csv')
    files <- paste0(path, files)
  } else {
    files <- paste0(path, files)
  }
  collection <- data.table::data.table()
  for (f in files) {
    m <- data.table::fread(f, integer64 = "character")
    collection <- plyr::rbind.fill(collection, m)
  }
  return(collection)
}