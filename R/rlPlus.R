#' Read Lines Plus
#' 
#' @description 
#' \code{rlPlus()}
#' returns a list of lines or a data frame with 
#' each line as a column and each position as a row.
#' 
#' @param path The filename from which to read lines.
#' @param n The number of lines to read -L for all.
#' @param sep Specify to parse the line by a delimeter, "," is default.
#' @param merge If TRUE (default) merges lines into a data.frame by position as 
#' row and line number as column.
#' @param ... Other values passed to readLines, or to read.table.
#' @keywords read lines, parse lines with different lengths, compare parsed lines
#' @export
#' @examples 
#' rlPlus()

rlPlus <- function(path = NULL,
                   n = 2,
                   sep = ",",
                   merge = TRUE,
                   ...) {
  if (is.null(path)) {
    print('Please enter a file name to read in.')
  } else {
    con <- file(description = path, open = "r")
  }
  lns <- readLines(con = con, n, ...)
  close(con)
  if (!is.null(sep)) {
    lnsout <- list()
    for (i in 1:length(lns)) {
      # Parse the row by separator
      block <- t(read.table(
        text = lns[i],
        sep = sep,
        stringsAsFactors = FALSE,
        ...
      ))
      # Transpose to df with ids
      block <-
        data.frame(
          'position' = row.names(block),
          'value' = block[, 1],
          stringsAsFactors = FALSE
        )
      # Add to list of rows
      lnsout[[i]] <- block
    }
  } else {
    lnsout <- lns
  }
  # Merge lines by parsed position
  if (merge == TRUE) {
    lnsout <- suppressWarnings(Reduce(function(x, y)
      merge(x, y, by = "position"), lnsout))
    lnsout[['position']] <-
      as.integer(gsub('[A-Z]', '', lnsout[['position']]))
    names(lnsout) <-
      c('position', paste0('line_', seq(1, dim(lnsout)[2] - 1)))
    lnsout <- lnsout[order(lnsout[['position']]), ]
  }
  return(lnsout)
}
