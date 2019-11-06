#' Write delimited data to the clipboard.
#'
#' \code{writeClip()} takes data from R and writes it to the clipboard in a
#' delimited format. Optionally output can be formatted to paste as a table
#' using kable.
#'
#' @param sep Separator for the delimited data, tab is the default.
#' @param row.names True if row names should be displayed.
#' @param col.names True if column names should be displayed.
#' @param quote Should the output be quoted.
#' @param kable If True returns table formatted by kable.
#' @param format The format for the kable table if True. Default is
#' markdown.
#' @keywords read clipboard
#' @export
#' @examples
#' writeClip()

writeClip <-
  function(x,
           row.names = FALSE,
           col.names = TRUE,
           sep = "\t",
           quote = TRUE,
           kable = FALSE,
           format = 'markdown',
           size = 2^20,
           ...) {

    if (kable) {
      x <- knitr::kable(x, format = format)
      sep <- "\n"
      quote <- FALSE
    }

    if (grepl('windows', Sys.info()[['sysname']], ignore.case = TRUE)) {
      # If the row only has 1 dimension transpose it.
      if (!is.null(dim(x))) {
        write.table(
          x,
          file = paste0("clipboard-", size),
          sep = sep,
          quote = quote,
          row.names = row.names,
          col.names = col.names,
          ...
        )
      } else {
        write.table(
          t(x),
          file = paste0("clipboard-", size),
          sep = sep,
          quote = quote,
          row.names = FALSE,
          col.names = FALSE,
          ...
        )
      }

    } else if (grepl('mac', Sys.info()[['sysname']], ignore.case = TRUE)) {
      clip <- pipe("pbcopy", "w")
      on.exit(close(clip))

      # If the row only has 1 dimension transpose it.
      if (!is.null(dim(x))) {
        write.table(
          x,
          sep = sep,
          quote = quote,
          row.names = row.names,
          col.names = col.names,
          file = clip,
          ...
        )
      } else{
        write.table(
          t(x),
          sep = sep,
          quote = quote,
          row.names = FALSE,
          col.names = FALSE,
          file = clip,
          ...
        )
      }
    } else if (grepl('linux', Sys.info()[['sysname']], ignore.case = TRUE)) {
      clip <- pipe(paste0("xclip -selection c"), "w")
      on.exit(close(clip))

      # If the row only has 1 dimension transpose it.
      if (!is.null(dim(x))) {
        write.table(
          x,
          sep = sep,
          quote = quote,
          row.names = row.names,
          col.names = col.names,
          file = clip,
          ...
        )
      } else{
        write.table(
          t(x),
          sep = sep,
          quote = quote,
          row.names = FALSE,
          col.names = FALSE,
          file = clip,
          ...
        )
      }
    }
  }
