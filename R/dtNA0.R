#' Replace NA in data table with 0
#' 
#' @param DT A data table to have NA replaced with 0.
#' @keywords data table, NA, replace, 0, zero
#' @export
#' @examples 
#' dtNA0()

dtNA0 <-  function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=0]
}
