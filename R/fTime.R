#' Formatted Time
#' 
#' This function takes seconds as an input and returns time in H:M:S format.
#' The time is treated as total time with hours as the maximum unit.
#' @param x Time in seconds
#' @param excelToSec If True the time in seconds is treated as MS Excel 
#' formatted time which is the time in seconds / (24*60*60). To compensate 
#' the input is multiplied by (24*60*60). Default is False.
#' @keywords Time, Seconds, Formatted
#' @export
#' @examples 
#' fTime()

fTime <- function(x, excelToSec = FALSE){
  
  sign <- ifelse(x < 0,-1,1)
  x <- abs(x)
  
  if (excelToSec == TRUE){
    x <- x * (24 * 60 * 60)
  }
  
  hrs <- floor(x / 3600)
  min <- floor((x - (hrs * 3600)) / 60)
  sec <- round((x - ((hrs * 3600) + (min * 60)))) 
  
  return(paste0(ifelse(sign == -1,'-',''),sprintf("%02d",hrs), ':', 
                sprintf("%02d", min), ':', sprintf("%02d", sec)))
}