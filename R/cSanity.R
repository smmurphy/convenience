#' Column Sanity
#' 
#' This function displays the targeted data frame or data table with all columns 
#' with greater than a defined percentage of missing data (default 50\%) removed. 
#' @param x A data frame or data table. 
#' @param prct The percent of blank or NA data to be used as a criteria for removal.
#' Default is 50\%.
#' @param view Show the data using the view command? Default is TRUE.
#' @param return Return the data as a data table with the columns removed? 
#' Default is FALSE.
#' @param message Display the message saying how many columns were removed? 
#' Default is TRUE.
#' @keywords Column Reduction
#' @export
#' @examples 
#' cSanity()

cSanity <- function(x, prct = 0.5, view = TRUE, return = FALSE, message = TRUE){
  x <- as.data.table(x)
  orig <- dim(x)[2]
  x <- sapply(x, as.character)
  x <- apply(x, 2, function(x) gsub("^$|^[:space:]$", NA, x))
  x <- data.table(x[, which(!(colMeans(is.na(x)) > prct))])
  if(view){
    View(x)
  }
  if(return){
    return(x)
  }
  if(message){
    end <- dim(x)[2]
    elim <- orig - end
    print(paste0("cSanity eliminated ", elim, ' columns from this data set.'))
  }
}
