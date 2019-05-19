#' Make random unique pairings from a list
#' 
#' @description 
#' \code{randomPairs()}
#' Takes a list of elements and pairs each element randomly with another element
#' from the same list to create unique pairs.  Elements are not paired with themselves
#' nor are they paired with items that have already been paired.
#' 
#' @param list Vector of the items to be paired.
#' @param lname Name for the left column of the returned data frame.
#' @param rname Name for the right column of the returned data frame.
#' 
#' @keywords unique random pairs, random pairs
#' @export
#' @examples 
#' randomPairs()

randomPairs <- function(list = letters, lname = 'l', rname = 'r'){
  list <- as.character(list)
  set <- data.frame()
  for(e in list){
    test <- list[list != e & !list %in% set$r]
    if(length(test) > 0){
      pair <- sample(test,1)
      set[e,'l'] <- e
      set[e,'r'] <- pair
    }
  }
  names(set) <- c(lname,rname)
  return(set)
}
