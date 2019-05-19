#' Context
#' 
#' Find a substring and return with a buffer of n characters on either side.
#' @param term A regular expression to search for.
#' @param back The number of characters back to return.
#' @param forward The number of characters forward to return.
#' @param ignore.case Should case be ignored for the search. Default is TRUE.
#' @param highligh Should the returned expression be in CAPS? Default is TRUE.
#' @keywords search, grep, context, phrase
#' @export
#' @examples 
#' context()

context <- 
  function(term = '.*', 
           target = NULL, 
           back = 20, 
           forward = 20, 
           ignore.case = TRUE, 
           highlight = TRUE){
    
    if(!is.null(target)){
      matches <- target[grepl(term, target, ignore.case = ignore.case)]
      cPos <- unlist(gregexpr(pattern = term, matches))
      str <- substr(matches, 
                    start = ifelse(cPos - back < 1, 1, cPos - back), 
                    stop = ifelse(cPos + forward > length(matches), 
                                  length(matches), cPos + forward))
      
      if(highlight == TRUE){
        str <- gsub(paste0('(',term,')'), '\\U\\1', str, perl = TRUE)  
      }
      
      str <- paste0('...', str, '...')
    }
    return(str)
}
