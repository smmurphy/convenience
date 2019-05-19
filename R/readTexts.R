#' Read text, rtf, or docx files into a dataframe and convert to tidy text format.
#'
#' \code{readTexts()} takes a directory as an input, loops through all files with 
#' a txt or a docx extension, reads each line into a dataframe with the document 
#' name as an identifier and optionally converts the file to tidy text format 
#' (one word per row).
#' 
#' @param directory The directory where files to be read are stored.
#' @param tidy Whether text should be returned in tidy (one word per row) format.
#' @keywords read docx, read txt, tidy text, read rtf
#' @export
#' @examples
#' readTexts()

readTexts <- function(directory, tidy = TRUE, split = NULL){
  
  docs <- dir(directory, full.names = TRUE)
  
  doc_list <- list()
  
  for(i in docs){
    if(grepl('docx$', i)){
      print(i)
      doc_list[[i]] <- qdapTools::read_docx(i)
    } else if(grepl('txt$', i)){
      print(i)
      doc_list[[i]] <- readLines(i)
    }  else if(grepl('rtf$', i)){
      print(i)
      doc_list[[i]] <- striprtf::read_rtf(i)
    }
  }
  df <- qdapTools::list2df(doc_list, col1 = 'line', col2 = 'document')
  df[['document']] <- gsub('\\..+$', '', basename(df[['document']]))
  if(!is.null(split)){
    sp <- strsplit(df[['line']], split, perl = TRUE)
  print(sp)
  }
  if(tidy){
    df <- tidytext::unnest_tokens(df, words, line, drop = FALSE)
  }
  return(df)
}
