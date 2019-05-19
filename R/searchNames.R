#' Search Names of Data Frame / Data Table
#'
#' Reads the names of a data frame or data table and lists them in a searchable
#' DT datatable along with information about the number and  
#' percent of entries that have non missing values. 
#' @param df The dataframe, or datatable to search names of.
#' @keywords variables, column names, search
#' @export
#' @examples 
#' searchNames()

searchNames <- function(df){
  df <- data.table::data.table(df)
  df <- data.frame(
    'N' = colSums(!is.na(df)),
    'prct' = colSums(!is.na(df)) / dim(df)[1] * 100)
  df[['var']] <- row.names(df)
  df <- df[,c('var','N','prct')]
  names(df) <- c('Variable','N','%')
  DT::datatable(df, 
                rownames = FALSE,
                extensions = c('FixedHeader', 'Buttons'),
                options = list(
                pageLength = 2000,
                lengthMenu = c(20, 50, 100, 200, 500, 2000),
                dom = 'Bfrtip',
                fixedHeader = TRUE,
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
}
