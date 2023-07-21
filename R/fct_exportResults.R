#' checkResultsComplete
#'
#' @param params 
#'
#' @return TRUE if params is complete; FALSE if something is missing
#'
#' @examples
#' data("deeptox_ExampleDataset")
#' @noRd
checkResultsComplete <- function(params){
  hasContent <- do.call(c, lapply(params, length))
  allHaveContent <- all(hasContent > 0)
  if (!allHaveContent)
    return(FALSE)
  
  
  
  return(TRUE)
  
  
}