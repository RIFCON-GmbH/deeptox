#' serializeToString
#'
#' @param vec the vector to be serialized
#' @param finalSepType 1: "and"; 2: ", and"; 3: ","
#' @param conj which conjunction to use. "and" as default. Alternative: "or"
#'
#' @return a single string serialized from the vector 
#'
#' @examples 
#' serializeToString(letters[1:10])
#' serializeToString(letters[1:10], finalSepType=2)
#' serializeToString(letters[1:10], finalSepType=3)
#' serializeToString(letters[1:10], conj="or")
#' @noRd
serializeToString <- function(vec, finalSepType=1, conj="and"){
  
  if (!(finalSepType %in% 1:3))
    warning("Choose only 1, 2 or 3 for the seperator type.")
  
  n <- length(vec)
  finalSep <- switch(finalSepType,
    '1' = paste0(" ",conj),
    '2' = paste0(", ",conj),
    '3' = ","
  )

  if ( n < 1 ){
    return(vec)
  } else {
    part1 <- paste(vec[1:(n-1)], collapse=", ")
    out <- paste0(part1,finalSep," ",vec[n])
  }
  
  return(out)
  
}


#' pluralS
#'
#' @param x 
#' @param countItems 
#'
#' @return 's' if plural, '' if singular
#'
#' @examples
#' vec <- 1:10
#' n <- length(vec)
#' s <- pluralS(n)
#' print(paste0(n," Element",s))
#' 
#' vec <- 1
#' n <- length(vec)
#' s <- pluralS(n)
#' print(paste0(n," Element",s))
#' 
#' vec <- 1:10
#' s <- pluralS(vec, countItems = TRUE)
#' print(paste0(n," Element",s))
#' 
#' vec <- 1
#' s <- pluralS(n)
#' s <- pluralS(vec, countItems = TRUE)
#' print(paste0(n," Element",s))
#' @noRd
pluralS <- function(x, countItems=FALSE){
  if (countItems)
    n <- length(x)
  else
    n <- x

  if (n >1)
    out <- "s"
  else
    out <- ""
  
  return(out)
}
