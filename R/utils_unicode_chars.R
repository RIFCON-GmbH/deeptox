#' Unicode definition of mu
#'
#' Search with regex '[^\x00-\x7F]+' to find non-ASCII characters.
#' Use the unicode definition instead
#'
#' @return mu as unicode character
#' @noRd
mu_char <- function(){
  "\U00B5"
}

#' Unicode definition of kappa
#'
#' Search with regex '[^\x00-\x7F]+' to find non-ASCII characters.
#' Use the unicode definition instead
#'
#' @return kappa as unicode character
#' @noRd
kappa_char <- function(){
  "\U03BA"
}
