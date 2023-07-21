#' check_window_length
#'
#' @param profiles 
#' @param windowLength 
#'
#' @return length of profiles that are longer than the windowLength
#' @noRd
check_window_length <- function(profiles, windowLength){
  profile_length <- do.call(c, lapply(profiles, function(p){
    max(p[,1])
  }))
  
  window_is_longer <- profile_length < windowLength
  
  if ( any(window_is_longer) ){
    
    length_too_long <- profile_length[window_is_longer]
    profile_name <- names(profiles[window_is_longer])
    
    out <- data.frame(profile_name = profile_name,
                      length = length_too_long,
                      stringsAsFactors = FALSE)
    
    return(out)
  }
  
  
  return(NULL)
}
