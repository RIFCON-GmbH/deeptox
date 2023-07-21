
#' removeElementRV
#'
#' Removes an element from a reactiveValues object
#'
#' @param rv 
#' @param name 
#'
#' @source https://github.com/rstudio/shiny/issues/2439
#' @noRd
removeElementRV <- function(rv, name){
  .subset2(rv, "impl")$.values$remove(name)
}


#' cleanRV
#'
#' remove all elements from a reactiveValues object
#'
#' @param rv 
#' @noRd
cleanRV <- function(rv){
  rvlist <- rvtl(rv)
  lapply(names(rvlist), function(i) removeElementRV(rv, i))
}


#' listToReactiveValues
#'
#' An inverse function to reactiveValuesToList.
#' 
#'
#' @param list_object source list whose list elements are transferred to the reactiveValues object
#' @param rv_object the reactiveValues object that is filled with the data from list_object
#' @param cleanRV remove all previous elements from object before filling
#'
#' @return reactive values object 
#' @noRd
listToReactiveValues <- function(list_object, rv_object, cleanRV=F){
  
  if (cleanRV){
    cleanRV(rv_object)
  }
  
  lapply(names(list_object), function(list_name_i){
    rv_object[[list_name_i]] <- list_object[[list_name_i]]
  })
  return(1)
}




#' coerceTo
#'
#' Coercion on single string. Output data type is determined by data itself.
#' The function can differentiate between numeric and logical.
#' All other will result in string output or uncoerced data output.
#'
#' @param x a single value
#'
#' @return coerced value in corresponding data type or uncoerced data.
#' @examples
#' ex1 <- "TRUE"
#' out1 <- deeptox:::coerceTo(ex1)
#' class(out1)
#' 
#' ex2 <- "42"
#' out2 <- deeptox:::coerceTo(ex2)
#' out2
#' class(out2)
#' 
#' ex3 <- "3.1415"
#' out3 <- deeptox:::coerceTo(ex3)
#' out3
#' class(out3)
#' 
#' ex4 <- "Passer domesticus"
#' out4 <- deeptox:::coerceTo(ex4)
#' out4
#' class(out4)
#' @noRd
coerceTo <- function(x){
  
  if ( length(x) == 1 ){
    # numeric ####
    ## numeric with/without decimal or in scientific notation ####
    if( grepl("^\\d+\\.*\\d*$",x) | grepl("^[+-]?\\d(\\.\\d+)?[Ee][+-]?\\d+$",x) )
      return( as.numeric(x) )
    
    # logical ####
    if ( grepl("^(F|T|FALSE|TRUE)$", x, ignore.case = TRUE) )
      return( as.logical( toupper(x) ) )
    
    # string  ####
    return(x)
  } else {
    warning("Length supplied to 'coerceTo' is longer than 1. Only apply a single element. Input not coerced.")
    return(x)
  }
  
}




#' unlistInitParameters
#'
#' @param parList nested list of parameters
#'
#' @return multilevel list reduced by one level
#' 
#' @examples
#' mylist <- list(a=list(a1=1, a2=2), b=list(b1=3, b2=4))
#' deeptox:::unlistInitParameters(mylist)
#' @noRd
unlistInitParameters <- function(parList){
  out <- do.call(c, lapply(parList, function(x){
    as.list(x)
  }))
  names(out) <- sub("^.*\\.(.*)$","\\1", names(out))
  
  return(out)
}




#' roundNearestPow
#' 
#' Rounds to the nearest power of 'base' (default: 10)
#'
#' @param x the number to round
#' @param base base of the exponential number
#' @param direction round 'up' or 'down'
#'
#' @return numeric
#'
#' @examples
#' roundNearestPow(0.0003)
#' roundNearestPow(0.0003, direction="down")
#' roundNearestPow(42)
#' roundNearestPow(42, direction="down")
#' @noRd
roundNearestPow <- function(x, base=10, direction="up"){
  
  if (direction == "up")
    out <- base^(ceiling(log(x, base)))
  else if (direction == "down")
    out <- base^(floor(log(x, base)))
  else
    stop("Select only 'up' or 'down' for 'direction'")
  
  return(out)
}


#' get_time_human
#'
#' get a formatted string of the timestamp (exclude colons as they are invalid
#  characters in Windows filenames)
#'
#' @return human readable date and time 
#' 
#' @source https://github.com/daattali/shiny-server/tree/master/persistent-data-storage
#'
#' @examples
#' get_time_human()
#' @noRd
get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}





#' Checks if app is within an electron environment
#'
#' @return TRUE if within an electron environment; FALSE otherwise
#' @examples
#' deeptox:::check_within_electron()
#' @noRd
check_within_electron <- function(){
  x <- Sys.getenv('WITHIN_ELECTRON')
  
  if( nchar(x) ){ # it could be that the Sys.getenv returns an empty string ""
    if(x){
      return(TRUE)
    }else if (!x){
      return(FALSE)
    }else{
      stop("check_within_electron(): Sys.getenv('WITHIN_ELECTRON') is neither TRUE nor FALSE")
    }
  }else{
    return(FALSE)
  }
 
}



#' Flatten a list to first layer
#'
#' @param lst the input list 
#'
#' @return a list 
#' @export
#'
#' @examples
#' iris_list <- split(iris, iris$Species)
#' nested_list <- list(iris_list[1], list(iris_list[2], iris_list[3]))
#' nested_list
#' flatten_list(nested_list)
flatten_list <- function(lst) {
  flattened_list <- list()
  
  for (i in seq_along(lst)) {
    if ( !("list" %in% class(lst[[i]])) ) {
      flattened_list <- c(flattened_list, lst[i])
    } else {
      nested_list <- Recall(lst[[i]])
      flattened_list <- c(flattened_list, nested_list)
    }
  }
  return(flattened_list)
}


#' Make Syntactically Valid Names
#'
#' Make syntactically valid names out of character vectors.
#' Based on base::make.names function. Added functionality to include the 
#'
#' @param names_ character vector to be coerced to syntactically valid names. This is coerced to character if necessary.
#' @param replace_char the character to be used to replace the default "." of make.names. 
#' @param ... arguments passed to make.names
#'
#' @return A character vector of same length as names with each changed to a syntactically valid name, in the current locale's encoding.
#' @export
#'
#' @examples
#' strings <- c("a and b", "a-and-b")
#' # same results as make.names
#' make.names2(strings, unique = TRUE)
#' make.names(strings, unique = TRUE)
#' 
#' # replace character "_" instead of "."
#' make.names2(strings, replace_char = "_", unique = TRUE)
make.names2 <- function(names_, replace_char = ".", ...){
  new_names <- make.names(names_, ...)
  
  if ( replace_char != "." ){
    new_names <- gsub("\\.",replace_char, new_names)
  }
  
  return(new_names)
  
}
