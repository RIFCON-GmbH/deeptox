#' getParaInfo
#'
#' @param parameterName 
#' @param info_level "description" or "long_description"
#'
#' @return info of parameter with 'parameterName'
#' 
#' @import magrittr
#' @import dplyr
#' @noRd
getParaInfo <- function(parameterName, info_level="description"){
  paraInfo <- paraDescTable %>%
    filter(parameter==parameterName) %>%
    select(info_level)
  
  if ( nrow(paraInfo)==0) return("No information available")
  
  return(paraInfo %>% as.character())
}


#' getParameterLimit
#'
#' @param parname 
#' @param minOrMax 
#' @param parameters_rv
#'
#' @return min or max limit
#' @noRd
getParameterLimit <- function(parname, minOrMax, parameters_rv){

  if ( !(minOrMax %in% c("min","max")) )
    stop("Please choose only 'min' or 'max'.")
  
  smallNo <- 1e-10
  largeNo <- 1e+10
  
  valueToEvaluate <- getParaInfo(parname, minOrMax)
  
  # if the valueToEvaluate is NA and 'max'
  if (is.na(valueToEvaluate) & minOrMax == "max")
    return(largeNo)
  
  # if the valueToEvaluate is NA and 'min'
  if (is.na(valueToEvaluate) & minOrMax == "min")
    return(-largeNo)
  
  
  # if the valueToEvaluate is only digits
  if ( grepl("^\\d+$",valueToEvaluate) | grepl("^[+-]?\\d(\\.\\d+)?[Ee][+-]?\\d+$",valueToEvaluate))
    return(as.numeric(valueToEvaluate))
  
  # if the valueToEvalute is > or < with a digit
  if ( grepl("^(<|>)\\d+$",valueToEvaluate)){
    sign <- sub("^(<|>)(\\d+)$","\\1",valueToEvaluate)
    value <- sub("^(<|>)(\\d+)$","\\2",valueToEvaluate) %>% as.numeric()

    if (sign == ">")
      f <- get("sum")
    else if (sign == "<")
      f <- get("diff")
    else
      stop("Misinterpretation of the sign")
    
    out <- f(c(smallNo,value))
    return(out)
  }
  
  # if the value is a string that can be found among the parameter names
  if ( any(grepl(valueToEvaluate, paraDescTable[,"parameter"]))  ){
    outstring <- paste0("parameter_",valueToEvaluate)

    out <- parameters_rv[[valueToEvaluate]]#outstring#valueToEvaluate#
    if (!length(out))
      out <- valueToEvaluate#outstring#NA#

    return(out)
  }
  
  stop("Parameter limit could not be set.")
  
}


#' outsideRangeMessage
#'
#' @param actVal the actual value to compare
#' @param minMaxRange the min and max values that define the range limits in which actVal may be
#'
#' @return a warning message (if actVal is outside the range) or NULL (if actVal is within the range)
#' @noRd
outsideRangeMessage <- function(actVal, minMaxRange, comment=NULL){
  
  if (is.na(actVal))
    return(
      "No value specified. Please enter a numeric value."
    )
    
    
  if (!length(actVal) | any(is.na(minMaxRange))){
    return(NULL)
  }
    
  
  if (actVal < minMaxRange["min"] | actVal > minMaxRange["max"]){

    return(
      paste0("Warning. Value outside of the allowed range. Min: ",minMaxRange["min"],"; Max: ", minMaxRange["max"])
      )
  } else {
    return(NULL)
  }

  return(NULL)
}



#' convertParameterNameToLabel
#'
#' @param parameterName 
#' @param label 
#'
#' @return return the "label" if available
#'
#' @examples
#' convertParameterNameToLabel("myName", "myLabel")
#' convertParameterNameToLabel("myName", NA)
#' @noRd
convertParameterNameToLabel <- function(parameterName, label){

  if( !is.na(label) ){
      labelString <- label
    } else {
      labelString <- parameterName
    }
  return(labelString)
}



#' Export parameters and setting
#'
#' @param parsAndSettings the parameters and settings to be exported
#' @param outfile the path to the file to which the parameters and settings are written
#' @param parorder the order of the parameters and settings
#'
#' @return write table to 'outfile' file
#' @export
exportParsAndSettings <- function(parsAndSettings, outfile, parorder=NA){
  
  out <- do.call(data.frame, parsAndSettings) %>% 
    t()
  
  # exclude the parameters and settings from the export list that are on the ignore list
  out <- out[!(rownames(out) %in% ignorelist_params),]
  
  if (length(rownames(out)) == length(parorder)){
    if (!any(sort(rownames(out)) != sort(parorder))){
      out <- out[order(match(rownames(out), parorder)),]
    }
  }
  
  write.table(out, outfile, col.names=FALSE, sep=",", quote=FALSE)
  
}



#' Import parameters and setting
#'
#' @param parameters_rv reactive values object to which the contents of the file are stored
#' @param pathname file whose contents are imported
#'
#' @return a reactive values object
#' @export
importParsAndSettings <- function(parameters_rv, pathname){

  inData <- read.csv(pathname, header=FALSE, sep=",")
  
  # exclude the parameters and settings from the export list that are on the ignore list
  inData <- inData[!(inData[,1] %in% ignorelist_params),]
  
  mylist <- as.list(inData[,2])
  names(mylist) <- inData[,1]
  
  mylist <- lapply(mylist, coerceTo)
  
  listToReactiveValues(mylist, parameters_rv, cleanRV=F)

  
}


#' checkParInsideLimits
#'
#' @param parname 
#' @param parameters_rv 
#'
#' @return TRUE if inside limits, FALSE if outside
#' @noRd
checkParInsideLimits <- function(parname, parameters_rv){
  min <- getParameterLimit(parname=parname, minOrMax="min", parameters_rv)
  max <- getParameterLimit(parname=parname, minOrMax="max", parameters_rv)
  val <- parameters_rv[[parname]]
  
  if (is.na(val))
    return(FALSE)
  
  if (val >= min & val <= max)
  {
    insideLimits <- TRUE
  }else{
    insideLimits <- FALSE
  }
  return(insideLimits)
}

