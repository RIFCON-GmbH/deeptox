
#' makeProjectSnapshot
#'
#' @param expProfiles 
#' @param parameters 
#' @param profileResults 
#' @param summaryResults 
#' @param plotResults 
#'
#' @return a list with exposure profiles, metadata, simulation settings, model parameters, profile results, summary results, and plot of the results
#' @noRd
makeProjectSnapshot <- function(expProfiles, parameters, profileResults, summaryResults, plotResults){
  

  combinedParameters <- rvtl(parameters)
  namesOfParametersToExport <- names(combinedParameters)[!(names(combinedParameters) %in% ignorelist_params)]
  combinedParameters <- combinedParameters[namesOfParametersToExport]
  
  
  if (length(combinedParameters)){

    simulationSettingsNamesToExport <- names(simsettings)[!(names(simsettings) %in% ignorelist_params)]
    simulationSettings <- combinedParameters[simulationSettingsNamesToExport]
    modelParametersNamesToExport <- parameternames[["daphnia_debkiss"]][!(parameternames[["daphnia_debkiss"]] %in% ignorelist_params)]
    modelParameters <- combinedParameters[modelParametersNamesToExport]
    metaData <- combinedParameters[c("Name","Species","Compound","Model","Length","Time","Concentration")]
  } else {
    simulationSettings <- list()
    modelParameters <- list()
    metaData <- list()
  }

  out <- list(
    expProfiles = expProfiles(),
    metaData = metaData,
    simulationSettings = simulationSettings,
    modelParameters = modelParameters,
    profileResults = rvtl(profileResults),
    summaryResults = summaryResults(),
    plotResults = rvtl(plotResults)
  )

  return(out)
  
  
  
}



#' loadProjectSnapshot
#'
#' @param expProfiles 
#' @param parameters 
#' @param profileResults 
#' @param summaryResults 
#' @param plotResults 
#'
#' @return 1 if correctly imported
#' @noRd
loadProjectSnapshot <- function(pathname, expProfiles, parameters, profileResults, summaryResults, plotResults){
  loadeddata <- readRDS(pathname)
  
  expProfiles(loadeddata[["expProfiles"]])
  summaryResults(loadeddata[["summaryResults"]])

  
  combinedParameters <- c(loadeddata[["metaData"]],loadeddata[["modelParameters"]], loadeddata[["simulationSettings"]])


  listToReactiveValues(combinedParameters, parameters, clean=F)
  listToReactiveValues(loadeddata[["profileResults"]], profileResults, clean=T)
  listToReactiveValues(loadeddata[["plotResults"]], plotResults, clean=T)
  
  return(1)
}
