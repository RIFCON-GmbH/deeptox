
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
makeProjectSnapshot <- function(expProfiles, parameters, profileResults, summaryResults, plotResults = list()){
  

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

  if (plotResults |> inherits("reactivevalues")){
    pR <- rvtl(plotResults)
  } else {
    pR <- list()
  }
  
  
  out <- list(
    expProfiles = expProfiles(),
    metaData = metaData,
    simulationSettings = simulationSettings,
    modelParameters = modelParameters,
    profileResults = rvtl(profileResults),
    summaryResults = summaryResults(),
    plotResults = pR
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
  loadeddata <- jsonlite::read_json(pathname, simplifyVector = TRUE)

  loadeddata[["plotResults"]] <- loadeddata |> reconstruct_plotEPX()
  
  expProfiles(loadeddata[["expProfiles"]])
  
  if (length(loadeddata[["summaryResults"]]) == 0){
    loadeddata["summaryResults"] <- list(NULL)
  }
  summaryResults(loadeddata[["summaryResults"]])
  
  
  combinedParameters <- c(loadeddata[["metaData"]],loadeddata[["modelParameters"]], loadeddata[["simulationSettings"]])


  listToReactiveValues(combinedParameters, parameters, clean=F)
  listToReactiveValues(loadeddata[["profileResults"]], profileResults, clean=T)
  listToReactiveValues(loadeddata[["plotResults"]], plotResults, clean=T)
  
  return(1)
}
