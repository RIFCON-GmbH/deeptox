#' trackInputDatVSResults UI Function
#'
#' @description A shiny Module to track the changes in the input parameters after the results were created.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom digest digest
#' @importFrom magrittr '%>%'
mod_trackInputDatVSResults_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' trackInputDatVSResults Server Functions
#'
#' @noRd 
mod_trackInputDatVSResults_server <- function(id,
                                              expProfiles_rv,
                                              parameters_rv,
                                              summaryResults_rv,
                                              triggerParamUpdate,
                                              triggerProjectImport,
                                              parametersOfResults_rv,
                                              expProfilesOfResults_rv,
                                              hash_parameters,
                                              hash_exposureprofiles,
                                              hash_parameters_results_created,
                                              hash_exposureprofiles_results_created
                                              ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 

    
    # set observers ####
    ## observe change in parameters_rv ####
    observeEvent(
      rvtl(parameters_rv),
      {
        
        parameterValuesString <- do.call(paste0, rvtl(parameters_rv))# use the string as it seems that the list object does store some other information that affects the md5 hash
        
        hash_parameters(
          parameterValuesString %>% 
            digest::digest(algo = "md5") # create the hash for the parameters 
          )


      },
      ignoreInit = FALSE)
    
    ## observe change in expProfiles_rv ####
    observeEvent(
      expProfiles_rv(),
      {
        hash_exposureprofiles(
          expProfiles_rv() %>%
            digest::digest(algo = "md5") # # create the hash for the exposure profiles 
        )
      },
      ignoreInit = TRUE)
    
    ## observe change in summaryResults_rv or when project is imported ####
    observeEvent(
      list(summaryResults_rv() , triggerProjectImport()),
      {
        
        parametersOfResults_rv <- listToReactiveValues(list_object = rvtl(parameters_rv), rv_object = parametersOfResults_rv, cleanRV=T)
        expProfilesOfResults_rv(expProfiles_rv()) # remember the exposure profiles that were used to create the results
        
        parameterValuesString <- do.call(paste0, rvtl(parameters_rv))# use the string as it seems that the list object does store some other information that affects the md5 hash
        hash_parameters_results_created(
          parameterValuesString %>% 
            digest::digest(algo = "md5") # create the hash for the parameters 
        )
        
        
        hash_exposureprofiles_results_created(# remember the hash for the exposure profiles that were used to create the results
          expProfiles_rv() %>% 
            digest::digest(algo = "md5")
        )
        
        
      },
      ignoreInit = TRUE)
    #...........................................
    
    # Are the results outdated because the parameters or exposure profiles were changed ####
    #  as compared by the hash
    paramsOutdated <- reactive({
      (hash_parameters() != hash_parameters_results_created())
    })
    
    expProfilesOutdated <- reactive({
      (hash_exposureprofiles() != hash_exposureprofiles_results_created())
    })
    
    resultsOutdated <- reactive( {
      if (!length(paramsOutdated()))
        parameters <- FALSE
      else
        parameters <- paramsOutdated()
      
      if (!length(expProfilesOutdated()))
        expProfiles <- FALSE
      else
        expProfiles <- expProfilesOutdated()
      
      
      out <- c(parameters=parameters, expProfiles=expProfiles)
      return( out )
    })
    

    
    return(resultsOutdated)
    
  })
}
    
## To be copied in the UI
# mod_trackInputDatVSResults_ui("trackInputDatVSResults_ui_1")
    
## To be copied in the server
# mod_trackInputDatVSResults_server("trackInputDatVSResults_ui_1")
