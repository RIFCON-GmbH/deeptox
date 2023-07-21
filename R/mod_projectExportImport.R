#' projectExportImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_projectExportImport_ui <- function(id){
  ns <- NS(id)
  tagList(

    downloadButton(ns("saveProject"), "Save project"),
    

    tags$div(
      fileInputOnlyButton(
        ns("loadProject"),
        buttonLabel=list(
          icon("upload", class = NULL,lib = "font-awesome"),
          "Load project"
        ),
        accept=".rds",
        width=72
      ),
      style="margin-left: 123px; margin-bottom: -15px; margin-top: -34px;"
    ),
    uiOutput(ns("warningOutdated"))
     
  )
}



#' projectExportImport Server Functions
#'
#' @noRd 
mod_projectExportImport_server <- function(id,
                                           expProfiles_rv,
                                           parameters_rv,
                                           profileResults_rv,
                                           summaryResults_rv,
                                           plotResults_rv,
                                           triggerParamUpdate,
                                           triggerProjectImport,
                                           resultsOutdated,
                                           parametersOfResults_rv,
                                           expProfilesOfResults_rv
                                           ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    params2export <- reactiveValues()
    expProfiles2export <- reactiveVal()
    
    # Manage input data change after results creation ####
    ## create warning message ####
    output[["warningOutdated"]] <- renderUI({
      if (any(resultsOutdated()) & length(summaryResults_rv()) ){
        textblocks()[["warningInputChanged"]]
        }
      else
        NULL
      })
    
    ## observe if the parameters were changed ####
    observeEvent(resultsOutdated()["parameters"],{
      listToReactiveValues(list_object = rvtl(parameters_rv), rv_object = params2export, cleanRV=T)
      if (length(resultsOutdated())){
        if (resultsOutdated()["parameters"])
          listToReactiveValues(list_object = rvtl(parametersOfResults_rv), rv_object = params2export, cleanRV=T)
      }
    })

    ## observe if the exposure results were changed ####
    observeEvent(resultsOutdated()["expProfiles"],{
      expProfiles2export(expProfiles_rv())
      if (length(resultsOutdated())){
        if (resultsOutdated()["expProfiles"])
          expProfiles2export(expProfilesOfResults_rv())
      }
  
    })
   
    # Save project ####
    output[["saveProject"]] <- downloadHandler(

      filename = function() {
        paste("deeptox-", get_time_human(), ".rds", sep="")
      },
      content = function(file) {
        toSave <- makeProjectSnapshot(expProfiles_rv, params2export, profileResults_rv, summaryResults_rv, plotResults_rv)
        saveRDS(toSave, file=file)
      },
      contentType = "application/rds"
    ) # end of downloadHandler
  
    
    
    # Load project ####
    observeEvent(input[["loadProject"]], {
      pathname <- NULL
      tryCatch({
      
        pathname <- input[["loadProject"]]$datapath
      }, error = function(ex) {
      })
      if (!is.null(pathname)){
        show_modal_spinner(spin="orbit")
        
        ## data import ####
        loadProjectSnapshot(pathname, expProfiles_rv, parameters_rv, profileResults_rv, summaryResults_rv, plotResults_rv)
        print(paste0("remove file ",pathname))
        file.remove(pathname)
        
        ## trigger for updating the parameters fields
        triggerParamUpdate(Sys.time()) # should only be triggered if loadProjectSnapshot was successful
        triggerProjectImport(Sys.time()) # should only be triggered if loadProjectSnapshot was successful

        remove_modal_spinner() # remove it when done
      }
      
      
      
      
      
    }, ignoreInit = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_projectExportImport_ui("projectExportImport_ui")
    
## To be copied in the server
# mod_projectExportImport_server("projectExportImport_ui")
