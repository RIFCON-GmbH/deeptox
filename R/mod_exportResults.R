#' exportResults UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import officedown
#' @importFrom shinyjs disable enable
mod_exportResults_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("genReport"), "Generate report"),
    uiOutput(ns("warningOutdated"))
  )
}
    
#' exportResults Server Functions
#'
#' @noRd 
mod_exportResults_server <- function(id,
                                     expProfiles_rv,
                                     parameters_rv,
                                     profileResults_rv,
                                     summaryResults_rv,
                                     plotResults_rv,
                                     resultsOutdated,
                                     parametersOfResults_rv,
                                     expProfilesOfResults_rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## initialize
    params2export <- reactiveValues()
    expProfiles2export <- reactiveVal()
    summaryResults2export <- reactiveVal()
    
    ## observe if the parameters were changed ####
    observeEvent(resultsOutdated()["parameters"],{
      listToReactiveValues(list_object = rvtl(parameters_rv), rv_object = params2export, cleanRV=T)
      if (length(resultsOutdated())){
        if (resultsOutdated()["parameters"]){
          listToReactiveValues(list_object = rvtl(parametersOfResults_rv), rv_object = params2export, cleanRV=T)
        }
      }
    }, ignoreInit = TRUE)
    
    ## observe if the exposure results were changed ####
    observeEvent(resultsOutdated()["expProfiles"],{
      expProfiles2export(expProfiles_rv())
      if (length(resultsOutdated())){
        if (resultsOutdated()["expProfiles"])
        {
          expProfiles2export(expProfilesOfResults_rv())
        }
      }
      
    })
    
    
    ## check if all necessary data is available to create report ####
    params <- reactive({
      ### remove the datetime column from the summary results table
      summaryResults2export(summaryResults_rv()[, colnames(summaryResults_rv()) != "DateTime"])

      out <- makeProjectSnapshot(expProfiles2export,
                                 params2export,
                                 profileResults_rv,
                                 summaryResults2export,
                                 plotResults_rv
                                 )
      return(out)
    })
    
    paramsOK <- reactive({
      checkResultsComplete(params())
    })
    

    observeEvent(paramsOK(), {
      if (paramsOK())
        shinyjs::enable("genReport")
      if (!paramsOK())
        shinyjs::disable("genReport")
    }, ignoreInit = FALSE)
    
    
    ### create report ####
    output[["genReport"]] <- downloadHandler(
      filename = function(){
        fname <- "report.docx"
        return(fname)
      },
      content = function(file) {
        

        generateReport(params(), file=file, session)
       
        
      }
    )
    
    
    
    output[["warningOutdated"]] <- renderUI({
      if (any(resultsOutdated()) & length(summaryResults_rv()) ){
        textblocks()[["warningInputChanged"]]
      }
      else
        NULL
    })
    
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_exportResults_ui("exportResults_ui_1")
    
## To be copied in the server
# mod_exportResults_server("exportResults_ui_1")
