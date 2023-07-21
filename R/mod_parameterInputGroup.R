#' parameterInputGroup UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parameterInputGroup_ui <- function(id){
  ns <- NS(id)
  tagList(
    ## ui objects
    uiOutput(ns("paraInputFields"))
  )
}
    
#' parameterInputGroup Server Functions
#'
#' @noRd 
mod_parameterInputGroup_server <- function(id, parameterNames, parameters_rv, triggerParamUpdate){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # create an input field for each of the 'parameterNames' ####
    lapply(parameterNames, function(para_i){
      
      
      datatype <- getParaInfo(para_i, "data type")
      if (grepl("no information",datatype, ignore.case = TRUE))
        datatype <- "text"
      
      value <- do.call(c, lapply(initValues[["daphnia_debkiss"]], function(x){
        out <- x[para_i]
        if (is.na(out)) out <- NULL
        return(out)
      }
      ))
      
      mod_parameterInput_server(id=para_i, parname=para_i, value=value, datatype=datatype, parameters_rv, triggerParamUpdate)
    })
    
    
    # render input value group; one input field for each 'parameterNames' ####
    output[["paraInputFields"]] <- renderUI({
      ns <- session$ns
      
      
      inputFields <- lapply(parameterNames, function(para_i){
        mod_parameterInput_ui(ns(para_i))
      }
      )
      
      do.call(tagList, inputFields)
    }) # end of renderUI
  })
}
    
## To be copied in the UI
# mod_parameterInputGroup_ui("parameterInputGroup_ui_1")
    
## To be copied in the server
# mod_parameterInputGroup_server("parameterInputGroup_ui_1")
