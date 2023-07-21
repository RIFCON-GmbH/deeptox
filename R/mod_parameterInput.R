#' parameterInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parameterInput_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    ## ui objects
    uiOutput(ns("inputFieldCheck")),
    uiOutput(ns("inputField")),
    uiOutput(ns("warningRange"))
  )
}
 


   
#' parameterInput Server Functions
#'
#' @noRd 
mod_parameterInput_server <- function(id,
                                      parname,
                                      value,
                                      datatype,
                                      parameters_rv,
                                      triggerParamUpdate,
                                      min = NA,
                                      max = NA,
                                      step = 0.000001,
                                      width = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # server commands

    # Parameter activatable? ####
    activatable <- reactive(parname %in% activatablePars[,"parameter"])
    
    # PARAMETER LIMITS ####
    if ( !(parname %in% names(initValues[["daphnia_debkiss"]][["Meta"]])) & datatype != "dropdown")
      specificPar <- TRUE
    else
      specificPar <- FALSE
    
    minLimit <- reactive({
      if (specificPar)
        getParameterLimit(parname, "min", rvtl(parameters_rv)) 
      else
        NA
      })
    
    maxLimit <- reactive({
      if (specificPar)
        getParameterLimit(parname, "max", rvtl(parameters_rv)) 
      else
        NA
      })
    
    parameterLimits <- reactive(
      data.frame(
        min = minLimit(),
        max = maxLimit()
      )
    )

    
    field_id <- paste0("par_",parname)
    infoButton_id <- paste0("info_",parname)

    ## render warning message ####
    output[["warningRange"]] <- renderUI({
      ns <- session$ns
      
      # generate warning message
      if ( datatype != "dropdown" ){
        warningmessage <- outsideRangeMessage( actVal = parameters_rv[[parname]], parameterLimits(), comment=parname)  
      } else {
        warningmessage <- NULL
      }
      
      
      # render
      if (!is.null(warningmessage)){
        tagList(
          tags$div(
            warningmessage,
            id=paste0(ns("rangeWarning")),
            style="color: red; padding-top: 0px; vertical-align: top; margin-top: -15px; margin-bottom: 5px; font-size: x-small;"
          )
        )
      }
      
    })
    
    
    
    # INFO FIELD ####
    ## create label for input field ####
    paraInfo <- getParaInfo(parname)
    if (!length(paraInfo) | !specificPar ) paraInfo <- "No information available"
    
    if (specificPar){
      if( !is.na(getParaInfo(parname, "label"))){
        labelString <- getParaInfo(parname, "label")
      }else{
        labelString <- getParaInfo(parname, "parameter")
      }
    } else {
      labelString <- parname
    }
    
    
    newLabel <- makeTextWithInfoButton(labelString,
                                       infoButtonId = session$ns(infoButton_id),
                                       div_title = paraInfo)

    
    # render input field ####
    output[["inputField"]] <- renderUI({
      ns <- session$ns
      value <- isolate(parameters_rv[[parname]])

      tagList(
        deactivateMouseWheel(), 
      
        switch(datatype,
               logical = checkboxInput(
                 inputId=ns(field_id),
                 label=newLabel,
                 value=value,
                 width = width
               ),
               
               numerical = tagList(
                   numericInput(
                   inputId=ns(field_id),
                   label=newLabel,
                   value=value,
                   min = NA,
                   max = NA,
                   step = step,
                   width = width)
               ),
               
               text = tagList(
                 textInput(
                   inputId=ns(field_id),
                   label=newLabel,
                   value=value,
                   width = width)
               ),
               
               dropdown = tagList(
                 selectInput(
                   inputId=ns(field_id),
                   label=newLabel,
                   selected=value,
                   choices=strsplit(getParaInfo(parname, "min"),",")[[1]],# the choices are read from the "min" column in the paraDesc.csv file
                   width = width)
               )
               
        )
      )
      
    })
    
    # render input field checkbox ####
    field_id_check <- paste0(field_id,"_check")
    labelCheck <- paste0(labelString," active")
    output[["inputFieldCheck"]] <- renderUI({
      ns <- session$ns
      
      if (activatable()){
      

        out <- tagList(
          tags$div(
            checkboxInput(
              inputId=ns(field_id_check),
              label=labelCheck,
              value=TRUE,
              width = width
            ),
            style="margin-top: 20px; margin-bottom: -15px; font-size: x-small;"
          )
        )
        
        
        return(out)
      } else {
        return(NULL)
      }

    })

    # observer input field checkbox ####
    observeEvent(input[[field_id_check]], {
      fallbackvalue <- activatablePars %>% filter(parameter==parname) %>% select(fallbackVal) %>% as.character()
        
      if (input[[field_id_check]])
        shinyjs::enable(field_id)
      if (!input[[field_id_check]]){
        shinyjs::disable(field_id)
        parameters_rv[[parname]] <- parameters_rv[[fallbackvalue]]
        updateNumericInput(session, inputId = field_id, value = parameters_rv[[parname]])
      }
      
    }, ignoreInit = TRUE)
    #}
    
    # observer input field ####
    observeEvent(input[[field_id]], {

      parameters_rv[[parname]] <- input[[field_id]]

    }, ignoreInit = TRUE)
    
    


    # MODAL HELP ####
    long_description <- getParaInfo(parname, "long_description")
    if (grepl("^$|(No information available)", long_description)) long_description <- getParaInfo(parname, "description")
    
    
    
    ## create the modal observer ####
    observeEvent(input[[infoButton_id]],{
      ns <- session$ns
      
      if( !is.na(getParaInfo(parname, "label"))){
        labelString <- getParaInfo(parname, "label")
      }else{
        labelString <- getParaInfo(parname, "parameter")
      }
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3(labelString),
                                "<br>",
                                long_description
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    
    # UPDATE FIELDS AFTER IMPORT ####
    observeEvent(triggerParamUpdate(),{
      switch(datatype,
             logical = updateCheckboxInput(session, inputId = field_id, value = parameters_rv[[parname]]),
             numerical = updateNumericInput(session, inputId = field_id, value = parameters_rv[[parname]]),
             text = updateTextInput(session, inputId = field_id, value = parameters_rv[[parname]]),
             dropdown = updateSelectInput(session, inputId = field_id, selected = parameters_rv[[parname]])
             )
      if (activatable()){
        updateCheckboxInput(session, inputId = field_id_check, value = TRUE)
      }
      
    }, ignoreInit = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_parameterInput_ui("parameterInput_ui_1")
    
## To be copied in the server
# mod_parameterInput_server("parameterInput_ui_1")
