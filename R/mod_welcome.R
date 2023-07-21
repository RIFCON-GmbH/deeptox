#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcome_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=tags$div("DeEP - DEB-TKTD EPx Predictor", class="main-title"), width=12,
          uiOutput(ns("welcome_text")),
          uiOutput(ns("disconnect_warning_text"))
          # tags$div("After 15 minutes of inactivity the app will disconnect. We recommend to save regularly to prevent data loss.",
          #         class="italic-note")
      )
    )
  )
}
    
#' welcome Server Functions
#'
#' @noRd 
mod_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output[["welcome_text"]] <- renderUI({
      
      infoButtonId <- ns("boxinfo_deb_theory")
      id <- sub("^(.*-).*$","\\1",infoButtonId)
      
      HTML(paste0(
        "<b>DeEP</b> is an easy-to-use platform using DEB-TKTD modelling
        to predict the EP<sub>x</sub> multiplier for a given chemical product and species.
        The EP<sub>x</sub> multiplier is an endpoint relevant to ecological risk assessment
        (ERA) of plant protection products. It is the factor by which a given
        (realistic) exposure profile would need to be multiplied in order to
        elicit an X% reduction in either survival, growth or reproduction.<br><br>
        
        You can load your parameter values, settings and exposure profiles using
        the dashboard below and click 'Run Exposure Assessment' in the 'Results'
        box to quickly generate results.
        Detailed instructions can be found in the <a href=\"https://www.deep-tox.info/downloads/DeEP%20User%20Manual.pdf\" target=\"_blank\">manual</a>
        while some background on the underlying theory is available ",
        actionLink(infoButtonId,
                   "here",
                   onclick = HTML(getPosition(infoButtonId, nsid = id))
        ),".<br><br>
        
        <b>NOTE:</b> Predictions rely on validated parameter values provided by the
        user, these can be loaded from a file or entered/adjusted manually.
        Untested parameter values may be used for illustrative or
        educational purposes, but the predictions will not be valid."
      ))
    })
    
    
    ## Moving time window box ####
    observeEvent(input[["boxinfo_deb_theory"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                                  HTML(
                                    paste0(
                                      textblocks()[["debtheoryBoxInfo"]]
                                    )
                                  ),
                                  footer=tagList(NULL),
                                  easyClose=TRUE,
                                  marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    
    ## disconnect warning text ####
    output[["disconnect_warning_text"]] <- renderUI({
      sessionIP <- session$clientData$url_hostname
          # only when running locally allow import of TOXSWA profiles
      if ( sessionIP != "127.0.0.1" ){
        tags$div("After 15 minutes of inactivity the app will disconnect. We recommend to save regularly to prevent data loss.",
                                class="italic-note")
      } else {
        tags$div("")
      }
    })
    
    
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_ui_1")
    
## To be copied in the server
# mod_welcome_server("welcome_ui_1")
