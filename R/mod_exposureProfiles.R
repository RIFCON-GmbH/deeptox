#' exposureProfiles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
mod_exposureProfiles_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      
      box(
        title = tags$b("Exposure profiles"),
        width=12, collapsible=T, collapsed=F,
        
        tags$div(
          HTML(
            textblocks()[["expProfilesBoxIntro"]],
            "<br>"
          ),
          class="box_intro_text"
        ),
        
        actionButton(inputId = ns("loadExposureProfiles"),
                     label = "Load profile(s)",
                     onclick = HTML(
                       getPosition(ns("loadExposureProfiles"),
                                   nsid = ns(""))
                       )
                     ),
        actionButton(ns("removeExposureProfiles"), "remove selected"),
        actionButton(ns("removeAllExposureProfiles"), "remove all"),
        tags$br(),tags$br(),
        uiOutput(ns("dropdownProfiles")),
        tags$br(),
        uiOutput(ns("profilePlot"))
      )
    ),
  )
}
    
#' exposureProfiles Server Functions
#'
#' @noRd 
mod_exposureProfiles_server <- function(id, expProfiles_rv, parameters_rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    mod_exposureProfilesImport_server("exposureProfilesImport_1", expProfiles_rv)
    
    # load exposure profiles button ####
    observeEvent(input[["loadExposureProfiles"]],{
      print("load exposure profiles")

      showModal(customModalDialog(title = NULL,
                            mod_exposureProfilesImport_ui(ns("exposureProfilesImport_1")),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    

    
    
    output[["dropdownProfiles"]] <- renderUI({
      if(length(expProfiles_rv())){
        exposureProfileNames <- names(expProfiles_rv())
        selectInput(ns("selInProfile"),
                    "Exposure profile",
                    choices=exposureProfileNames,
                    selected=exposureProfileNames[1]
                    )
      }else{
        NULL
      }
    })
    

    output[["profilePlot"]] <- renderUI({
      if (length(expProfiles_rv()) & length(input[["selInProfile"]])){
        sel <- input[["selInProfile"]]
        
        output[["profilePlot_tmp"]] <- renderPlot({
          plotExpProfiles(
            data=expProfiles_rv()[[sel]],
            x="V1",
            y="V2",
            title=sel,
            xunit=rvtl(parameters_rv)[["Time"]],
            yunit=rvtl(parameters_rv)[["Concentration"]]
            )
        })
        
        plotOutput(ns("profilePlot_tmp"))
        
      }else{
        NULL
      }
      
      
    })
    
    
    # observer: remove selected exposure profiles ####
    observeEvent(input[["removeExposureProfiles"]],{
      print("remove exposure profiles")
      if (length(input[["selInProfile"]])){
        sel <- input[["selInProfile"]]
        expProfiles_rv(expProfiles_rv()[names(expProfiles_rv()) != sel])
        exposureProfileNames <- names(expProfiles_rv())
        updateSelectInput(session, "selInProfile", label = "Exposure profile",
                          choices = exposureProfileNames)
      }
    })
    
    # observer: remove all exposure profiles ####
    observeEvent(input[["removeAllExposureProfiles"]],{
      print("remove all exposure profiles")
      if (length(input[["selInProfile"]])){
        expProfiles_rv(list())
        updateSelectInput(session, "selInProfile", label = "Exposure profile",
                          choices = "")
      }
    })
    
    
    # MODEL FOR BOX INFO ####
    observeEvent(input[["boxinfo"]],{
      ns <- session$ns

      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Exposure profiles"),
                                "<br>",
                                "" # text body
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
  })
}
    
## To be copied in the UI
# mod_exposureProfiles_ui("exposureProfiles_ui_1")
    
## To be copied in the server
# mod_exposureProfiles_server("exposureProfiles_ui_1")
