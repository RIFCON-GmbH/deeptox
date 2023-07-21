#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinybusy
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  shinybusy::show_modal_spinner(spin="orbit", text="Starting app...")
  
  fake_electron <- FALSE # Can be set to TRUE during development to simply fake R shiny running within electron
  isWithinElectron <- reactive({
    if (fake_electron){
      return(TRUE)
    }else{
      return(check_within_electron())
    }
    })
  sessionIP <- reactive(session$clientData$url_hostname)
  
  observe({
    # set size of files to be imported when running locally; needed for large TOXSWA files.
    if (sessionIP() == "127.0.0.1"){
      options(shiny.maxRequestSize = 70*1024^2) 
    }
  })
  
    
  # declare reactive values ####
  expProfiles_rv <- reactiveVal() # 1:1 representation of the exposure profiles loaded
  expProfilesOfResults_rv <- reactiveVal() # exposure profiles that was used to create the results
  parameters_rv <- reactiveValues() # 1:1 representation of the parameters in the input fields
  observeEvent(initValues[[1]],{
               listToReactiveValues(list_object = unlistInitParameters(initValues[[1]]), rv_object = parameters_rv, cleanRV=T)
               }) # set init parameter values to parameters reactive values object
  parametersOfResults_rv <- reactiveValues() # parameter set that was used to create the results
  profileResults_rv <- reactiveValues()
  summaryResults_rv <- reactiveVal()
  plotResults_rv <- reactiveValues()
  
  triggerParamUpdate <- reactiveVal()
  triggerProjectImport <- reactiveVal()
  

  hash_parameters <- reactiveVal()
  hash_exposureprofiles <- reactiveVal()
  hash_parameters_results_created <- reactiveVal()
  hash_exposureprofiles_results_created <- reactiveVal()
  
  #...........................................
  
  

  
  # render hostname ####
  output[["sessionInfo"]] <- renderUI({
    paste(packageVersion("deeptox"), collapse=".") %>% 
      HTML() %>% 
      tags$div(class = "version_number")
  })
  #...........................................
  
  
  # global observer ####
  ## observe "interval"/"timesteps" to synchronize padDt to the same value ####
  observeEvent(parameters_rv[["interval"]],{
    parameters_rv[["padDt"]] <- parameters_rv[["interval"]]
  },ignoreInit = TRUE)
  
  # run modules ####
  mod_welcome_server("welcome")
  
  mod_parameters_server("parameters_ui",
                        parameters_rv,
                        triggerParamUpdate)
  
  resultsOutdated <- mod_trackInputDatVSResults_server("trackInputDatVSResults_ui",
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
                                                       )
  
  mod_exposureProfiles_server("exposureProfiles_ui",
                              expProfiles_rv,
                              parameters_rv)
  
  mod_results_server("results_ui",
                     expProfiles_rv,
                     parameters_rv,
                     profileResults_rv,
                     summaryResults_rv,
                     plotResults_rv,
                     parametersOfResults_rv,
                     expProfilesOfResults_rv,
                     resultsOutdated,
                     triggerParamUpdate
                     )
  
  mod_projectExportImport_server("projectExportImport_ui",
                                 expProfiles_rv,
                                 parameters_rv,
                                 profileResults_rv,
                                 summaryResults_rv,
                                 plotResults_rv,
                                 triggerParamUpdate,
                                 triggerProjectImport,
                                 resultsOutdated,
                                 parametersOfResults_rv,
                                 expProfilesOfResults_rv)
  
  mod_electronTop_server("electronTop", isWithinElectron)
  #...........................................
  
  remove_modal_spinner() # remove it when done
  
  message("session started")
  print(paste0("session token: ", session$token))
  print(paste0("Tempdir:", tempdir()))
  
  onStop(function() message("onStop called: session stopped"))
}
