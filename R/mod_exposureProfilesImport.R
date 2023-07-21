#' exposureProfilesImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exposureProfilesImport_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("select_exp_profiles_btns")),
    fluidRow(
      uiOutput(ns("assembled"))
      ),
    actionButton(ns("importSelProfiles"), "Import files")
    
  )
}
    
#' exposureProfilesImport Server Functions
#'
#' @noRd 
mod_exposureProfilesImport_server <- function(id, expProfiles_rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # setting initial reactive values ####
    triggerNoImportFilesSelected <- reactiveVal(0)
    
    # Path information to example files ####
    fpath <- system.file("extdata",package="deeptox") %>%
      list.files("*.\\.txt", full.names = T)
    files <- basename(fpath)
    

    # Render radiobuttons ####
    output[["select_exp_profiles_btns"]] <- renderUI({
      sessionIP <- session$clientData$url_hostname
      print(sessionIP)
      # only when running locally allow import of TOXSWA profiles
      if ( sessionIP == "127.0.0.1" ){
        localChoice <- c("TOXSWA profiles"="TOXSWA")
      } else {
        localChoice <- NULL
      }
      
      choices <- c(
        "example profiles"="example",
        "upload your own"="custom",
        localChoice
      )
      
      radioButtons(inputId = ns("selExpProfSource"),
                   label = "Select the source of the exposure profiles",
                   choices = choices,
                   inline = TRUE)
    })
    
    # Render the assembled elements ####
    output[["assembled"]] <- renderUI({
      req(input[["selExpProfSource"]])

      tagList(
        if ( input[["selExpProfSource"]] == "example"){
          tagList(
            column(
              uiOutput(ns("defaultProfiles")),
              width=6
              ),
            column(
              HTML(
                textblocks()[["expProfilesExampleImport"]]
              ),
              width=6
              ),
            tags$br()
            )
          
        } else if ( input[["selExpProfSource"]] == "custom" ){
          tagList(
            column(
              uiOutput(ns("customProfiles")),
              width=6
              ),
            column(
              HTML(
                textblocks()[["expProfilesCustomImport"]]
                ),
              width=6),
            tags$br()
          )
          
        }else if ( input[["selExpProfSource"]] == "TOXSWA" ){
          tagList(
            column(
              uiOutput(ns("toxswaProfiles")),
              width=6
            ),
            column(
              HTML(
                textblocks()[["expProfilesToxswaImport"]]
              ),
              width=6),
            tags$br()
          )

        },
        uiOutput(ns("warningNoFilesSelected"))
      )

    })
    
    ## default profiles ####
    output[["defaultProfiles"]] <- renderUI({
      if (length(expProfiles_rv())){
        not_yet_chosen_files <- grep(paste(names(expProfiles_rv()), collapse="|"), files, value = TRUE, invert = TRUE)
      } else {
        not_yet_chosen_files <- files
      }

      tagList(selectizeInput(
                ns("selectedExpProfiles"),
                "Select exposure profiles",
                choices=not_yet_chosen_files,
                multiple=TRUE,
                options = list(placeholder = 'select profile(s)')
              )
      )
      
    })
    
    ## custom profiles ####
    output[["customProfiles"]] <- renderUI({
      tagList(
        fileInputOnlyButton(
          ns("importExpProfiles"),
          buttonLabel=list(
            icon("upload", class = NULL, lib = "font-awesome"),
            "Upload exposure profiles"
          ),
          accept=c(".txt",".csv"),
          multiple = TRUE,
          width=72
        )
      )
      
    })
    
    ## TOXSWA profiles ####
    output[["toxswaProfiles"]] <- renderUI({
      tagList(
        fileInputOnlyButton(
          ns("importToxswaExpProfiles"),
          buttonLabel=list(
            icon("upload", class = NULL, lib = "font-awesome"),
            "Upload exposure profiles"
          ),
          accept=c(".out"),
          multiple = TRUE,
          width=72
        )
      )
      
    })

    
    # render warning message if no files are selected ####
    output[["warningNoFilesSelected"]] <- renderUI({
      if ( triggerNoImportFilesSelected() == 1 ){
        tags$div(
          "No files selected.",
          style="color: red; padding-left: 5px;"
        )
      }else{
        NULL
      }
    })
    
    # observer to check if imported 'custom' profiles are uploaded ####
    observeEvent(input[["importExpProfiles"]]$name,{
      if ( length(input[["importExpProfiles"]]$name) )
        triggerNoImportFilesSelected(0)
      else
        triggerNoImportFilesSelected(1)
      
      scrollIntoView(ns("selExpProfSource")) # force setting the view back to the radiobutton in the modal
      
      })
    
    # observer to check if example profiles are selected ####
    observeEvent(input[["selectedExpProfiles"]],{
      if ( length(input[["selectedExpProfiles"]]) )
        triggerNoImportFilesSelected(0)
      else
        triggerNoImportFilesSelected(1)
    })

    # observer to check if TOXSWA profiles are selected ####
    observeEvent(input[["importToxswaExpProfiles"]],{
      if ( length(input[["importToxswaExpProfiles"]]) )
        triggerNoImportFilesSelected(0)
      else
        triggerNoImportFilesSelected(1)
    })
    

    
    
    
    # Import exposure profiles button ####
    observeEvent(input[["importSelProfiles"]],
                 {
                   
                   print("importSelProfiles")
                   
                   ## example profiles ####
                   if ( input[["selExpProfSource"]] == "example"){
                     selectedFiles <- paste(input[["selectedExpProfiles"]], collapse="|")
                     if (!grepl("^$",selectedFiles)){
                       filesToImport <- grep(selectedFiles, fpath, value=T)
                       expProfilesImported <- readExpProfiles(filesToImport)
                       merge_old_and_new <- c(expProfiles_rv(), expProfilesImported)
                       names(merge_old_and_new) <- make.names2(names(merge_old_and_new), unique = TRUE) # create unique names
                       expProfiles_rv(merge_old_and_new)
                       triggerNoImportFilesSelected(0)
                     }else{
                       triggerNoImportFilesSelected(1)
                       return(0)
                     }
                     }
                   
                   
                   ## custom profiles ####
                   if ( input[["selExpProfSource"]] == "custom"){
                     filesToImport <- input[["importExpProfiles"]]$datapath
                     fileNames <- input[["importExpProfiles"]]$name %>% tools::file_path_sans_ext()
                     if (length(filesToImport)){
                       expProfilesImported <- readExpProfiles(filesToImport, fileNames)
                       merge_old_and_new <- c(expProfiles_rv(), expProfilesImported)
                       names(merge_old_and_new) <- make.names2(
                         names(merge_old_and_new),
                         replace_char = "_",
                         unique = TRUE
                         ) # create unique names
                       expProfiles_rv(merge_old_and_new)
                       triggerNoImportFilesSelected(0)
                     }else{
                       triggerNoImportFilesSelected(1)
                       return(0)
                     }
                       
                   }

                   
                   if ( input[["selExpProfSource"]] == "TOXSWA"){
                     
                     filesToImport <- input[["importToxswaExpProfiles"]]$datapath
                     fileNames <- input[["importToxswaExpProfiles"]]$name %>% tools::file_path_sans_ext()
                     if (length(filesToImport)){
                       deeptox:::show_modal_spinner( text = "Importing TOXSWA profiles. Omitting all files that are no TOXSWA out-files.", top = "90%" )
                       checked_toxswa_txt <- pre_check_toxswa_files(filesToImport, input[["importToxswaExpProfiles"]]$name)
                       update_modal_spinner(checked_toxswa_txt)
                       expProfilesImported <- readToxswaExpProfiles(filesToImport, fileNames)
                       merge_old_and_new <- c(expProfiles_rv(), expProfilesImported)
                       names(merge_old_and_new) <- make.names2(names(merge_old_and_new), replace_char = "_", unique=TRUE) # create unique names
                       expProfiles_rv(merge_old_and_new)
                       triggerNoImportFilesSelected(0)
                     }else{
                       triggerNoImportFilesSelected(1)
                       return(0)
                     }
                     
                   }
                   
                   removeModal()
                   print(names(expProfiles_rv()))
                   
                 }, ignoreInit = TRUE)
  }
  )}
    
## To be copied in the UI
# mod_exposureProfilesImport_ui("exposureProfilesImport_ui_1")
    
## To be copied in the server
# mod_exposureProfilesImport_server("exposureProfilesImport_ui_1")
