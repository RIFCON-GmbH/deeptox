#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinybusy
#' @import deSolve
#' @import rootSolve
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title=makeTextWithInfoButton(tags$b("Results"), infoButtonId = ns("boxinfo")),
        
          width=12, collapsible=T, collapsed=F,
        
        tags$div(
          HTML(
            textblocks()[["resultsBoxIntro"]],
            "<br>"
          ),
          class="box_intro_text"
        ),
        downloadButton(ns("saveAndRun"), "Save and run assessment", icon = NULL),
        actionButton(ns("assessExpo"), "Run assessment",
                     onclick = HTML(
                       getPosition(ns("assessExpo"),
                                   nsid = ns(""))
        )),
        mod_exportResults_ui(ns("exportResults")),
        uiOutput(ns("warningProfile")),
        uiOutput(ns("warningParameters")),
        uiOutput(ns("warningWindowlength")),
        DT::dataTableOutput(ns("summaryResultsTable")),
        tags$br(),
        uiOutput(ns("resultsPlots"))
          
      )
    )
  )
}
    
#' results Server Functions
#'
#' @noRd 
mod_results_server <- function(id,
                               expProfiles_rv,
                               parameters_rv,
                               profileResults_rv,
                               summaryResults_rv,
                               plotResults_rv,
                               parametersOfResults_rv,
                               expProfilesOfResults_rv,
                               resultsOutdated,
                               triggerParamUpdate
                               ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # module internal reactiveVal ####
    triggerProfileWarning <- reactiveVal(FALSE)
    triggerParameterWarning <- reactiveVal(FALSE)
    
    # run exportResults module ####
    mod_exportResults_server("exportResults",
                             expProfiles_rv,
                             parameters_rv,
                             profileResults_rv,
                             summaryResults_rv,
                             plotResults_rv,
                             resultsOutdated,
                             parametersOfResults_rv,
                             expProfilesOfResults_rv)
    
    
    
    # observer to check for parameter limits ####
    parsInsideLimits <- reactive({
        do.call(c, lapply(names(rvtl(parameters_rv)), function(x){
        out <- tryCatch(
          checkParInsideLimits(x,parameters_rv),
          error=function(cond){
            allowedErrorIn <- c("Compound", "Name", "Species", "Model", "Length", "Time", "Concentration")
            if (x %in% allowedErrorIn){
              TRUE
            } else {
              stop(cond)
            }
          })
        return(out)
      })
      )
    })

    
    # Prompt user to save before running the exposure assessment ####
    assesExpoOK <- reactiveVal()
    observeEvent(input[["continue"]], {
      assesExpoOK(Sys.time())
      removeModal()
      })
    observeEvent(input[["cancel"]], removeModal())
    

    observeEvent(input[["assessExpo"]],{
      assesExpoOK(Sys.time())
    })
    
    # Save and run exposure assessment ####
    output[["saveAndRun"]] <- downloadHandler(
      filename = function() {
        paste("deeptox-", get_time_human(), ".rds", sep="")
      },
      content = function(file) {
        toSave <- makeProjectSnapshot(
          expProfiles = expProfiles_rv,
          parameters = parameters_rv, 
          profileResults = reactiveValues(),
          summaryResults = reactiveVal(), 
          plotResults = reactiveValues())
        saveRDS(toSave, file=file)
        removeModal()
        assesExpoOK(Sys.time())
      }
    ) # end of downloadHandler
    
    
    # run exposure assessment button ####
    observeEvent(assesExpoOK(),{

      ## check for existing profiles ####
      if (!length(expProfiles_rv())){
        triggerProfileWarning(TRUE)
        return()
      } else
        triggerProfileWarning(FALSE)

      # check for parameter limits ####
      if (any(!parsInsideLimits())){
        triggerParameterWarning(TRUE)
        return()
      }else{
        triggerParameterWarning(FALSE)
      }
      
      # check for correct window length. It should not be longer than the shortest profile. ####
      checked_wl <- check_window_length(expProfiles_rv(), parameters_rv[["windowLength"]])
      if ( length(checked_wl)>0 ){
        parameters_rv[["windowLength"]] <- min(checked_wl[,"length"])
        triggerParamUpdate(Sys.time())
      }
      
      
      profiles <- expProfiles_rv()
      padDt <- parameters_rv[["interval"]]
      windowLength <- parameters_rv[["windowLength"]]
      interval <- parameters_rv[["interval"]]
      thinning <- parameters_rv[["thinning"]]
      cutOff <- parameters_rv[["cutOff"]]
      ECpct <- parameters_rv[["ECpct"]]
      detail <- parameters_rv[["detail"]]
      passFail <- parameters_rv[["passFail"]]
      

      parmsList <- reactiveValuesToList(parameters_rv)
      parms <- unlist(parmsList[parameternames[["daphnia_debkiss"]]])
      y0 <- unlist(parmsList[ sort(grep("^y0_\\d{1}",names(parmsList), value=T)) ])
      names(y0) <- paste0("y",1:length(y0))
      
      deeptox:::show_modal_spinner( text = "Construct all time windows.", top = "90%" )
      ## add zero concentration to before and after profile ####
      paddedProfiles <- lapply(profiles, add_padding,
                               dt = padDt,
                               windowLength = windowLength)
      ## construct all time windows ####
      windows <- lapply(paddedProfiles, organise_windows,
                        windowLength = windowLength,
                        interval = interval)
      remove_modal_spinner()
        
      
      ## get rid of ones which will never be worst case. ####
      if (thinning) windows_proc <- lapply(windows, thinWindows)
      if (!thinning) windows_proc <- windows

      ## run windows ####
      thisProfile <- lapply(names(windows_proc), function(i){
        
        progressBarName <- paste0(
          which(names(windows_proc) == i),"/",length(windows_proc)," ",
          i
          )

        # for new version that prevents excessive shrinkage of the organism
        parms["L0"] <- y0[1]
        
        moving_time_windows(
          windows=windows_proc[[i]],
          parms=parms,
          y0=y0,
          cutOff = cutOff,
          ECpct = ECpct,
          detail = detail,
          passFail = passFail,
          progressBar = "shiny",
          scenName = progressBarName
        )
      })
      names(thisProfile) <- names(windows_proc)
      
      ## generate results as reactive values ####
      totalSummary <- do.call(rbind,
                              lapply(names(windows),
                                     summaryForSingleProfile,
                                     windows = windows,
                                     windows_proc = windows_proc,
                                     thisProfile = thisProfile,
                                     passFail = passFail,
                                     roundEPx = 3
                              )
      )
      # add the time and date to the summary to table to 
      totalSummary[["DateTime"]] <- get_time_human()
      
      
      plotEPx_res <- lapply(names(paddedProfiles), function(i){
        tmax <- max(paddedProfiles[[i]][,1])
        plotEPx(thisProfile[[i]][,"startTime"],
                thisProfile[[i]][,"EPxResults"],
                interval=interval,
                tmax=tmax,
                windowLength=windowLength,
                passFail=passFail,
                cutOff=cutOff,
                xunit=rvtl(parameters_rv)[["Time"]]
                ) +
          ggtitle(i)
      })
      names(plotEPx_res) <- names(paddedProfiles)
      
      summaryResults_rv(totalSummary)
      
      lapply(names(thisProfile), function(i){
        profileResults_rv[[i]] <- thisProfile[[i]]
        plotResults_rv[[i]] <- plotEPx_res[[i]]
      })

      
    }) # end of observe event "exposure assessment"
    
    
    # summary results table ####
    summaryResultsTableToRender <- reactive({
      req(ncol(summaryResults_rv()))
      
      summaryResultsTable <- summaryResults_rv()
      summaryResultsTable <- summaryResultsTable[, colnames(summaryResultsTable) != "DateTime"]
      
      newcolnames <- c("Profile names", "Pass/Fail", "EPx", "Critical endpoint", "Total no. of VTTs", "No. of VTTs thinned", "No. of VTTs tested")
      colnames(summaryResultsTable) <- newcolnames
      return(summaryResultsTable)
    })
    
    output[["summaryResultsTable"]] <- DT::renderDataTable(
      summaryResultsTableToRender(),
      escape = F,
      rownames= FALSE,
      options = list(
        paging=F,
        info=FALSE,
        dom='t',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      )

    
    selRows <- reactive({
        sort(input[[paste0("summaryResultsTable","_rows_selected")]])
      })
    
    profilesSelected <- reactive({
      if (length(selRows()))
        summaryResults_rv()[selRows(),"profileNames"]
      else
        NULL
      })
    
    plotsSelected <- reactive({
      if (length(profilesSelected()))
        rvtl(plotResults_rv)[profilesSelected()]
      else
        NULL
      })
    
    
    # create plots based on selecion in summary table ####
    observeEvent(profilesSelected(),{
      print(profilesSelected())
      
      if (length(profilesSelected())){
        lapply(seq_along(profilesSelected()), function(i){
          
          output[[paste0("resultsPlot_",i)]] <- renderPlot(
            plotsSelected()[[i]] +
              theme(axis.text=element_text(size=13)) +
              theme(axis.title=element_text(size=14)) +
              theme(legend.text=element_text(size=12))
            )
        })
      }else{
        NULL
      }
      
    })
    
    output[["resultsPlots"]] <- renderUI({
      if (length(profilesSelected())){
        lapply(seq_along(profilesSelected()), function(i){
          tagList(
            plotOutput(ns(paste0("resultsPlot_",i))),
            tags$br()
          )
        })
      }else{
        NULL
      }
    })
    
    
    # Warning profiles ####
    output[["warningProfile"]] <- renderUI({
      if ( (!length(expProfiles_rv())) & triggerProfileWarning() ){
        HTML(paste0(
          "<div class=\"text-danger\">
        <b>No profile selected.<br></div>"
        ))
        
      }
      else
        NULL
    })
    
    # Warning parameters ####
    output[["warningParameters"]] <- renderUI({
      if ( triggerParameterWarning() & any(!parsInsideLimits()) ){
        HTML(paste0(
          "<div class=\"text-danger\">
        <b>Some parameters are outside of the allowed limits or not specified.<br></div>"
        ))
        
      }
      else
        NULL
    })
    
    
    # Warning windowlength ####
    output[["warningWindowlength"]] <- renderUI({
      # check if the windowLength fits to the length of the profiles ####
      profiles <- expProfiles_rv()
      windowLength <- parameters_rv[["windowLength"]]
      
      checked_wl <- check_window_length(profiles, windowLength)

      if ( length(checked_wl)>0 ){
        HTML(paste0(
          "<div class=\"text-warning\">
        <b>The window length is longer than the profile length. The window length will be set to the profile length before running the exposure assessment.<br></div>"
        ))
      }
      else
        NULL
    })
    
    
    # MODAL FOR BOX INFO ####
    observeEvent(input[["boxinfo"]],{
      ns <- session$ns

      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Results"),
                                "<br>",
                                textblocks()[["resultsBoxInfo"]] # text body
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
# mod_results_ui("results_ui_1")
    
## To be copied in the server
# mod_results_server("results_ui_1")
