#' parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = tags$b("Parameters and settings"),
          width=12, collapsible=T, collapsed = F,
        tags$div(
          HTML(
            textblocks()[["parameterBoxIntro"]],
            "<br>"
          ),
          class="box_intro_text"
        ),
        
          fluidRow(
            tags$div(
              downloadButton(ns("exportParsBtn"), "Save"),
              style="margin-left: 15px;"
            ),
            tags$div(
              fileInputOnlyButton(
                ns("importParsBtn"),
                buttonLabel=list(
                  icon("upload", class = NULL,lib = "font-awesome"),
                  "Load"
                ),
                accept=".csv",
                width=72
              ),
              style="margin-left: 92px; margin-bottom: -15px; margin-top: -34px;"
            )
          ),

          
          # Input parameters and setting ####
        fluidRow(
          ## Project info box ####
          box(
            title = makeTextWithInfoButton("Project information", infoButtonId = ns("boxinfo_projectInfo")),
            solidHeader = T, collapsible = T,  status = "primary", collapsed = F, width=boxwidth,
            mod_parameterInputGroup_ui(ns("projectInfo"))
          ),
          box(
            title = makeTextWithInfoButton("Units", infoButtonId = ns("boxinfo_unitsInfo")),
            solidHeader = T, collapsible = T,  status = "primary", collapsed = F, width=boxwidth,
            mod_parameterInputGroup_ui(ns("units"))
          )
        ),
        
          fluidRow(
             ## Physical parameters box ####
            box(
              title = makeTextWithInfoButton("Physiological parameters", infoButtonId = ns("boxinfo_physParameters")), 
              solidHeader = T, collapsible = T, status = "primary", collapsed = F, width=boxwidth,
              mod_parameterInputGroup_ui(ns("physParameters"))
            ),
            ## Kinetics box ####
            box(
              title = makeTextWithInfoButton("Kinetics", infoButtonId = ns("boxinfo_kinetics")),
              solidHeader = T, collapsible = T, status = "primary", collapsed = F, width=boxwidth,
              mod_parameterInputGroup_ui(ns("kinetics")),
              mod_parameterInputGroup_ui(ns("kinetics_Lmtk"))
            ),
            ## Sublethal dynamics box ####
            box(
              title = makeTextWithInfoButton("Sublethal dynamics", infoButtonId = ns("boxinfo_sublethDynamics")),
              solidHeader = T, collapsible = T, status = "primary", collapsed = F, width=boxwidth,
              mod_parameterInputGroup_ui(ns("sublethDynamics")),
              ## Mode(s) of actions box ####
              makeTextWithInfoButton(tags$b("Physiological modes of action (pMoAs)"), infoButtonId = ns("boxinfo_pMoA")),
              mod_parameterInputGroup_ui(ns("pMoA"))
            )
            

          ),
          fluidRow(
            ## Advanced parameters box ####
            box(
              title = makeTextWithInfoButton("Advanced parameters", infoButtonId = ns("boxinfo_advParameters")), 
              solidHeader = T, collapsible = T, status = "info", collapsed = T, width=boxwidth,
              tagList(
                tags$div("Only used to calculate XR (Feedback: Dilution by reproduction)", class="inbox-section-header"),
                mod_parameterInputGroup_ui(ns("advParameters1")),
                tags$hr(),
                tags$div("Only used when starvation occurs", class="inbox-section-header"),
                mod_parameterInputGroup_ui(ns("advParameters2"))
              )
            ),

            ## Simulation settings box ####
            box(
              title = "Simulation settings",
              solidHeader = T, collapsible = T, status = "info", collapsed = T, width=8,
              column(width=12,
                     uiOutput(ns("simSettingBoxInfoText")),
                     tags$br(), tags$br()
                     ),
                column(width=6,
                       mod_parameterInputGroup_ui(ns("SimSettings1"))
                ),
                column(width=6,
                       mod_parameterInputGroup_ui(ns("SimSettings2"))
                )
            )
          )
          
          
      )
    )
  )
}
    
#' parameters Server Functions
#'
#' @noRd 
mod_parameters_server <- function(id, parameters_rv, triggerParamUpdate){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # Parameters and settings ####
    observeEvent(input$par_LmTK_active, {
      if (input$par_LmTK_active)
        shinyjs::enable("par_Lm_TK")
      if (!input$par_LmTK_active)
        shinyjs::disable("par_Lm_TK")
    }, ignoreInit = TRUE)
    
    
    # Generate all parameter input groups ####
    mod_parameterInputGroup_server("projectInfo", c("Name","Species","Compound"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("units", c("Length","Time","Concentration"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("physParameters", c("f","rB","y0_1","Lp", "Lm", "Rm"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("sublethDynamics", c("z_b","b_b"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("kinetics", c("kd","Xu","Xe", "XG", "XR"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("kinetics_Lmtk", c("Lm_TK"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("pMoA", c("SA","SM","SG", "SR1", "SR2"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("advParameters1", c("F_BV","K_RV"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("advParameters2", c("yP", "kap"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("SimSettings1", c("ECpct", "windowLength", "interval", "thinning"), parameters_rv, triggerParamUpdate)
    mod_parameterInputGroup_server("SimSettings2", c("cutOff","passFail","detail"), parameters_rv, triggerParamUpdate)
    
    
    

    
    ## export parameters ####
    output[["exportParsBtn"]] <- downloadHandler(
      filename = function(){
        fname <- paste0("parameters-",get_time_human(),".csv")
        return(fname)
      },
      content = function(file) {
        parorder <- do.call(c, lapply(initValues[["daphnia_debkiss"]], function(x) names(x)))
        exportParsAndSettings(rvtl(parameters_rv), outfile=file, parorder=parorder)
      }
    )
    
    
    # import parameters ####
    observeEvent(input[["importParsBtn"]], {
      pathname <- NULL
      tryCatch({
        
        pathname <- input[["importParsBtn"]]$datapath
      }, error = function(ex) {
      })
      if (!is.null(pathname)){
        importParsAndSettings(parameters_rv, pathname)
        print(paste0("remove file ",pathname))
        file.remove(pathname)
        triggerParamUpdate(Sys.time())
      }
 
    }, ignoreInit = TRUE)
    
    
    # renderUI for simulation settings box info ####
    output[["simSettingBoxInfoText"]] <- renderUI({
      
      infoButtonId <- ns("boxinfo_movingTimewindow")
      id <- sub("^(.*-).*$","\\1",infoButtonId)
      
      HTML(paste0("In this section, various settings can be adjusted.
                     These include the criteria for risk assessment and several
                     options regarding the ",
                  actionLink(infoButtonId,
                             "'moving time window'",
                             onclick = HTML(getPosition(infoButtonId, nsid = id))
                  ),
                  " approach.")
      )
    })
    
    
    # MODAL FOR BOX INFO ####
    ## main box ####
    observeEvent(input[["boxinfo"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Parameters and settings"),
                                "<br>",
                                "" # text body
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    ## project info box ####
    observeEvent(input[["boxinfo_projectInfo"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Project information"),
                                "<br>",
                                "Information on the project's name, the modelled species and compound can be given." # text body
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    ## phys parameters box ####
    observeEvent(input[["boxinfo_physParameters"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Physiological parameters"),
                                "<br>",
                                textblocks()[["physParaBoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    ## Kinetics box ####
    observeEvent(input[["boxinfo_kinetics"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Kinetics"),
                                "<br>",
                                textblocks()[["kineticsBoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    ## Survival dynamics box ####
    observeEvent(input[["boxinfo_survDynamics"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Survival dynamics"),
                                "<br>",
                                textblocks()[["survDynamicsBoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    
    ## Sublethal dynamics box ####
    observeEvent(input[["boxinfo_sublethDynamics"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Sublethal dynamics"),
                                "<br>",
                                textblocks()[["sublethDynamicsBoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    
    ## Mode(s) of actions box ####
    observeEvent(input[["boxinfo_pMoA"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Mode(s) of actions"),
                                "<br>",
                                textblocks()[["pMoABoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    
    ## Advanced parameters box ####
    observeEvent(input[["boxinfo_advParameters"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Advanced parameters"),
                                "<br>",
                                textblocks()[["advParametersBoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    
    
    
    ## Simulation settings box ####
    observeEvent(input[["boxinfo_simSettings"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Simulation settings"),
                                "<br>",
                                textblocks()[["simSettingsBoxInfo"]]
                              )
                            ),
                            footer=tagList(NULL),
                            easyClose=TRUE,
                            marginTop = paste0(input[["btn_clicked_pos"]],"px"))
      )
    }, ignoreInit = TRUE)
    
    ## Moving time window box ####
    observeEvent(input[["boxinfo_movingTimewindow"]],{
      ns <- session$ns
      
      showModal(customModalDialog(title = NULL,
                            HTML(
                              paste0(
                                tags$h3("Moving time window"),
                                "<br>",
                                textblocks()[["movingTimewindowBoxInfo"]]
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
# mod_parameters_ui("parameters_ui_1")
    
## To be copied in the server
# mod_parameters_server("parameters_ui_1")
