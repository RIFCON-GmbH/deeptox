#' electronTop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @source https://www.w3schools.com/css/css_navbar_horizontal.asp
#' @importFrom shiny NS tagList 
mod_electronTop_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("navbarTop"))
  )
}

#' electronTop Server Functions
#'
#' @noRd 
mod_electronTop_server <- function(id, show){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    
    observeEvent(input[["refresh"]],{
      session$reload()
    })
    
    observeEvent(input[["about"]],{
      showModal(
        modalDialog(
          tagList(
            tags$img(style="float: right; padding-right: 20px", alt="deeptox logo", src="www/hex-DeEP.png", height=100),
            tags$h1("deeptox"),
            paste0("Version ", packageVersion("deeptox")),
            tags$hr(),
            HTML(paste0("This deeptox tool is build with the R-package 'electricShine'
                        available from github at <a href='https://github.com/chasemc/electricShine' target='_blank'>github.com/chasemc/electricShine</a>
                        using the fork at <a href='https://github.com/erikvona/electricShine' target='_blank'>github.com/erikvona/electricShine</a>
                        that both utilize <a href='https://www.electronjs.org/', target='_blank'>electron</a>,
                        a framework for 'creating native applications with web technologies JavaScript, HTML, and CSS',
                        to build standalone applications from web apps build with 
                        <a href='https://shiny.rstudio.com/' target='_blank'>shiny</a>.
                        <br><br>",
                        "This electron app is bundled with <a href='https://www.r-project.org/' target='_blank'>R</a> version ",paste(version[c("major","minor")],collapse="."),"
                        and <a href='https://nodejs.org/en/' target='_blank'>node.js</a> version 14.19.3.<br><br>",
                        "Further information on the tool itself its function and documentations can be found at <a href='https://deeptox-tox.info/' target='_blank'>deeptox-tox.info/</a>
                        <br><br>
                        Developed by<br>
                        <img src=\"www/rifcon_logo_nc_03.png\" alt=\"Rifcon company logo\" height=50>   <img src=\"www/Syngenta_Logo.svg.png\" alt=\"Syngenta company logo\" height=50>
                        "))
            ),
          title = "",
          footer = tagList(
            modalButton("Close")
            )
          )
        )
      })
    
    
    output[["navbarTop"]] <- renderUI({
      
      #inspired by https://www.w3schools.com/css/tryit.asp?filename=trycss_dropdown_right
      content_for_electron <- tagList(
        HTML(paste0("
        <div class='electron-dropdown' style='float:right;'>
          <button class='dropbtn'><i class='fa fa-bars'></i></button>
          <div class='dropdown-content'>
            <a class='action-button shiny-bound-input' id='",ns("refresh"),"' href='#'><i class='bi bi-arrow-clockwise'></i>Refresh</a>
            <a class='action-button shiny-bound-input' id='",ns("about"),"' href='#'><i class='bi bi-card-text'></i>About</a>
          </div>
        </div>
        "))
      )
      
      if (show()){
        return(content_for_electron)
      }else{
        return(NULL)
        #return(tags$img(style="float: right; padding-right: 20px; padding-bottom: 10px;", alt="deeptox logo", src="www/hex-deeptox.png", height=90))
      }
      
    })
    
  })
}
    
## To be copied in the UI
# mod_electronTop_ui("electronTop_ui_1")
    
## To be copied in the server
# mod_electronTop_server("electronTop_ui_1")
