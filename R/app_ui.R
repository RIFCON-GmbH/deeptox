#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import dashboardthemes
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    dashboardPage(
      skin="red",
      
      header=dashboardHeader(disable = TRUE),
      sidebar=dashboardSidebar(disable = TRUE),
  
      body=dashboardBody(
  
       customTheme,
       
       mod_electronTop_ui("electronTop"),

       # Leave this function for adding external resources
       golem_add_external_resources(),
       
       # Project import and export buttons ####
       mod_projectExportImport_ui("projectExportImport_ui"),
        
       # Welome text ####
       mod_welcome_ui("welcome"),
       
       # Input parameters and setting ####
       mod_parameters_ui("parameters_ui"),
       
       # Exposure profile ####
       mod_exposureProfiles_ui("exposureProfiles_ui"),
       
       # Results ####
       mod_results_ui("results_ui"),
       
       #verbatimTextOutput("sessionInfo")
       uiOutput("sessionInfo")
       
    )
  ),
  
  HTML("<div data-iframe-height style=\"position: relative;\"></div>")# empty div-container marking lowest position on page for the iframe resizer
  
  
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'deeptox'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    
    # add bootstrap icons
    HTML("<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.3/font/bootstrap-icons.css'>"),
    
    # script necessary for info of iframe resizing
    HTML("<script src=\"https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/4.3.2/iframeResizer.contentWindow.js\" integrity=\"sha512-cJ7aOLpXbec1Km9craM6xL6UOdlWf9etIz7f+cwQv2tuarLm3PLb3dv3ZqIK++SE4ui+EE0nWqKB0dOaAOv9gQ==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\"></script>"),
    
    # script for enabling of tracking by Matamoto
    HTML("
         <!-- Matomo -->
          <script>
            var _paq = window._paq = window._paq || [];
            /* tracker methods like \"setCustomDimension\" should be called before \"trackPageView\" */
            _paq.push(['trackPageView']);
            _paq.push(['enableLinkTracking']);
            (function() {
              var u=\"//www.deep-tox.info/Analytics/\";
              _paq.push(['setTrackerUrl', u+'matomo.php']);
              _paq.push(['setSiteId', '1']);
              var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
              g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
            })();
          </script>
          <!-- End Matomo Code -->
         ")
  )
}

