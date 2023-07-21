#' getPosition
#'
#' @param elementname 
#' @param pos 
#'
#' @return js code that gets the position of element with 'elementname"
#' @noRd
getPosition <- function(elementname, pos="top", nsid=""){
  paste0("
  var out = document.getElementById('",elementname,"').getClientRects()[0].",pos,"-100
  Shiny.setInputValue('",nsid,"btn_clicked_pos', out);
  //alert(out);
         ")
}


#' deactivateMouseWheel
#'
#' Deactivates the changing of the numeric input field with the mouse wheel
#'
#' @import htmltools
#' @return js code to deactivate the mouse wheel
#' @noRd
deactivateMouseWheel <- function(){
  # Deactivate mousewheel
  tags$script(HTML("
      $(function(){ 
        // Disable Mouse scrolling
        $('input[type=number]').on('wheel',function(e){ $(this).blur();});
        
        // Disable keyboard scrolling
        $('input[type=number]').on('keydown',function(e) {
          var key = e.charCode || e.keyCode;
          // Disable Up and Down Arrows on Keyboard
          if(key == 38 || key == 40 ) {
            e.preventDefault();
          } else {
            return;
          }
        });
      });
      "))
}


#' js_center_modal
#'
#' @param translateX 
#' @param translateY 
#' @param top 
#' @param left 
#'
#' @source modified from 'shinybusy:::js_center_modal'
#' 
#' @import htmltools
#'
#' @return js code to center a modal
#' @noRd
js_center_modal <- function(translateX = "-50%",
                            translateY = "-100%",
                            top="50%", 
                            left="50%"
                            ){
  tags$script(
    HTML(
    paste(
      sep = ".",
      "$('.shinybusy-modal')",
      "parent()",
      "parent()",
      "css('position', 'fixed')",
      paste0("css('top', '", top ,"')"),
      paste0("css('left', '", left ,"')"),
      paste0("css('transform', 'translate(",translateX,",",translateY,")');")
    )
  )
  )
}

#' scrollIntoView
#' 
#' Wrapper for js function scrollIntoView.
#' Scroll window to the anchor id.
#'
#' @param id the html anchor id
#'
#' @return js code to scroll the element with 'id' in to the viewable screen
#' @source https://stackoverflow.com/questions/61757698/how-to-go-to-an-html-anchor-programatically-in-shiny
#' @importFrom shinyjs runjs
#' @noRd
scrollIntoView <- function(id){
  runjs(paste0('document.getElementById(\"',id,'\").scrollIntoView();'))
}
