#' fileInputOnlyButton
#'
#' A simplified version of the shiny::fileInputButton.
#' Use in the same way as you would use shiny::fileInputButton.
#'
#' @param ... 
#' @param label 
#'
#' @return a file input button DOM
#' @noRd
fileInputOnlyButton <- function(..., label="") {
  temp <- fileInput(..., label=label)
  # Cut away the label
  temp$children[[1]] <- NULL
  # Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  # Remove input group classes (makes button flat on one side)
  temp$children[[1]]$attribs$class <- NULL
  temp$children[[1]]$children[[1]]$attribs$class <- NULL
  temp
}



#' customModalDialog
#' 
#' customized version of the the ModalDialog call that is able to specify the
#'  position of the modal
#'
#' @param ... parameters for shiny::modalDialog
#' @param marginTop modal is placed with the given top margin
#'
#' @importFrom shiny modalDialog
#'
#' @return a modal dialog DOM
#' 
#' @source modifies the DOM created by 'shiny::modalDialog'
#' @noRd
customModalDialog <- function (..., marginTop){

  temp <- shiny::modalDialog(...)
  temp$children[[1]]$attribs[["style"]] <- paste0("position: absolute; margin-top: ", marginTop,"; margin-left: 5%;")
  return(temp)

}



#' show_modal_progress_circle
#'
#' @param value 
#' @param text 
#' @param color 
#' @param stroke_width 
#' @param easing 
#' @param duration 
#' @param trail_color 
#' @param trail_width 
#' @param height 
#' @param session 
#'
#' @source modified from 'shinybusy::show_modal_progress_circle'
#'
#' @return a progress circle
#' @noRd
show_modal_progress_circle <- function (value = 0,
                                        text = "auto",
                                        color = "#112446", 
                                        stroke_width = 4,
                                        easing = "linear",
                                        duration = 1000, 
                                        trail_color = "#eee",
                                        trail_width = 1,
                                        height = "200px", 
                                        session = shiny::getDefaultReactiveDomain(),
                                        translateX = "-50%",
                                        translateY = "-100%",
                                        top="50%", 
                                        left="50%"
                                        ){
  showModal(
    modalDialog(
      class = "shinybusy-modal",
      js_center_modal(translateX=translateX, translateY=translateY, top=top, left=left),
      shinybusy::progress_circle(value = value,
                       text = text,
                       color = color,
                       stroke_width = stroke_width,
                       easing = easing,
                       duration = duration,
                       trail_color = trail_color,
                       trail_width = trail_width,
                       height = height,
                       width = height,
                       shiny_id = session$ns("shinybusy-modal-progress")
      ),
      footer = NULL,
      easyClose = FALSE,
      fade = FALSE,
      size = "m"
    ),
    session = session)
}




#' show_modal_spinner
#'
#' @param spin 
#' @param color 
#' @param text 
#' @param session 
#' 
#' @source modified from 'shinybusy::show_modal_spinner'
#'
#' @return a modal spinner
#' @noRd
show_modal_spinner <- function (spin = "double-bounce",
                                color = "#112446", 
                                text = NULL,
                                session = shiny::getDefaultReactiveDomain(),
                                translateX = "-50%",
                                translateY = "-100%",
                                top="50%", 
                                left="50%"
                                ) 
  {
  spin <- match.arg(arg = spin,
                    choices = c(
                      shinybusy:::spinkit_spinners(),
                      shinybusy:::epic_spinners()
                      )
                    )
  
  if (spin %in% shinybusy:::spinkit_spinners()) {
    tag_spin <- shinybusy:::spin_kit(spin = spin, color = color)
  }
  else {
    tag_spin <- shinybusy:::spin_epic(spin = spin, color = color)
  }
    
  showModal(
    modalDialog(
      class = "shinybusy-modal",
      js_center_modal(translateX=translateX, translateY=translateY, top=top, left=left),
      shinybusy:::html_dependency_spinkit(),
      shinybusy:::html_dependency_epic(),
      tags$div(
        style = "width: 60px; height: 60px; position: relative; margin: auto;", 
        tag_spin
        ),
      tags$div(
        style = "text-align: center;", 
        text
        ),
      footer = NULL,
      easyClose = FALSE,
      fade = FALSE, 
      size = "s"
      ),
    session = session
    )
}