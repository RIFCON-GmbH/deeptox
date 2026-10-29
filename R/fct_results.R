#' Plot EPx
#'
#' function plots the EPx multiplier curve. In cases where thinning has been used multiple
#'  separate lines are plotted.
#'
#' @param sT startTimes of all windows
#' @param EP_V EPx values of all windows
#' @param interval length of time (in days) between starts of successive windows
#' @param tmax end time of the profile
#' @param windowLength How long (days) is the time window
#' @param passFail chronic AF value used in all risk assessments for pass/fail.
#' @param cutOff the cutOff value is used to determine the y-axis maximum
#'
#' @return a ggplot object of the EPx
#' @export
plotEPx <- function(sT, EP_V, interval, tmax, windowLength=21, passFail=10, cutOff=NA, xunit=NA) {
  
  xlab0 <- "Start time"
  xlab <- ifelse( is.na(xunit), xlab0, paste0(xlab0," (",xunit,")"))
  
  
  if ( is.na(cutOff))
    ylim_max <- 1000
  else
    ylim_max <- cutOff
  
  ylim_ymin <- min(1, roundNearestPow(min(EP_V), direction="down"))
  
  sT2 <- c(sT[2:length(sT)], length(sT)) # shift startTimes by 1 index
  timeDiffs <- abs(sT2 - sT)
  timeStops <- which(timeDiffs > (1.01*interval)) # report indices of gaps, 1.01* interval prevents rounding issues
  timeStops <- c(0, timeStops, sT[length(sT)])
  p <- ggplot() + 
    labs(y="EPx multiplier", x = xlab) +
    xlim(-windowLength+interval, tmax)
  for (ii in 1:(length(timeStops)-1)) {
    x <- sT[(timeStops[ii]+1):timeStops[ii+1]]
    y <- EP_V[(timeStops[ii]+1):timeStops[ii+1]]
    p <- p + geom_line(data = data.frame(x, y), mapping = aes(x, y, color = 'EPx'))
    rm(x, y)
  }
  xmin = -windowLength+interval
  xmax = max(tmax)
  x = c(xmin, xmax)
  ymin = passFail
  ymax = passFail
  y = c(ymin, ymax)
  p <- p +
    geom_line(data = data.frame(x = c(-windowLength+interval, tmax), y = c(passFail, passFail)), mapping = aes(x, y, color = 'thres.'))
  p <- p +
    scale_y_continuous(trans = "log10", limits = c(ylim_ymin, ylim_max)) +
    scale_color_manual(values = c('EPx' = 'black', 'thres.' = 'red'))+
    labs(color = 'Legend') +
    theme(legend.title = element_blank(), legend.position = "top")
  
  return(p)
  
}

#' Reconstruct EPx plots from imported JSON data
#'
#' Creates a list of EPx plots from an object imported via
#' [jsonlite::read_json()]. The function extracts simulation settings,
#' metadata, exposure profiles, and profile results from the imported
#' structure and recreates the corresponding plots using [plotEPx()].
#'
#' This function is primarily intended for internal use when restoring
#' previously exported simulation results.
#'
#' @param df_from_json A list created from a JSON file containing the
#'   elements `simulationSettings`, `metaData`, `expProfiles`, and
#'   `profileResults`.
#'
#' @return A named list of `ggplot` objects. Each list element corresponds
#'   to one exposure profile and is named according to the entries in
#'   `expProfiles`.
#'
#' @details
#' For each exposure profile, the maximum simulation time is determined
#' from the profile definition and passed to [plotEPx()]. Plot titles are
#' set to the corresponding profile names.
#'
#' @seealso [plotEPx()]
reconstruct_plotEPX <- function(df_from_json){
  
  sim_settings <- df_from_json$simulationSettings
  md <- df_from_json$metaData
  expProfiles <- df_from_json$expProfiles
  profileResults <- df_from_json$profileResults
  
  if (length(profileResults) == 0){
    return(list())
  }
  
  plotEPx_res <- lapply(names(expProfiles), function(i){
    tmax <- max(expProfiles[[i]][,1])
    plotEPx(profileResults[[i]][,"startTime"],
            profileResults[[i]][,"EPxResults"],
            interval = sim_settings$interval,
            tmax = tmax,
            windowLength = sim_settings$windowLength,
            passFail = sim_settings$passFail,
            cutOff = sim_settings$cutOff,
            xunit = md$Time
    ) +
      ggplot2::ggtitle(i)
  })
  names(plotEPx_res) <- names(expProfiles)
  
  plotEPx_res
}


#' Plot failures_rect
#' 
#' function plots the EPx multiplier curve. In cases where thinning has been used multiple
#'  separate lines are plotted.
#'
#' @param profile time and concentration vectors for the whole profile plus padding
#' @param sT start times of all windows
#' @param EP_V EPx results for all windows
#' @param interval length of time (in days) between starts of successive windows
#' @param tmax end time of the profile
#' @param windowLength How long (days) is the time window
#' @param passFail chronic AF value used in all risk assessments for pass/fail.
#'
#' @return a ggplot object of the failures
#' @export
plotFailures_rect <- function(profile, sT, EP_V, interval, tmax, windowLength=21, passFail=10, yunit=NA, xunit=NA) {
  
  xlab0 <- "Time"
  ylab0 <- "External conc."
  xlab <- ifelse( is.na(xunit), xlab0, paste0(xlab0," (",xunit,")"))
  ylab <- ifelse( is.na(yunit), ylab0, paste0(ylab0," (",yunit,")"))
  
  profileFrame <- data.frame(profile)
  p <- ggplot() + 
    geom_line(data=profileFrame, mapping=aes(V1, V2)) + 
    labs(y=ylab, x = xlab)
  
  sT <- sT[EP_V < passFail] # only interested in startTimes which fail
  failures <- EP_V[EP_V < passFail]
  sT2 <- c(sT[2:length(sT)], length(sT)) # shift startTimes by 1 index
  timeDiffs <- abs(sT2 - sT)
  timeStops <- which(timeDiffs > (1.01*interval)) # report indices of gaps due to thinning, 1.01* interval prevents rounding issues
  xStart <- rep(0, length(timeStops) + 1)
  xEnd <- rep(0, length(timeStops) + 1)
  timeStops <- c(0, timeStops, sT[length(sT)])
  
  for (ii in 1:(length(timeStops)-1)) {
    xStart[ii] <- sT[(timeStops[ii]+1)]
    xEnd[ii] <- sT[timeStops[ii+1]]
  }
  p <- p + geom_rect(aes(xmin = xStart, ymin = 0, xmax = xEnd, ymax = max(profile[,2])), alpha =0.2, fill = "red") # shaded region

  return(p)
}

#' Summarize single profile
#'
#' @param profileName the name of the profile to analyse
#' @param windows all windows available for all profiles
#' @param windows_proc processed windows for all profiles
#' @param thisProfile the profile results
#' @param passFail the threshold at which pass/fail are assessed
#' @param roundEPx the number of digits to which the EPx should be rounded; NA: no rounding
#'
#' @return summary table for a single profile
#' @export
summaryForSingleProfile <- function(profileName, windows, windows_proc, thisProfile, passFail, roundEPx=NA){
  profSum <- profileSummary(thisProfile[[profileName]], passFail, roundEPx)
  VTTSum <- VTTsummary(profileName=profileName, windows=windows, windows_proc=windows_proc)
  
  out <- data.frame(profileNames=profileName, profSum,VTTSum)
  return(out)
}

#' Summarize profiles
#'
#' @param profileRes the profile results
#' @param passFail the threshold at which pass/fail are assessed
#' @param roundEPx the number of digits to which the EPx should be rounded; NA: no rounding
#'
#' @return summary for a single profile
#' @export
profileSummary <- function(profileRes, passFail, roundEPx=NA){
  EPx <- min(profileRes$EPxResults)
  if (!is.na(roundEPx)) EPx <- round(EPx, digits = roundEPx)
  minEntry <- which.min(profileRes$EPxResults)
  critEndpoint <- profileRes$critEndpoint[minEntry]
  if (EPx >= passFail) {
    pass_fail_results <- "Pass"
  } else {
    pass_fail_results <- "Fail"
  }
  
  out <- data.frame(
    pass_fail_results=pass_fail_results,
    EPx=EPx,
    critEndpoint=critEndpoint
  )
  
  return(out)
}

#' Summarize VTTs
#' 
#' The function summarized the virtual toxicity tests (VTTs)
#'
#' @param profileName the name of the profile to analyse
#' @param windows all windows available for all profiles
#' @param windows_proc processed windows for all profiles
#'
#' @return a summary of the VTT summary
#' @export
VTTsummary <- function(profileName, windows, windows_proc){
  total_VTTs <- length(windows[[profileName]][[1]])
  No._VTTs_tested <- length(windows_proc[[profileName]][[1]])
  No._VTTs_thinned <- total_VTTs-No._VTTs_tested
  
  out <- data.frame(total_VTTs, No._VTTs_thinned, No._VTTs_tested)
  return(out)
}