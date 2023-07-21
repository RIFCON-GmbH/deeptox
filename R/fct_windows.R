#' Organise windows
#'
#' Takes the given profile and returns a list (of lists) with time and concentration vectors for all time windows
#'
#' @param profile 2 column data set: [time, conc]
#' @param windowLength duration (in days) of each window
#' @param interval how far apart each window is.
#'
#' @return time windows
#' @export
organise_windows <- function(profile, windowLength, interval) {

  # take the full FOCUS profile and build the time windows to test:
  fullTime <- profile[,1] # first column is times
  fullConc <- profile[,2] # concentrations
  
  finished <- FALSE # used to stop the while loop
  startTime <- profile[1,1] # time of the first window
  profileNum <- 1 # counter
  
  timeWindows <- list() # empty list to be completed in while loop
  concWindows <- list() # empty list
  
  while (isTRUE(finished)==FALSE) {
    endTime <- startTime + windowLength
    
    # find index of closest start and end time in the FOCUS profile.
    startTimeIndex <- which(abs(fullTime-startTime)==min(abs(fullTime-startTime)))
    endTimeIndex <- which(abs(fullTime-endTime)==min(abs(fullTime-endTime)))
    #  values between start and end times added to the list of windows
    timeWindows[[profileNum]] <- fullTime[startTimeIndex:endTimeIndex]
    concWindows[[profileNum]] <- fullConc[startTimeIndex:endTimeIndex]
    # iterate
    profileNum <- profileNum + 1
    startTime <- startTime + interval
    if (startTime >= fullTime[length(fullTime)] - windowLength) {
      finished = TRUE # stops when the proposed time window overlaps the endTime.
    }
    
  }
  result <- list(timeWindows, concWindows)
  return(result)
}

#' Thin windows
#'
#' thin out the list of windows to only those which might be worst case scenario
#'
#' @param windows 2dim array (list of lists), times concs
#'
#' @return time windows
#' @export
thinWindows <- function(windows) {

  times <- windows[[1]]
  concs <- windows[[2]]
  minList <- unlist(lapply(concs, min)) # get minimum concentration in each window, convert to a vector
  maxList <- unlist(lapply(concs, max)) # repeat for all maximums
  maxmin <- max(minList) # find largest minimum
  losers <- (maxList < maxmin) # record TRUE for all we want to lose. We lose those whose maximum is less than the largest minimum
  timesKeep <- times[losers == FALSE]
  concsKeep <- concs[losers == FALSE]
  # build list for output
  result <- list(timesKeep, concsKeep)
  return(result)
}

#' Apply moving time windows approach to exposure assessment
#' 
#' Assesses the effects of exposure in each time window at the cutOff and passFail values.
#' If detail is TRUE, it also calculates the exact values.
#'
#' @param windows list of lists - vectors for times and concentrations
#' @param cutOff safety cutoff that is good enough for us to move on
#' @param ECpct what percentage of effects do we want to see? Probably 10
#' @param tol tolerance around ECpct acceptable from the algorithm
#' @param detail Boolean, do we want to only get pass or fail results at passFail value?
#' @param passFail the passFail AF - usually 10.
#' @param progressBar defines the type of progress bar either "txt" for a text based progress bar or "shiny" for a shiny progress bar
#'
#' @return results of the EPx analysis for the moving time windows
#' @export
moving_time_windows <- function(windows, parms, y0, cutOff, ECpct, tol = 1e-5, detail, passFail, progressBar="txt", scenName="") {

  times <- windows[[1]] # list of all time window timepoints
  concs <- windows[[2]] # corresponding list of all time window concentrations
  windowlength <- length(windows[[1]])
  # Build vectors for outputs:
  EPxResults <- rep(0, windowlength)
  startTime <- rep(0, windowlength)
  critEndpoint <- rep(NA, windowlength)
  
  if (progressBar=="txt")
    pb <- txtProgressBar(min = 0, max = windowlength, style = 3) # progress bar to display in console
  if (progressBar=="shiny")
    #shinybusy::show_modal_progress_circle(text=paste0(scenName," Run windows. 0%"))
    deeptox:::show_modal_progress_circle(text=paste0(scenName," Run windows. 0%"), top="90%")
  
  for (i in 1:windowlength) {
    if (progressBar=="txt")
      setTxtProgressBar(pb, i) # update progress
    if (progressBar=="shiny")
      shinybusy::update_modal_progress(i/windowlength,
                                       text=paste0(scenName," Run windows. ",round((i/windowlength)*100,0),"%")
                                       )
    
    ### first get control results for this time vector
    control <- rep(0, length(times[[i]]))
    forcings <- matrix(c(times[[i]], control), ncol = 2)

    ctrlResults <- ode(y0, times[[i]], func = "derivs", parms = parms,
                       dllname = "deeptox",
                       initfunc = "initmod", initforc = "forcc", forcings = forcings, nout = 1, outnames = "Sum",
                       maxsteps = 10000)

    ctrlResults <- ctrlResults[length(ctrlResults[,1]),]
    ###
    startTime[i] <- times[[i]][1]
    
    # test the cutoff multiplier first:
    val <- min_effect(unlist(times[[i]]), unlist(concs[[i]]), cutOff, ECpct, parms, ctrlResults, fitFound = FALSE, y0=y0) 

    ub <- cutOff # upper bound default
    lb <- -1 # lower bound default
    if (val > 0) {# pass, move on
      EPxResults[i] <- cutOff # actual multiplier value is greater than the cutOff
      next # so we don't care about exact value, move on
    } else {
      # test again at passFail - will usually be the AF for chronic tests
      val <- min_effect(unlist(times[[i]]), unlist(concs[[i]]), passFail, ECpct, parms, ctrlResults, fitFound = FALSE, y0=y0)
      if (val >0) {
        # MF for this window is greater than passFail
        EPxResults[i] <- passFail
        lb <- passFail # true MF larger, so we use this as lower bound for root finding
      } else {
        # MF for this window is less than passFail
        ub <- passFail # true MF smaller, so we use this as upper bound for root finding
      }
      if (detail == FALSE) {
        next # don't want to calculate the true value
      }
      # calculating the true value:
      # Find lb
      test <- 1 # multiplier value to test
      while (lb < 0) {
        val <- min_effect(unlist(times[[i]]), unlist(concs[[i]]), test, ECpct, parms, ctrlResults, fitFound = FALSE, y0=y0)
        # testing result:
        if (val > 0) {# set lb
          lb <- val
        } else {
          test <- test/5 # test multiplier too high, shrink it.
        }
      }
      # Run Brent's algorithm using R's built in uniroot function, looking to zero the output of min_effect function
      rootOut <- uniroot(min_effect, c(lb, ub), times = times[[i]], conc = concs[[i]], effectX = ECpct, parms = parms, ctrlResults = ctrlResults, fitFound = FALSE, tol = tol, y0=y0, extendInt = "yes")
      if (parms["Xe"] == 1 && (parms["SA"] == 1 || parms["SM"] == 1)) {
        # potential for multiple roots to be found. Must be checked
        rootOut <- multipleRootChecking(times[[i]], concs[[i]], ctrlResults, rootOut, nTests = 10, ECpct=ECpct, y0=y0, parms=parms)
      }
      EPxResults[i] <- rootOut$root # This is critical MF value
      # run ODE one final time to identify critical endpoint (hence fitFound = TRUE):
      results <- min_effect(unlist(times[[i]]), unlist(concs[[i]]), rootOut$root, ECpct, parms, ctrlResults, fitFound = TRUE, y0=y0)
      critEndpoint[i] <- results$ms
    }
  }
  
  if (progressBar=="shiny")
    shinybusy::remove_modal_progress()
  
  
  # build the data frame of results to output:
  fullResults <- data.frame(startTime, EPxResults, critEndpoint)
}
