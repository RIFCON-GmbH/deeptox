#' min_effect
#'
#' @param times vector to assess ODEs at corresponding concentration
#' @param conc corresponding external exposure concentrations
#' @param multiplier margin of safety multiplier applied to concentration
#' @param effectX is the x in EPx, what percent age do we want to see? - change to ECpct??
#' @param parms stores all parameter values for the DEB model
#' @param ctrlResults stores L, R and S at the end of the interval.
#' @param fitFound If true, script will also output which endpoint was the most sensitive
#'
#' @return the minimal effect
#' @noRd
min_effect <- function(times, conc, multiplier, effectX, parms, ctrlResults, fitFound, y0) {

  concsTest <- multiplier*conc
  
  forcings <- matrix(c(times, concsTest), ncol = 2)
  forcings[,2] <- as.numeric(as.character(forcings[,2]))
  
  out <- ode(y0, times, func = "derivs", parms = parms,
             dllname = "deeptox",
             initfunc = "initmod", initforc = "forcc", forcings = forcings, nout = 1, outnames = "Sum",
             maxsteps = 10000)
  

  if ( length(times) == nrow(out) ){ # unequal length happens if ode doesn't converge for the whole timeseries most likely caused by excessive shrinkage and damage: interprete as death 
    L_frac <- out[length(times),2]/ctrlResults[2] # compare each to control result at same time.
    R_frac <- out[length(times),3]/ctrlResults[3]
    S_frac <- out[length(times),4]/ctrlResults[4]
    minRes <- min(L_frac, R_frac, S_frac) # take minimum as the driver of EPx decision
  } else {
    minRes <- 0 # equivalent to 0 survival
  }



  if (is.na(minRes)) {
    # NaN cause by extreme starvation (to L=0) - take this as death!
    minRes <- 0 # Multiplier too high, equivalent to 0 survival
  }
  zero <- minRes - (100 - effectX)/100 # >0 means less than effectX effect (higher multiplier required) and vice versa
  if (fitFound == TRUE) {
    # fit found, now just reporting the lowest endpoint
    id <- which.min(c(L_frac, R_frac, S_frac)) # which endpoint is most sensitive?
    translator <- c("Growth", "Reproduction", "Survival")
    mostSensitive <- translator[id] # converts id numeric to text of corresponding endpoint
    results <- list("z" = zero, "ms" = mostSensitive) # return multiplier value and most sensitive endpoint
  }  else {
    results <- zero # return multiplier value only
  }
  return(results)
}



#' multipleRootChecking
#'
#' In cases where multiple critical multiplier values can be found (Xe = 1 and SA, SM and/or SG = 1),
#'  this function works to check no lower value exists, or finds it if it does exist.
#'
#' @param times the current times (from the whole profile) for the model to run
#' @param conc corresponding external exposure concentrations
#' @param ctrlResults Growth, survival and reproduction fraction under zero exposure for the same time window
#' @param rootOut The previously identified critical multiplier (and additional info), now used as upper bound
#' @param nTests number of values between root and 0 to test for sign changes
#' @param ECpct 
#' @param y0 
#' @param parms 
#' 
#' @return the lower root value
#' @noRd
multipleRootChecking <- function(times, conc, ctrlResults, rootOut, nTests = 100, ECpct, y0, parms) {

  mTests <- seq(0, rootOut$root - rootOut$root/nTests, by = rootOut$root/nTests) # vector of multipliers to test
  testResults <- rep(0, nTests) # initialize results vector
  for (i3 in 1:nTests) {
    mT <- mTests[i3] # set multiplier
    testResults[i3] <- min_effect(times, conc, mT, ECpct, parms, ctrlResults, fitFound = FALSE, y0=y0) # find function result at this multiplier
  }
  if (min(testResults) <= 0) {
    # negative means multiplier is having greater than desired ECpct effect, a lower multiplier exists!
    warning(paste("Non-unique multiplier value, start time: ", times[1], ". Recalculating for lower value.", sep = ""))
    ub2 <- testResults[testResults<= 0] # look at all negative values
    ub2 <- ub2[1] # take the smallest negative in case there are more than one, this will give smallest multiplier.
    ub2Index <- which(testResults == ub2) # find which test number did it to recover multiplier
    ub2 <- mTests[ub2Index]
    if (ub2Index >= 1) {
      lb2 <- mTests[ub2Index-1] # previous multiplier must be positive, root lies between these mT values
    } else {
      # if first entry is negative then there is no previous entry, set lb to 0 instead.
      lb2 <- 0
    }
    # find lower root value.
    rootOut <- uniroot(min_effect, c(lb2, ub2), times = times, conc = conc, effectX = ECpct, parms = parms, ctrlResults = ctrlResults, fitFound = FALSE, tol = 1e-5, y0=y0)
  }
  return(rootOut) # this is same as original value if no lower root found
}