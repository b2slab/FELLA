checkArguments <- function(method = "diffusion", 
                           approx = "normality", 
                           loadMatrix = NULL, 
                           threshold = 0.05, 
                           plimit = 15, 
                           nlimit = 250, 
                           niter = 1e3, 
                           layout = F, 
                           splitByConnectedComponent = F, 
                           askPlots = T, 
                           thresholdConnectedComponent = 0.05, 
                           GO.CellularComponent = NULL,
                           GONamesAsLabels = T, 
                           LabelLengthAtPlot = 22, 
                           object = new("FELLA.USER"), 
                           data = new("FELLA.DATA")) {
  # METHOD
  ##############################################################################
  if (!is.character(method)) {
    message("'method' must be a character: 'hypergeom', 'diffusion', 'pagerank' or 'all'. Returning original 'object'...")
    return(list(ans = object, valid = F))
  }
  
  if (length(method) > 1) {
    message("'method' must be a length 1 character. Returning original 'object'...")
    return(list(ans = object, valid = F))
  }
  
  if (!(method %in% c("hypergeom", "diffusion", "pagerank", "all"))) {
    message("'method' must be a character: 'hypergeom', 'diffusion', 'pagerank' or 'all'. Returning original 'object'...")
    return(list(ans = object, valid = F))
  }
  
  # approx
  ##############################################################################
  
  if (!is.character(approx)) {
    message("'approx' must be a character: 'simulation' or 'normality'. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(approx) > 1) {
    message("'approx' must be a length 1 character. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (!(approx %in% c("simulation", "normality"))) {
    message("'approx' must be a character: 'simulation' or 'normality'. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # loadMatrix
  ##############################################################################
  if (!is.null(loadMatrix) & length(loadMatrix) > 1)  {
    message("'loadMatrix' can only be a length 1 character ('diffusion', 'pagerank', 'all') or NULL.")
    return(list(ans = NULL, valid = F))
  }
  
  if (!is.null(loadMatrix) & !is.character(loadMatrix)) {
    message("'loadMatrix' can only be a length 1 character ('diffusion', 'pagerank', 'all') or NULL.")
    return(list(ans = NULL, valid = F))
  }
  
  
  # threshold
  ##############################################################################
  if (!is.numeric(threshold)) {
    message("'threshold' must be numeric between 0 and 1. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(threshold) > 1) {
    message("'threshold' must be a length 1 numeric. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (threshold <= 0 | threshold >= 1) {
    message("'threshold' must be numeric between 0 and 1. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # plimit
  ##############################################################################
  if (!is.numeric(plimit)) {
    message("'plimit' must be numeric between 1 and 50. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(plimit) > 1) {
    message("'plimit' must be a length 1 numeric. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (plimit <= 0 | plimit >= 50) {
    message("'plimit' must be numeric between 1 and 50. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # nlimit
  ##############################################################################
  if (!is.numeric(nlimit)) {
    message("'nlimit' must be numeric between 1 and 1000. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(nlimit) > 1) {
    message("'nlimit' must be a length 1 numeric. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (nlimit <= 0 | nlimit > 1000) {
    message("'nlimit' must be numeric between 1 and 1000. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # niter
  ##############################################################################
  if (!is.numeric(niter)) {
    message("'niter' must be numeric between 1e2 and 1e5. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(niter) > 1) {
    message("'niter' must be a length 1 numeric. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (niter < 1e2 | niter > 1e5) {
    message("'niter' must be numeric between 1e2 and 1e5. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # layout
  ##############################################################################
  if (!is.logical(layout)) {
    message("'layout' must be logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }

  if (length(layout) > 1) {
    message("'layout' must be a length 1 logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # splitByConnectedComponent
  ##############################################################################
  if (!is.logical(splitByConnectedComponent)) {
    message("'splitByConnectedComponent' must be logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(splitByConnectedComponent) > 1) {
    message("'splitByConnectedComponent' must be a length 1 logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # askPlots
  ##############################################################################
  if (!is.logical(askPlots)) {
    message("'askPlots' must be logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(askPlots) > 1) {
    message("'askPlots' must be a length 1 logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }  
  # thresholdConnectedComponent
  ##############################################################################
  if (!is.numeric(thresholdConnectedComponent)) {
    message("'thresholdConnectedComponent' must be numeric between 0 and 1. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(thresholdConnectedComponent) > 1) {
    message("'thresholdConnectedComponent' must be a length 1 numeric. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (thresholdConnectedComponent <= 0 | thresholdConnectedComponent >= 1) {
    message("'thresholdConnectedComponent' must be numeric between 0 and 1. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # GO.CellularComponent
  ##############################################################################
  if (!is.null(GO.CellularComponent) & !is.character(GO.CellularComponent)) {
    message("'GO.CellularComponent' must be NULL or a GO entry. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(GO.CellularComponent) > 1) {
    message("'GO.CellularComponent' must be a length 1 character. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # splitByConnectedComponent
  ##############################################################################
  if (!is.logical(splitByConnectedComponent)) {
    message("'splitByConnectedComponent' must be logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(splitByConnectedComponent) > 1) {
    message("'splitByConnectedComponent' must be a length 1 logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  
  # GONamesAsLabels
  ##############################################################################
  if (!is.logical(GONamesAsLabels)) {
    message("'GONamesAsLabels' must be logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(GONamesAsLabels) > 1) {
    message("'GONamesAsLabels' must be a length 1 logical. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # LabelLengthAtPlot
  ##############################################################################
  if (!is.numeric(LabelLengthAtPlot)) {
    message("'LabelLengthAtPlot' must be numeric between 10 and 50. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (length(LabelLengthAtPlot) > 1) {
    message("'LabelLengthAtPlot' must be a length 1 numeric. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  if (LabelLengthAtPlot < 10 | LabelLengthAtPlot > 100) {
    message("'LabelLengthAtPlot' must be numeric between 10 and 100. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  # object
  ##############################################################################
  if (!is.FELLA.USER(object)) {
    message("'object' is not a FELLA.USER object. Returning NULL...")
    return(list(ans = NULL, valid = F))
  } 
  
  # data
  ##############################################################################
  if (!is.FELLA.DATA(data)) {
    message("'data' is not a FELLA.DATA object. Returning NULL...")
    return(list(ans = NULL, valid = F))
  }
  
  return(list(valid = T))
}