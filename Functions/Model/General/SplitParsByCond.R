SplitParsByCond <- function(params, which_pars) {
  ### Returns params with condition specifiers enabling different 
  # params to be fit depending on condition ###
  # Copy pars in which pars and label with NEU for neutral condition..
  pars_to_tack_on <- setNames(params[which_pars], paste0(which_pars, "_NEU"))
  # .. label the originals with RUM..
  names(params)[names(params) %in% which_pars] <- paste0(which_pars, "_RUM")
  # Tack on the neutral pars 
  params <- data.frame(params, pars_to_tack_on)
params  
}