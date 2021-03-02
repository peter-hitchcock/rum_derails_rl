RunOpt <- function(this_df, sim_opt, helpers) {
  ### Run a single optimization, returning optim's result list ###
  CallOpt <- function(params,
                      this_df,
                      sim_opt,
                      helpers,
                      lb_ub
                      ) {
    ### Call opt and return res ###
    cat("\n ## Original params next run: ## \n"); print(params)
    ########################### INITIALIZATIONS ###########################
    better_than_chance <- 0
    attempt_counter <- 0
    chance_threshold <- -nrow(this_df) * log(1/3) - 1 
    ######################################################################
    # Try again if optimizer returns a result very close to chance as this typically means it
    # conked out at local minimum, but give up after 4 attempts 
    # res <- solnp(pars = params,
    #              fun = RunModel(params, this_phase=this_df, sim_opt, helpers),
    #              control = list(trace = 0),
    #              LB = unlist(lb_ub[["lb"]]),
    #              UB = unlist(lb_ub[["ub"]]))
    while (!better_than_chance & attempt_counter < 4) {
    res <- optim(par = params,
                 fn = function(params) { RunModel(params=params, this_phase=this_df, sim_opt, helpers) },
                 gr = NULL,
                 method = c("L-BFGS-B"),
                 lower = unlist(lb_ub[["lb"]]),
                 upper = unlist(lb_ub[["ub"]]),
                 hessian = FALSE,
                 control=list(trace=0)#,
                 #control=list(maxit=10000)
)
      better_than_chance <- ifelse (res$value < chance_threshold, 1, 0)
      attempt_counter <- attempt_counter + 1
    }
    cat("\n ## Optimization results: ## \n"); print(res$par)
  res    
  }
  ########################### INITIALIZATIONS ###########################
  # Generate random parameters..
  param_labels <- helpers[["param_labels"]]
  params <- GenRandParInits(helpers) # generate the numbers..
  params <- setNames(data.frame(t(params)), param_labels) #..and label them
  # Get lower and upper bounds for optimization
  lb_ub <- SetLBAndUB(helpers)
  ## Modify lower and upper bounds and parameter initializations to allow
  # repeats for pars fit separately in diff conditions..
  # Extract list of pars to split by cond from helpers..
  psc <- helpers[["pars_to_split_by_cond"]]
  if (!is_empty(psc)) {
    #.. tack onto the lower and upper bounds repeats of the cond split lb, ubs..
    # (note the conversion to numeric is in order to give optimizer an unnamed list)
    lb_ub[["lb"]] <- as.numeric(cbind(lb_ub[["lb"]], lb_ub[["lb"]] %>% select(psc)))
    lb_ub[["ub"]] <- as.numeric(cbind(lb_ub[["ub"]], lb_ub[["ub"]] %>% select(psc)))
    # .. then expand the parameter vec to add duplicate inits for pars fit separately by cond
    params <- SplitParsByCond(params, psc)
   } 
  # Set a default res in case of error
  res <- NA
  ######################################################################
  ############## RUN OPT WITH OPTION TO HANDLE ERRORS ##################
  if (helpers[["handle_errors_wo_breaking"]]) {
    res <- 
      tryCatch(error=function(cnd) NA,
        CallOpt(params, this_df, sim_opt, helpers, lb_ub),
        silent=TRUE) 
  } else {
    res <- CallOpt(params, this_df, sim_opt, helpers, lb_ub)
  }
  ######################################################################
res  
}