#### OUTER FUNCTION ####
RunParRec <- function(
                       emp_df,
                       #which_or_other,
                       model_number,
                       helpers, 
                       mm_df,
                       par_combos=200,
                       # keeping as an option for future but right now only seq implemented 
                       parallelize=0 
                      ) {
  
  ### Runs parameter recovery for a given model
  # Returns par_combos number of:
  # 1. parameters used to simulate data
  # 2. parameters recovered when optimizing with that simulated data ###
  browser()
  #### HELPER FUNCTIONS ####
  GeneratePRGrid <- function(par_df, par_labels, par_combos, mm_df) {
    ### Generates random parameter combinations in the range of empirical data (defaults to 200) #
    # Parameter combos are uniformly drawn from the min:max of empirical range) #
    # These will be used to simulate with then try to recover via optimization #
    values <- list()
    for (i in seq_along(par_labels)) {
      
      values[[par_labels[i]]] <- c(runif(par_combos, # no. values to pull 
                                         # use the min/max df from the empirical df to set the range of the data
                                         as.numeric(mm_df %>% filter(X == par_labels[i]) %>% select(c("min"))), 
                                         as.numeric(mm_df %>% filter(X == par_labels[i]) %>% select(c("max")))))
      
    }
    par_grid <- data.frame(values %>% bind_cols)
    
  par_grid    
  }
  RunSequentialPR <- function(par_grid,
                              emp_df,
                              rand_pars_df,
                              helpers,
                              lb_ub
  ) {
    ### Iterate sequentially through RunOneSimThenOpt ###
    
    # List to put in sim (true generative) and opt pars
    par_list <- list()
    # Run through the set of par combos
    for (i in 1:nrow(par_grid)) { 
      # Get one set of sample pars
      sample_pars <- par_grid[i, ] 
      # Get one set of random pars to start opt from 
      rand_pars <- rand_pars_df[i, ]
      cat('\n original params \n'); print(rand_pars)
      # Run a single sim and single opt
      one_set_sim_opt_res <- RunOneSimThenOpt(helpers, emp_df, sample_pars, rand_pars, lb_ub)
      cat('Full results \n:'); print(one_set_sim_opt_res)
      # .. and put the results into a list
      par_list[[i]] <- one_set_sim_opt_res
    }
    # List -> df
    par_rec_res <- par_list %>% bind_rows()
    
  par_rec_res  
  }
  RunOneSimThenOpt <- function(helpers, 
                               emp_df,
                               sample_pars, # pars to use for simulation
                               rand_pars, # pars to initialize at for optimization
                               lb_ub # lower and upper bounds for optimization
  ) {
    ### For a single set of sample pars, simulate with then try to recover the pars #
    ## Simulate 
    # Randomly pick a real single-phase trajectory through the task
    sample_ID <- sample(helpers[[1]], 1)
    #sample_cond <- sample(unique(emp_df$condition), 1)
    #sample_phase <- emp_df[emp_df$ID == sample_ID & emp_df$condition == sample_cond, ]
    sample_phase <- emp_df[emp_df$ID == sample_ID, ]
    # Extract just the task parameters (ie ignore anything pt specific)
    sim_df <- sample_phase[, c("game", "trial",
                               "s1_C", "s1_S", "s1_T",
                               "s2_C", "s2_S", "s2_T", 
                               "s3_C", "s3_S", "s3_T", 
                               "relevant_dimension", 
                               "relevant_feature")]
    # Simulate using the pars and task parameters
    sim_outs <- RunModel(params=sample_pars, this_phase=sim_df, sim_opt=1, helpers)
    # Complete the sim df by adding the simulated information
    sim_df$reinforcement <- sim_outs[["reward_vec"]]
    sim_df$correct <- sim_outs[["correct_vec"]]
    sci <- sim_outs[[4]] # schosen index (sci) at the stimulus level (eg 2 = stimulus 2)
    # Use sci to add the simulated chosen stimulus to the sim df in the same 
    # format as in the empirical data
    for (j in 1:nrow(sim_df)) {
      if (sci[j] == 1) {
        # Extract stim features
        sfs <- as.numeric(sim_df[j, c("s1_C", "s1_S", "s1_T"), ])
        # Put in df
        sim_df[j, c("chstim_color", "chstim_shape", "chstim_texture")] <- sfs
      } else if (sci[j] == 2) {
        sfs <- as.numeric(sim_df[j, c("s2_C", "s2_S", "s2_T"), ])
        sim_df[j, c("chstim_color", "chstim_shape", "chstim_texture")] <- sfs
      } else if (sci[j] == 3) {
        sfs <- as.numeric(sim_df[j, c("s3_C", "s3_S", "s3_T"), ])
        sim_df[j, c("chstim_color", "chstim_shape", "chstim_texture")] <- sfs
      }
      # Error check
      row <- sim_df[j, c("chstim_color", "chstim_shape", "chstim_texture")]
      if (any(is.na(row))) stop("Error following simulation: Couldn't correctly
                                 put chosen stim into sim_df.")
    }
    ## Optimize 
    convergence <- 1
    attempt_counter <- 1
    # Keep trying until convergence is 0 (indicating successful completion),
    # but give up after 5 attempts
    while (convergence > 0 & attempt_counter < 6) {
      sim_opt_res <- optim(par = rand_pars,
                           fn = function(rand_pars) { RunModel(params=rand_pars,
                                                               this_phase=sim_df,
                                                               sim_opt=2,
                                                               helpers) }, 
                           gr=NULL,
                           method=c('L-BFGS-B'),
                           lower=unlist(lb_ub[[1]]),
                           upper=unlist(lb_ub[[2]]),
                           hessian=TRUE,
                           control=list(maxit=10000))
      convergence <- sim_opt_res$convergence
      # Try again if optimization peters out at chance performance (which sometimes happens due
      # to local minima)
      #if(sim_opt_res$value > -nrow(sim_df) * log(1/3) - 1) convergence <- 1
      if(sim_opt_res$value > 1448) convergence <- 1
      attempt_counter <- attempt_counter+1
      # If 5 attempts at convergence without success, just store NA as the opt value
      #if (attempt_counter > 5) sim_opt_res$par <- NA
    }
    ## Store
    # Extract opt pars out into dataframe
    opt_par_names <- paste(names(sim_opt_res$par), "opt", sep="_")
    opt_pars <- data.frame(t(sim_opt_res$par))
    # Identify the simulated and optimization result pars as such..
    names(sample_pars) <- paste(names(sample_pars), "sim", sep="_")
    names(opt_pars) <- opt_par_names
    #names(opt_pars) <- paste(names(data.frame(t(opt_pars))), "opt", sep="_")
    # .. and put them in a 1-row dataframe to pass out
    sim_and_opt_pars <- data.frame(cbind(sample_pars, opt_pars), "value"=sim_opt_res$value)
  sim_and_opt_pars    
  }
  ############################################ END HELPER FUNCTIONS
  # Model family is specified by which_or_other and model is specified by model number ###
  res_parlabels_mlabel <- SetUpModelSpecStructsAndLabels(helpers[["which_model"]], helpers)
  #### INITIALIZATIONS ####
  # Setup model-specific stuff we need for RunModel
  param_labels <- res_parlabels_mlabel[["param_labels"]] 
  helpers[["param_labels"]] <- param_labels 
  print(param_labels)
  # Generate params in the range of the empirical data
  par_grid <- GeneratePRGrid(par_df, param_labels, par_combos, mm_df)
  # Generate random parameter initializations for optimization of size proportional to 
  # no. of par combos to run (/100 because that's the number of phases in the study, hence
  # the no. of rows returnd by GenRandParInits) #
  rand_pars_df <- lapply(1:nrow(par_grid), function(x) {
    rps <- setNames(as.data.frame(t(GenRandParInits(helpers))), param_labels)
  rps    
  }) %>% bind_rows()
  # for (rp in 1:ceiling(nrow(par_grid)/100)) rp_list[[rp]] <- GenRandParInits(helpers) 
  # rand_pars_df <- rp_list %>% bind_rows()
  # Set lower and upper bounds
  lb_ub <- SetLBAndUB(helpers)
  ############################################
  # Continue sequentially or in parallel # Edit: parallel never ended up being worth the trouble so didn't implement
  if (!parallelize) par_recov_res <- RunSequentialPR(par_grid,
                                                     emp_df,
                                                     rand_pars_df,
                                                     helpers,
                                                     lb_ub)
par_recov_res
}