RunModel <- function(params, this_phase, sim_opt, helpers, pr=0, pstate=0, p_final_nll=1, ptime=0) {
  ### Returns nll for optimization or simulated data for simulation #
  # Does this by looping through trials of this phase and running trial-level
  # model computations #
  ########################### INITIALIZATIONS ###########################
  param_labels <- helpers[["param_labels"]]
  ## General inits ####
  nll <- 0
  nll_vec <- rep(NA, nrow(this_phase)) # Store all the nll's for debugging 
  RL_weight_init <- 0
  # Iniitalize list of trial-wise vars we want to persist. Will get appended in RunOneTrial 
  persistent_vars <- list() 
  # Pstate is for general printing, pfinal_nll is for printing the negative log lik
  pstate <- helpers[["pstate"]]
  p_final_nll <- helpers[["p_final_nll"]]
  ## Preallocate these vectors and lists ##
  lik_vec <- rep(NA, nrow(this_phase))
  correct_vec <- rep(NA, nrow(this_phase))
  reward_vec <- rep(NA, nrow(this_phase))
  schosen_index_vec <- rep(NA, nrow(this_phase))
  vs_vec_list <- list()
  # vs_vec_ # Record 
  ## To allow fitting some pars on all data and others by condition, find the 
  # point where condition changes (so there can start optimizing with new params)..
  if (nrow(this_phase) > 660) {
    cond_change_point <- which(diff(as.numeric(this_phase$condition)) == 1) + 1
  } else {
    # Sometimes pass in smaller df for testing so this allows passage wo error 
    cond_change_point <- 0
  }
  # .. and the col indices of parameters to fit seprately on rum and neu
  # (see parameter splitting above simulation call for details)
  rum_par_cols <- grep("_RUM", names(params))
  neu_par_cols <- grep("_NEU", names(params))
  ######################################################################
  ########################### VECTORIZATION #######################
  ## For speed and easier handling ### 
  s1_C <- this_phase$s1_C; s1_S <- this_phase$s1_S; s1_T <- this_phase$s1_T
  s2_C <- this_phase$s2_C; s2_S <- this_phase$s2_S; s2_T <- this_phase$s2_T
  s3_C <- this_phase$s3_C; s3_S <- this_phase$s3_S; s3_T <- this_phase$s3_T
  reinf_vec <- this_phase$reinforcement
  sc_color <- this_phase$chstim_color
  sc_shape <- this_phase$chstim_shape
  sc_texture <- this_phase$chstim_texture
  trial_vec <- this_phase$trial
  condition <- this_phase$condition
  ######################################################################
  ########################### LOOP THROUGH TRIALS #######################
  n_trials <- nrow(this_phase)
  #for (trial in 1:5) { # For testing
  for (trial in 1:n_trials) {
    ############### SET UP VARIABLES FOR THIS TRIAL ######################
    trial_row <- this_phase[trial, ] # Pull trial row 
    this_trial <- trial_vec[trial] # Extract the actual trial
    ## Reset weights and Bayes probs on trial 1 ###
    if (this_trial == 1) {
      weights_RL <- rep(RL_weight_init, 9)
    }
    if (pstate)  cat('\n ######### TRIAL', this_trial, '###########',
                     '\n  RL weights before update', weights_RL)
    ## Construct / assign trial-wise vars ##
    # Stimulus 1:3
    s1 <- c(s1_C[trial], s1_S[trial], s1_T[trial]) 
    s2 <- c(s2_C[trial], s2_S[trial], s2_T[trial]) 
    s3 <- c(s3_C[trial], s3_S[trial], s3_T[trial]) 
    # Extract RL weights by dimension 
    color_RL_weights <- weights_RL[1:3]
    shape_RL_weights <- weights_RL[4:6]
    texture_RL_weights <- weights_RL[7:9]
    # Pull the reward and schosen and information about these for opt (don't have this yet for sim)
    if (sim_opt == 2) { 
      reward <- reinf_vec[trial]  
      schosen <- c(sc_color[trial], sc_shape[trial], sc_texture[trial])
      schosen_index <- which(c(all(schosen == s1), all(schosen == s2), all(schosen == s3)))
      # Variable used for easier indexing
      schosen_vector_indices <- c(schosen[1], (schosen[2] + 3), (schosen[3] + 6)) 
    }
    if (pstate) {
      cat('\n ## Stim Indices At Vector Level: ## 
             \n S1', c(s1[1], s1[2]+3, s1[3] + 6), 
            '\n S2', c(s2[1], s2[2]+3, s2[3] + 6), 
            '\n S3', c(s3[1], s3[2]+3, s3[3] + 6))
    }
    ######################################################################
    ############# SELECT THE PARAMS FOR THIS COND ONLY ##################
    # If all params are same in each condition, just alias params ..
    if (is_empty(rum_par_cols)) {
      these_pars <- params
    } else {
      # .. whereas if at least 1 parameter is fit separately by condition then,
      # before passing parameters to run trial-wise comps, excise from the par df
      # the pars that should only be used in the other condition..
      if (trial == 1) {
        these_pars <- params[-c(neu_par_cols)]
        # .. and rename with generic names..
        these_pars <- setNames(these_pars, sub("_RUM", "", names(these_pars)))
      } 
      if (trial == cond_change_point) {
        # .. now repeat with the other set of pars. Thus params is never modified in RunModel
        # and this enables only the cond-specific vars to be passed on at the correct time 
        # to affect the likelihood in RunOneTrial
        these_pars <- params[-c(rum_par_cols)]
        these_pars <- setNames(these_pars, sub("_NEU", "", names(these_pars)))
      }
    }
    ######################################################################
    ######### RUN A SINGLE TRIAL AND HANDLE DIFFERENT OUTPUTS FOR SIM / OPT #########
    # Record trial-wise sim variables #
    if (sim_opt == 1) {
      outs <- RunOneTrial(
                          s1, s2, s3, 
                          color_RL_weights,
                          shape_RL_weights,
                          texture_RL_weights,
                          weights_RL,
                          sim_opt, 
                          param_labels, 
                          # These pars are just the pars for this condition 
                          pars=these_pars, 
                          trial_row,
                          this_trial,
                          pstate,
                          trial, # overall trial
                          persistent_vars
      )
      reward <- outs[["reward"]]
      reward_vec[trial] <- reward
      schosen <- outs[["schosen"]]
      #schosen_vec[trial] <- schosen
      correct <- outs[["correct"]]
      correct_vec[trial] <- correct
      schosen_index <- outs[["schosen_index"]]
      schosen_index_vec[trial] <- outs[["schosen_index"]]
      weights_RL <- outs[["weights_RL"]]
      vs_vec_list[[trial]] <- outs[["vs_vec"]]
    }
    if (sim_opt == 2) {
      opt_specific_vars <- list()
      opt_specific_vars[["schosen"]] <- schosen
      opt_specific_vars[["schosen_index"]] <- schosen_index
      opt_specific_vars[["schosen_vector_indices"]] <- schosen_vector_indices
      opt_specific_vars[["reward"]] <- reward
      outs <- RunOneTrial(
                          s1, s2, s3, 
                          color_RL_weights,
                          shape_RL_weights,
                          texture_RL_weights,
                          weights_RL,
                          sim_opt, 
                          param_labels, 
                          pars=these_pars,
                          trial_row,
                          this_trial,
                          pstate,
                          trial, # overall trial
                          persistent_vars,
                          opt_specific_vars
      )
      nll_this_trial <- outs[["nll_this_trial"]]
      weights_RL <- outs[["weights_RL"]]
      ########################### SUM NLL FOR OPT  ###########################
      nll_vec[trial] <- nll_this_trial # Stored for debugging only
      nll <- sum(nll, nll_this_trial) # What optimizer gets
      if (trial == n_trials & p_final_nll) {
        cat('\n summed nll final trial:      ', nll) # sum_nll
        #cat("\n ## Params from RunModel \n"); print(params)
      } 
      ######################################################################  
    }
    ## Unload the trial-wise persistent vars that will go into a RunOneTrial argument 
    # next round 
    persistent_vars <- outs[["persistent_vars"]] 
  } ## END LOOP THROUGH TRIALS  
  ########################### PASS OUT RETURN VALUES #####################
  if (sim_opt == 1) vs_vecs_out <- setNames(do.call(rbind.data.frame, vs_vec_list), c("vs1", "vs2", "vs3"))
  if (sim_opt == 1) output <- list("reward_vec"=reward_vec, 
                                   "correct_vec"=correct_vec, 
                                   "lik_vec"=lik_vec,
                                   "schosen_index_vec"=schosen_index_vec,
                                   "vs_vecs"=vs_vecs_out) 

  if (sim_opt == 2) output <- nll
  ######################################################################
output
}