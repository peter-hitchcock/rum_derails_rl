RunOneTrial <- function(
                        s1, s2, s3, 
                        color_RL_weights,
                        shape_RL_weights,
                        texture_RL_weights,
                        weights_RL,
                        sim_opt, 
                        param_labels, 
                        pars,
                        trial_row,
                        this_trial,
                        pstate,
                        trial,
                        persistent_vars=NULL,
                        # Allow passage of vars that we only have at this point for opt
                        opt_specific_vars=NULL
                        ) {
  ### Calls trial-wise learning and choice functions. Returns the trial-wise likelihood 
  # in case of opt or sim outs (choice, reward, lik) in case of sim ###
  ########################### SETUP ####################################
  outs <- list() # Where to store fx outputs  
  # Extract variables that we only have for opt at this point
  if (sim_opt == 2) {
    schosen <- opt_specific_vars[["schosen"]]
    schosen_index <- opt_specific_vars[["schosen_index"]]
    schosen_vector_indices <- opt_specific_vars[["schosen_vector_indices"]]
    reward <- opt_specific_vars[["reward"]]
  }
  #########################################################################
  #################### PUT ATTN BIASES ON STIMULUS VALUES.. ###############
  # .. if the model contains an attentional bias..
  if (
    "attn_beta" %in% param_labels |
    "delta" %in% param_labels |
    "kappa" %in% param_labels
  ) {
    if (trial == 1) {
      posterior_target <- rep(1/9, 9)
    } else { 
      posterior_target <- persistent_vars[["posterior_target"]]
    }
    # Calculates it.. 
    attn_biases <- CalcAttnWeights(pars,
                                   weights_RL,
                                   param_labels,
                                   reward,
                                   schosen_vector_indices,
                                   color_RL_weights,
                                   texture_RL_weights,
                                   shape_RL_weights,
                                   pstate,
                                   this_trial, 
                                   bayes_posterior_target=posterior_target)
    # .. and use it to bias value construction ..
    vs_vec <- ConstructStimValues(
                                  s1, s2, s3,
                                  color_RL_weights, 
                                  shape_RL_weights,
                                  texture_RL_weights,
                                  param_labels,
                                  attn_biases
                                 )
  } else {
    # .. otherwise construct an unbiased vs vec
    vs_vec <- ConstructStimValues(
                                  s1, s2, s3,
                                  color_RL_weights, 
                                  shape_RL_weights,
                                  texture_RL_weights,
                                  param_labels
                                  )
  }
  if (sim_opt == 2) {
    # Value of chosen stimulus 
    # .. don't yet have schosen in here 
    vchosen <- vs_vec[which(c(all(schosen == s1), all(schosen == s2), all(schosen == s3)))]
  }
  ########################### CALL LOG LIK FUNCTIONS ######################
    if (pstate)  cat('\n ######### CHOICE ############## \n') 
  # Calculate RL log liks #
  log_liks <- PerfSoftmax(beta=as.numeric(pars["beta"]), vec=vs_vec, pstate, this_trial)
  ## Add undirected noise 
  if ('eps' %in% param_labels) {
      for (stim in seq_along(log_liks)) log_liks[stim] <- 
          (1 - pars["eps"]) * log_liks[stim] + pars["eps"] * log(1/3)
  }
  ######################################################################
  # IF SIMULATING, CHOOSE, RECEIVE REWARD, AND LABEL AS CORRECT / NOT ##
  if (sim_opt == 1) { 
    # Extract vars we'll need for simulation #
    relevant_dimension <- trial_row$relevant_dimension
    target_feature <- trial_row$relevant_feature
    # Run sim this trial #
    outs <- 
      SimulateChoice(
        param_labels, 
        vs_vec,
        log_liks,
        s1, s2, s3,
        relevant_dimension, 
        target_feature,
        pstate,
        trial,
        schosen_index_vec
      )
    # Extract the simulated vars that we need for the rest of RunOneTrial 
    vchosen <- outs[["vchosen"]]
    reward <- outs[["reward"]]
    schosen_index <- outs[["schosen_index"]]
    schosen_vector_indices <- outs[["schosen_vector_indices"]]
  }
  #####################################################################
  ########################### CALCULATE PE ###########################
  if ('eta' %in% param_labels | 
      'color_eta' %in% param_labels | 
      'eta_pos' %in% param_labels) PE <- as.numeric(reward - vchosen)
  ## GET LOG LIKELIHOOD OF CHOSEN 
  # (NOW THAT WE HAVE SCHOSEN FOR ALL VARIANTS OF sim_opt) ##
  ll_chosen <- log_liks[schosen_index]
  ######################################################################
  if (pstate) cat('\n ######### UPDATES ############## \n') 
  ## UPDATE RL VIA LEARNING RATE AND PE ####
  if ('eta' %in% param_labels) {
    weights_RL <- UpdateRL(pstate,
                           pars["eta"], 
                           weights_RL, 
                           schosen_vector_indices, 
                           PE)
  }
  #############################################
  ############ UPDATE RL WITH SEPARATE POS/NEG LRS #####################
  if ("eta_pos" %in% param_labels) {
    weights_RL <- UpdateRLNegPosLR(
      pstate,
      pars["eta_pos"],
      pars["eta_neg"],
      weights_RL, 
      schosen_vector_indices, 
      PE
    )
  }
  ######################################################################
  #### UPDATE RL VIA DIMENSION-SPECIFIC LEARNING RATES + PE ####
  if ('color_eta' %in% param_labels) {
    weights_RL <- 
        DimUpdateRL(pstate, ptime, 
                    pars["color_eta"], pars["shape_eta"], pars["texture_eta"], 
                    weights_RL, schosen_vector_indices, param_labels, PE)     
  }
  ############################################
  #### DECAY WEIGHTS ####
  if ('d' %in% param_labels) weights_RL <- DecayWeights(pstate, pars["d"], weights_RL, schosen_vector_indices)
  ############################################
  #### CALCULATE BAYESIAN PROBABILITY THIS IS THE TARGET FEATURE ####
  # Note this has to come after choice so don't bias weights with information don't yet have #
  if ('kappa' %in% param_labels | "bayes_beta" %in% param_labels) {
    # Initalize priors to 1/9, otherwise yester-trial posterior becomes today-trial prior #
    if (this_trial == 1) {
      prior_ptarget <- rep(1/9, 9)
    } else {
      prior_ptarget <- persistent_vars[["posterior_target"]]
    }
    # Calculate updated posterior
    posterior_target <- CalcBayesProbTarget(prior_ptarget,
                                           reward,
                                           schosen_vector_indices,
                                           this_trial,
                                           pstate)
    persistent_vars <- list("posterior_target"=posterior_target)
  }
  ######################################################################
  if (sim_opt == 1) {
    outs[["weights_RL"]] <- weights_RL
    outs[["vs_vec"]] <- vs_vec
  }
  if (sim_opt == 2) {
    nll_this_trial <- -ll_chosen
    # Break in case of defective likelihood
    if (nll_this_trial < 0) nll <- NA
    outs[["nll_this_trial"]] <- nll_this_trial
    outs[["weights_RL"]] <- weights_RL
  }
  outs[["persistent_vars"]] <- persistent_vars
  ######################################################################
outs  
}