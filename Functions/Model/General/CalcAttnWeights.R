CalcAttnWeights <- function(pars,
                            weights_RL,
                            param_labels,
                            reward,
                            schosen_vector_indices,
                            color_RL_weights,
                            texture_RL_weights,
                            shape_RL_weights,
                            pstate,
                            this_trial,
                            bayes_posterior_target=NULL) {
  ### Calculates various kinds of attention weights based on feature weight magnitudes or stimulus values by 
  # calling fxs in LearningandChoiceComps ###
  # CalcAttnWeights is alled from RunOneTrial #
  ## Feature attention weights based on a softmax of feature weights (Jaskir et al 17) #
  if ('attn_beta' %in% param_labels) {
      attn_weights <- exp(PerfSoftmax(pars["attn_beta"],
                                      weights_RL,
                                      pstate,
                                      this_trial,
                                      identifier="attn"))
      if (pstate) (cat('\n Attention weights \n:', attn_weights))
  }
  ######################################################################
  # Phi RL for dimension-based attn weighting based on the RL-based weights (Daniel et al 20) #
  if ('delta' %in% param_labels) {
    attn_weights <- CalcDimPhiRLAttnWeights(pars["delta"],
                                            color_RL_weights,
                                            shape_RL_weights,
                                            texture_RL_weights,
                                            pstate,
                                            this_trial)
  } 
  ######################################################################
  ################## Phi RL for RL/Bayes attn weight mixtures #########
  # Mix the just-calculated DIMENSIONAL RL-based weights (via delta) #
  # via kappa with weights based on the Bayesian posterior of the target #
  # (Daniel et al 20; kappa here=alpha in that paper) .. #
  if ('kappa' %in% param_labels & 'delta' %in% param_labels) {
    # The posterior prob by target vector is arranged as CST so for the Bayesian dimension
    # weights just need the sum  
    bayes_dim_weights <- 
      c(sum(bayes_posterior_target[1:3]), 
        sum(bayes_posterior_target[4:6]), 
        sum(bayes_posterior_target[7:9])
        )
      attn_weights <- CalcMixedPhiDimWeights(pars["kappa"], # ** doesn't yet exist
                                             bayes_dim_weights, # bayes probs (either of stims or features)
                                             RL_attn_weights=attn_weights, # RL attn weights (either for features or dimensions)
                                             pstate)  
  } 
  # .. or mix the just-calculated FEATURE RL-based weights (via attn beta) #
  # via kappa with weights based on the Bayesian posterior of the target #
  # (Daniel et al 20; kappa here=alpha in that paper) .. #
  if ('kappa' %in% param_labels & 'attn_beta' %in% param_labels) {
      attn_weights <- CalcMixedPhiFeatWeights(pars["kappa"],
                                              bayes_attn_weights=bayes_posterior_target, # bayes probs (either of stims or features)
                                              RL_attn_weights=attn_weights, # RL attn weights (either for features or dimensions)  
                                              pstate)     
  }
  if ('kappa' %in% param_labels & (!'attn_beta' %in% param_labels) & (!'delta' %in% param_labels)) {
    ## Trying out kappa just in the early trials. This is based on the insight that it can take on a unique
    # fx in the eary trials by guiding attn away from losses, in contrast to the RL model that's initialized at 
    # 0 and so never experiences negative prediction errors until it's actually had a reward
    # (conditional on 0 initializations, so this suggests initializing at a non-zero
    # value might allow this via the RL-based system). More generally the Bayesian early can stand in for early 
    # MB learning whose marginal utility I assume will decrease as the RL-based components become more reliable
    if (this_trial < 5) {
      attn_weights <- CalcBayesAttnWeights(pars["kappa"],
                                           bayes_attn_weights=bayes_posterior_target,
                                           pstate)
    } else {
     attn_weights <- rep(1, 9)
    }
  }
attn_weights 
}