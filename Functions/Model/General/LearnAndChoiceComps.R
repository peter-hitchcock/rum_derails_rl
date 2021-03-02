########################### ABOUT THESE FUNCTIONS ######################
### Various trial-wise learning and choice computations from RL and other 
# models that are called from RunOneTrial or CalculateAttnWeights ###
######################################################################
PerfSoftmax <- function(beta, # inverse temp
                        vec, # vector to apply softmax to 
                        pstate, # for printouts
                        this_trial, # for giving to browser for debugging
                        identifier='default' # optional ident for debugging on spec SM calls
) {
  ### Performs softmax using log sum exp trick. Returns softmax(vec) ###
  # Pull out the index of the max term, breaking ties by just taking first
  beta <- as.numeric(beta)
  mi <- which(vec == max(vec))[1]
  # Demonominator (constant)
  term2 <- beta * vec[mi] + # max term 
    log(sum(exp(beta * vec - beta * vec[mi])))
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(vec))
  # Numerators
  term1_vec <- beta * vec
  # Calc softmax for each elem 
  for (i in seq_along(vec)) sm_vec[i] <- term1_vec[i] - term2
  # break likelihood in case of over/underflow
  if (any(sm_vec > 0)) sm_vec <- rep(NA, 3)
  if (pstate) cat('\n Log liks', sm_vec)
sm_vec  
}
UpdateRL <- function(
  pstate,
  eta, 
  weights_RL, 
  schosen_vector_indices, 
  PE
) {
  ### Update weight vector via PE and learning rate ###
  weights_RL[schosen_vector_indices] <- 
          weights_RL[schosen_vector_indices] + as.numeric(eta * PE) 
  if (pstate) cat('\n RL weights after update', weights_RL)
weights_RL
}
UpdateRLNegPosLR <- function(
  pstate,
  eta_pos,
  eta_neg,
  weights_RL, 
  schosen_vector_indices, 
  PE
) {
  ### Update weight vector via PE and separate pos and neg learning rates ###
  if (PE < 0) {
    weights_RL[schosen_vector_indices] <- 
      weights_RL[schosen_vector_indices] + as.numeric(eta_neg * PE) 
  } else {
    weights_RL[schosen_vector_indices] <- 
      weights_RL[schosen_vector_indices] + as.numeric(eta_pos * PE) 
  }
  if (pstate) cat('\n RL weights after update', weights_RL)
weights_RL
}
DecayWeights <- function(pstate, d, weights_RL, schosen_vector_indices) {
  ### Decay weight vector via PE and learning rate ###
  weights_RL[setdiff(1:9, schosen_vector_indices)] <- 
    
    weights_RL[setdiff(1:9, schosen_vector_indices)] * as.numeric(1 - d)

  if (pstate) cat('\n RL weights after decay', weights_RL)
weights_RL
}
DimUpdateRL <- function(pstate, 
                        color_eta, shape_eta, texture_eta, 
                        weights_RL, 
                        schosen_vector_indices, 
                        param_labels, 
                        PE) { 
  ### Update weight vector via PE and learning rate but with distinct learning rates
  # for each dimension ###
  if (pstate) { 
    cat('\n color eta', color_eta, '* PE', PE, '=', as.numeric(color_eta * PE),
        '\n shape eta', shape_eta, '* PE', PE, '=', as.numeric(shape_eta * PE),
        '\n texture eta', texture_eta, '* PE', PE, '=', as.numeric(texture_eta * PE),
        '\n these respectively influence the 1:3, 4:6, and 7:9 RL weights')
  }
  # update color schosen weights using coloreta
  weights_RL[intersect(schosen_vector_indices, 1:3)] <- 
    
    weights_RL[intersect(schosen_vector_indices, 1:3)] + as.numeric(color_eta * PE) 
  
  # update shape schosen weights using shape eta
  weights_RL[intersect(schosen_vector_indices, 4:6)] <- 
    
    weights_RL[intersect(schosen_vector_indices, 4:6)] + as.numeric(shape_eta * PE)
  
  # update texture schosen weights using texture eta
  weights_RL[intersect(schosen_vector_indices, 7:9)] <- 
    
    weights_RL[intersect(schosen_vector_indices, 7:9)] + as.numeric(texture_eta * PE)
  
  if (pstate) cat('\n RL weights after update', weights_RL)
weights_RL
}
CalcDimPhiRLAttnWeights <- function(delta,
                                    color_weights,
                                    shape_weights,
                                    texture_weights,
                                    pstate,
                                    this_trial) {
  ### Returns a length 3 vector corresponding to CST dimensional weights for phi_RL (Daniel et al. 2020, JNeuro) ###
  # Get the dimension(s) with the highest weight
  max_csw <- c(max(color_weights)[1], max(shape_weights)[1], max(texture_weights)[1])
  # Find its index (1, 2, or 3 or some combo thereof in case of ties)
  max_inds <- which(max_csw==max(max_csw))
  phi_RL_weights <- rep(NA, 3)
  # If one dimension has the highest weight
  if (length(max_inds) == 1) {
    phi_RL_weights[max_inds] <- delta
    phi_RL_weights[setdiff(1:3, max_inds)] <- (1-delta)/2
    # If two
  } else if (length(max_inds) == 2) {
    phi_RL_weights[max_inds] <- delta/2
    phi_RL_weights[setdiff(1:3, max_inds)] <- (1-(delta/2))/2
    # Uniform weighting if there's no max
  } else {
    phi_RL_weights[1:3] <- 1
  }
phi_RL_weights    
} 
CalcMixedPhiFeatWeights <- function(kappa,
                                    bayes_attn_weights, # bayes probs (either of stims or features)
                                    RL_attn_weights, # RL attn weights (either for features or dimensions)  
                                    pstate) {
  ### Mixes Bayesian and RL based attention weights at the FEATURE level ###
  attn_weights <- kappa * bayes_attn_weights + (1-kappa) * RL_attn_weights
  if (pstate) {
    cat('\n Kappa', kappa, '* bayesian attn weights', bayes_attn_weights,
        '= \n', kappa * bayes_attn_weights,
        '\n 1 - kappa', 1-kappa, '* RL attention weights', RL_attn_weights,
        '= \n', (1-kappa) * RL_attn_weights,
        '\n Their correlation = ', cor(bayes_attn_weights, RL_attn_weights),
        '\n And phi weights total = ', attn_weights)
  }
attn_weights  
}
CalcMixedPhiDimWeights <- function(kappa,
                                    bayes_attn_weights, # bayes probs (either of stims or features)
                                    RL_attn_weights, # RL attn weights (either for features or dimensions)  
                                    pstate) {
  ### Mixes Bayesian and RL based attention weights at the DIMENSION level ###
  attn_weights <- kappa * bayes_attn_weights + (1-kappa) * RL_attn_weights
  if (pstate) {
    cat('\n Kappa', kappa, '* bayesian attn weights', bayes_attn_weights,
        '= \n', kappa * bayes_attn_weights,
        '\n 1 - kappa', 1-kappa, '* RL attention weights', RL_attn_weights,
        '= \n', (1-kappa) * RL_attn_weights,
        '\n Their correlation = ', cor(bayes_attn_weights, RL_attn_weights),
        '\n And phi weights total = ', attn_weights)
  }
attn_weights  
}
CalcBayesAttnWeights <- function(kappa,
                                   bayes_attn_weights, # bayes probs (either of stims or features)
                                   RL_attn_weights, # RL attn weights (either for features or dimensions)  
                                   pstate) {
  ### Just Bayesian attn weights at the feature level  ###
  unnormed_attn_weights <- kappa * bayes_attn_weights + (1-kappa) * 1/9
  attn_weights <- unnormed_attn_weights/sum(unnormed_attn_weights) * 9
  #cat("\n attn weights", attn_weights)
attn_weights  
}