CalcBayesProbTarget <- function(prior_ptarget,
                                reward,
                                schosen_vector_indices,
                                this_trial,
                                pstate # Print results while running?
) {
  ### Calculates Bayesian probability a given feature is the target via Bayes rule. Returns
  # the posterior over all features after the update ###
  # Prior ptarget this trial is posterior_target from the last trial (initialized at 1 on trial 1)
  if (pstate) {
    cat('\n CALC BAYES PROB: \n Reward is:', reward,
        '\n Indices of chosen stim are:', schosen_vector_indices,
        '\n Prior is', prior_ptarget)
  }
  likelihood_vec <- rep(NA, 9)
  if (reward) {
    # Set likelihood to .75 for the chosen features, .25 for other features 
    likelihood_vec[schosen_vector_indices] <- .75
    likelihood_vec[setdiff(1:9, schosen_vector_indices)] <- .25
  } else {
    likelihood_vec[schosen_vector_indices] <- .25
    likelihood_vec[setdiff(1:9, schosen_vector_indices)] <- .75
  }
  unnormed_post_target <- likelihood_vec * prior_ptarget
  posterior_target <- unnormed_post_target / sum(unnormed_post_target)
  if (pstate) {
    cat('\n Likelihood vec is: \n', likelihood_vec,
        '\n Unnormalized posterior of target is: \n', unnormed_post_target,
        '\n and normalized posterior is: \n', posterior_target,
        '\n Sum of normalized posterior =', sum(posterior_target))
  }
posterior_target  
}