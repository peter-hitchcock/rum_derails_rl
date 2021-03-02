SimulateChoice <- function(     
  param_labels, 
  vs_vec,
  log_liks,
  s1, s2, s3,
  relevant_dimension, 
  target_feature,
  pstate,
  trial,
  schosen_index_vec
) {
  ### Returns simulated choice and given this: reward, value of chosen stim (vchosen), 
  #  and whether the choice was correct #
  # Steps: 
  # 1. Convert log liks to probabilities ### 
  # 3. Assign schosen via random number selection corresponding to a section of that interval
  # 4. Find vchosen and index of chosen
  # 5. Determine if schosen contains the target feature and probabilistically assign reward if so 
  # 5. Pass out reward, schosen, and a vector tracking if the choice was correct / not ###
  ########################### HELPER FUNCTIONS #########################
  FindSchosen <- function(s1, s2, s3,
                          stim1_probability_bound,
                          stim2_probability_bound) {
    
    ### Assign schosen ###
    if (rand_prob_for_stim <= stim1_probability_bound) {
      schosen <- as.numeric(s1)
      if (pstate)  cat('\n agent chooses stim1:', c(s1[1], s1[2]+3, s1[3]+6)) 
    }
    if (rand_prob_for_stim > stim1_probability_bound & rand_prob_for_stim <= stim2_probability_bound) { 
      schosen <- as.numeric(s2) 
      if (pstate)  cat('\n agent chooses stim2:', c(s2[1], s2[2]+3, s2[3]+6)) 
    }
    if (rand_prob_for_stim > stim2_probability_bound) { schosen <- as.numeric(s3)
    if (pstate) cat('\n agent chooses stim3:', c(s3[1], s3[2]+3, s3[3]+6)) 
    }
    
  schosen  
  }
  ######################################################################
  # Convert log liks to probabilities  
  for (this_prob in 1:3) assign(paste0('p', this_prob), exp(log_liks[this_prob]))
  if (pstate) cat('\n p(stim1)', p1, 
                  '\n p(stim2)', p2, 
                  '\n p(stim3)', p3, 
                  '\n probs sum:', p1 + p2 + p3)
  ## Choose a stimulus in proportion to its probability by generating a random number and 
  # picking schosen proportional to the prob mass intervals of s1:s3 in 0, 1 #
  stim1_probability_bound <- p1
  stim2_probability_bound <- p1 + p2 
  rand_prob_for_stim <- runif(1, 0, 1)
  if (pstate) cat('\n stim1 probability bound 
                  (should be same as normed p1):', stim1_probability_bound, 
                  '\n stim 2 probability bound 
                  (should be same as normed p1 + p2):', stim2_probability_bound, 
                  '\n random probability:', rand_prob_for_stim)
  schosen <- FindSchosen(s1, s2, s3,
                         stim1_probability_bound,
                         stim2_probability_bound)
  # assign Vchosen and get schosen index
  vchosen <- vs_vec[which(c(all(schosen == s1), all(schosen == s2), all(schosen == s3)))]
  schosen_index <- which(c(all(schosen == s1), all(schosen == s2), all(schosen == s3)))
  #schosen_vector_indices <- c(schosen[1], schosen[2]+3, 
  #schosen_index_vec[trial] <- schosen_index
  if (pstate) cat('\n vchosen', vchosen) 
  # Assign a reward probability and print some information
  rand_prob_for_rew <- runif(1, 0, 1)
  relevant_feature <- schosen[relevant_dimension] # find the relevant feature (the element of schosen from the relevant dimension)
  if (pstate) cat('\n Random probability for reward assignment 
                    (25% / 75% are the cutoffs for rew assignment if the choice is 
                    NOT / IS correct) \n', 
                    rand_prob_for_rew, 
                    '\n the relevant dimension in the chosen stim is  ', 
                    relevant_dimension, 
                    '\n and the relevant feature in that dimension, in the chosen stim, is  ', 
                    relevant_feature, 
                    '\n the target feature is ', 
                    target_feature)
  # Determine if the relevant feature matches the target
  # and if it does, assign reward w p(.75) else p(.25)
  # If the relevant feature in the simulated schosen matches the truly relev feature 
  # (here called target feature)
  if (relevant_feature == target_feature) { 
    if (pstate) cat('\n target and relevant feature match') 
    correct <- 1
    reward <- ifelse(rand_prob_for_rew <= .75, 1, 0)
  # If the relevant feature in schosen doesn't match the target feature   
  } else { 
    if (pstate)  cat('\n target and relevant feature dont match')
    correct <- 0
    reward <- ifelse(rand_prob_for_rew <= .25, 1, 0)
  }
  if (pstate)  cat('\n reward:', reward, 
                   '\n correct assignment = ', correct)
  schosen_vector_indices <- c(schosen[1], (schosen[2] + 3), (schosen[3] + 6))
  if (pstate) cat('\n schosen vec indices', schosen_vector_indices) # ** add this
  
list(
  "reward"=reward, 
  "schosen"=schosen, 
  "correct"=correct, 
  "vchosen"=vchosen, 
  "schosen_index"=schosen_index,
  "schosen_vector_indices"=schosen_vector_indices#, 
  #"reward_vec"=reward_vec, 
  #"schosen_index_vec"=schosen_index_vec
) 
}