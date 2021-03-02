RunSim <- function(helpers, which_model, par_res, df, identifier="emp") {
  ### Runs simulation by splitting data on condition, ID, and identifier, returning a generically 
  # labeled simulation ###
  
  if (identifier=="emp") df$identifier <- identifier
  
  model_settings <- SetUpModelSpecStructsAndLabels(which_model)
  helpers[["param_labels"]] <- model_settings[["param_labels"]]
  helpers[["model_label"]] <- model_settings[["model_label"]]
  
  # for sim / pchosen, apply model over ID, condition, and param value (factor labeled with identifier) subsets 
  model_df <- df %>% 
    # Subset by condition, ID (together constituting 1 phase), 
    # and identifier (which param value to run, w diff idents diff values of the varied param)
    split(list(.$condition, .$ID, .$identifier)) %>% 
    # run model with that subset
    map(~ RunModel(params = unique(.[param_labels]), this_phase = ., sim_opt_pchosen, helpers)) %>% 
    map(function(x) { 
      x %>% bind_cols() 
      out <- as.data.frame(x) 
      colnames(out) <- paste0('var', 1:ncol(out))
      out
    }) %>% 
    bind_rows() 
  
  #**to do: add labeling ##
  
}