SetUpAndSaveOptRun <- function(df,
                               sim_opt,
                               helpers,
                               which_model) {
  ### Sets up for an optimization run, runs i iterations of optimization,
  # packages up the results and saves both all opts and best fit opts ###
  
  # Notes: Returns the results to s.R but more typically invoked just for side effect of saving #
  # out results #
  
  ### Take just the lowest NLL for this subj ###
  GetBestFit <- function(x) data.frame(x %>% group_by(ID) %>% slice(which.min(nll)))
  # Dynamically set up for the optimization run for this model. (This enables # 
  # multiple models to be passed in during sequential pass through mutliple models #
  model_settings <- SetUpModelSpecStructsAndLabels(which_model, helpers)
  model_label <- model_settings[["model_label"]]
  param_labels <- model_settings[["param_labels"]] 
  helpers[["param_labels"]] <- param_labels
  # Pass to iter through opt 
  iters <- 1:helpers[["iterations"]]
  all_iters_df <- lapply(iters, function(x) {
    # Run one iter of optimization all subjects in the df..
    subj_res <- IterateThroughSubjs(df, helpers, sim_opt)
    res_w_iter_label <- data.frame(subj_res, "iter"=x)
  res_w_iter_label   
  }) %>% bind_rows() # .. and bind up 
  # Create df with just the best fit 
  best_fits <- GetBestFit(all_iters_df)
  ## Save results 
  all_iters_path <- opt_paths[["all_iters"]]
  best_fit_path <- opt_paths[["best_fit_path"]]
  write.csv(best_fits, paste0(best_fit_path, model_label, ".csv"), row.names=FALSE)
  write.csv(all_iters_df, paste0(all_iters_path, model_label, ".csv"), row.names=FALSE)
  
# Return both best fits and all iters in a labeled list  
list("best_fits"=best_fits, "all_iters"=all_iters_df)
}