IterateThroughSubjs <- function(df, helpers, sim_opt) {
  ### Iterates through subjects and fits optimization then cleans up the results for #
  # storage in dataframe. Returns a dataframe with optimization results for each subject ###

  # Split df into list of IDs..
  #df <- df %>% filter(ID %in% c(5:7)) # For testing **
  spl_df <- split(df, df$ID)
  # .. run optimization subject-wise..
  full_res <- lapply(spl_df, function(x) {
    
      opt_res <- RunOpt(x, sim_opt, helpers)
      subj_res <- data.frame("ID"=as.numeric(unique(x$ID)),
                             t(opt_res$par), 
                             "nll"=opt_res$value, 
                             "conv_code"=opt_res$convergence, 
                             "exit_msg"=opt_res$message)
    }
  ) %>% bind_rows() # ..package up all subj results into single df
full_res  
}