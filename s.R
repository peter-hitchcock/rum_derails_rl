which_model <- 12
pars_to_split_by_cond <- c("d") # Note this will break if it's not one of the pars in model
sim_opt <- 2
iterations <- 1
param_recov <- 1
model <- "RL_9" # Text string only used for sim or param_recov
rm_excluded_pts <- 1
if (param_recov) sim_opt <- 0 
sapply(c("dplyr", "truncnorm", "rlang"), require, character.only=TRUE)
####################### ABOUT THIS SCRIPT ###########################
# -- Minimal script to run computationally intensive stuff 
# (opt, sim, or param recov) locally or on cluster -- #
# About the above initializations:
# which_model: IDs the model by number. This can be a vector of more than one models for optimization, 
# in which case it will run optimizations for these models in sequence 
# pars_to_split_by_cond is a vector of parameters for which to derive separate parameters by cond; names
# must match exactly
# use cluster run > 0 to run on cluster  
# model: Character vec IDing the same model needed for param_recov/sim to pull 
# the file with empirical fit results (sim) or find the file with min/max range (par recov)
# sim_opt: whether to run simulation or optimization (note this can be left non-zero for par recov
# because of the hack on line 8 setting it automatically to 0 in case of param recov)
# however, you do need to supply a path to a csv for the min and max in the param recov section below (line 132)
## rm_excluded pts allows pulling either the dataset excluding pts performing at ~ chance 
# (see CleanData.Rmd for details) or that includes all pts 
######################################################################
########################### SOURCE FUNCTIONS #########################
sf <- function() {
  ### Handle to quickly source all functions ###
  fps <- c( 
    list.files("./Functions/Model/Optimize", full.names=TRUE),
    list.files("./Functions/Model/Simulate", full.names=TRUE),
    list.files("./Functions/Model/Recover_Params", full.names=TRUE),
    list.files("./Functions/Analyze", full.names=TRUE),
    list.files("./Functions/Model/General/", full.names=TRUE)
  )
  sapply(fps, source)
}
sf() 
######################################################################
########################### DEFINE SAVE PATHS ###########################
# For optimization..
opt_paths <- list()
opt_paths[["all_iters"]] <- "./../../model_res/opt_res/all_iters/batch1/"
opt_paths[["best_fit_path"]] <- "./../../model_res/opt_res/best_fits/batch1/"
# ..  par recov 
pr_path <- './../../model_res/par_rec_res/'
pr_mm_path <- './../../data/min_max_for_pr/' 
######################################################################
########################### LOAD DATA ################################
if (rm_excluded_pts) {
  df <- read.csv("./../../data/bx_df_w_exclusions_removed.csv")  
} else {
  df <- read.csv("./../../data/bx_df_full-with_no_performance_removals.csv")  
} 
######################################################################
########################### OTHER SPECS ##############################
full_par_list <-  c(
  'beta', # inverse temp on weights 
  'eta', # RL learning rate
  'd', # RL decay 
  'attn_beta', # softmax weight on RL weights, leading in the limit to winner-take-all dynamics
  'color_eta', # learning rate specific to color dimension
  'shape_eta', # learning rate specific to shape dimension
  'texture_eta', # learning rate specific to texture dimension
  'kappa', # mixture on statistically optimal attn weight
  'bayes_beta', # softmax if directly mapping bayes probs to action probs  ** delete
  'eps', # undirected noise
  'delta', # determines size of phi_RL dimensional attention weight  
  'eta_pos', # positive RL learning rate 
  'eta_neg' # negative RL learning rate 
) 
helpers <- list(
  "IDs"=unique(df$ID),
  "conditions"=unique(df$condition), 
  "iterations"=iterations,
  "which_model"=which_model,
  "full_par_list"=full_par_list,
  "param_labels"=c(), # preallocate, will get filled when setting up model  
  "sim_opt"=sim_opt,
  "pars_to_split_by_cond"=pars_to_split_by_cond,
  "opt_file_paths"=opt_paths,
  "pstate"=0, # print out to console?
  "p_final_nll"=1,
  "handle_errors_wo_breaking"=0 # silently record NA in case case of error 
)
######################################################################
########################### RUN SIMULATION ###########################
# This variant of simulation requires you first load the results dataset; here's an example. This
# is how I ended up running all sims reported in paper.
if (sim_opt == 1) {
  r1 <- read.csv(paste0(opt_paths[["best_fit_path"]], "m_12_beta_d_eta_pos_eta_neg__6507_split_on_eta_pos.csv"))
  r2 <- read.csv(paste0(opt_paths[["best_fit_path"]], "m_12_beta_d_eta_pos_eta_neg__9611_split_on_eta_pos.csv"))
  r3 <- read.csv(paste0(opt_paths[["best_fit_path"]], "m_12_beta_d_eta_pos_eta_neg__6106_split_on_eta_pos.csv"))
  r4 <- read.csv(paste0(opt_paths[["best_fit_path"]], "m_12_beta_d_eta_pos_eta_neg__5304_split_on_eta_pos.csv"))
  m12_diff_etas_split_pos <- GetBestFit(rbind(r1, r2, r3, r4))
  all_iters <- lapply(1:10, function(x) {
    sim_outs <- list() 
      for (subj in unique(df$ID)) {
        helpers[["param_labels"]] <- c("beta", "d", "eta_pos", "eta_neg")
        emp_pars <- m12_diff_etas_split_pos %>% filter(ID==subj) %>% select(beta, d, eta_pos_RUM, eta_pos_NEU, eta_neg)
        pt_df <- df %>% filter(ID==subj)
        sim_outs[[subj]] <- RunOneSim(helpers, emp_pars, pt_df)
      } 
      df_with_sim <- sim_outs %>% bind_rows
    df_with_sim
    }) %>% bind_rows()
}
# summs <- all_iters %>% group_by(condition, trial) %>% summarize(pcor_sim=mean(sim_correct),
#                                                                 pcor_emp=mean(correct))
#write.csv(all_iters, "./../../model_res/sim_outs/10iter_sim_res_m12_splitonpos.csv")
#########################################
## RUN OPTIMIZATION ####
# The structure of optimization functions is SetUpAndSave.. (iterate through iterations) -> 
# IterateThroughSubjects --> RunOpt (run a single opt) --> 
# RunModel (the function calculating summed logged lik in case of opt, which also includes simulation)
if (sim_opt == 2) {
  # Loop through 1+ models, returning a list of models and also saving out results
  res_par_lists <- list() # This will store for each model all iter and best fit results
  for (m in 1:length(which_model)) {
    res_par_lists[[m]] <- 
      SetUpAndSaveOptRun(df, sim_opt, helpers, which_model=which_model[m])
  }
}
################################## 
#### RUN PAR RECOV ####
if (param_recov) {
  # Number of parameter combos to try out is an optional arg par_combos. Defaults to 200 #
  pr_res <- list()
  # Pull min and max so can run param recov in range of empirical data
  #mm_df <- read.csv(paste0(pr_mm_path, paste0(model, ".csv")))
  mm_df <- read.csv(paste0(pr_mm_path, "m12.csv"))
  for (i in seq_along(iterations)) {
    pr_res[[i]] <-
                    RunParRec(df,
                              model_number=which_model,
                              helpers,
                              mm_df,
                              par_combos=60)
  }
  par_recov_df <- pr_res %>% bind_rows()
  model_settings <- SetUpModelSpecStructsAndLabels(which_model, helpers)
  # Save out
  write.csv(par_recov_df,
            paste0(pr_path,
                   model_settings[["model_label"]],
                   '.csv'),
            row.names= FALSE)
}
############################################