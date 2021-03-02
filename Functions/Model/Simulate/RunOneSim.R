RunOneSim <- function(helpers, 
                      emp_pars,
                      pt_df
) {
  ### Simulates using same task trajectory as pt and their empirical fit parmams ###
  # Extract just the task parameters (ie ignore anything pt specific)
  sim_df <- pt_df[, c("game", "trial", "ID",
                      "s1_C", "s1_S", "s1_T",
                      "s2_C", "s2_S", "s2_T", 
                      "s3_C", "s3_S", "s3_T", 
                      "relevant_dimension", "relevant_feature", "condition")]
  # Simulate using the pars and task parameters
  sim_outs <- RunModel(params=emp_pars, this_phase=sim_df, sim_opt=1, helpers)
  pt_df$sim_correct <- sim_outs[["correct_vec"]]
  pt_df <- cbind(pt_df, sim_outs[["vs_vecs"]])
  #vs_vecs <- sim_outs[["vs_vecs"]]
#list("pt_df_with_sim"=pt_df, "vs_vecs"=vs_vecs) 
pt_df  
}