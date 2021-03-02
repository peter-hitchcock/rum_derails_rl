GenRandParInits <- function(helpers) {
  ### Returns random parameter initializations within a reasonable range for the par ###
  param_labels <- helpers[["param_labels"]]
  full_par_list <- helpers[["full_par_list"]]
  rand_lb <- rep(0, length(full_par_list))[which(full_par_list %in% param_labels)] 
  rand_ub <-  c(20, # beta
               .4, # eta
                1, # decay
                5, # attn_beta
                .4, # color_eta
                .4, # shape_eta
                .4, # texture_eta
                1, # kappa
                3, # bayes_beta
                1, # eps
                1, # delta
                1, # eta_pos
                1 # eta_neg
               )[which(full_par_list %in% param_labels)] 
  rand_pars <- rep(NA, length(param_labels))
  for (rand_init in 1:length(param_labels)) {
    midway_point <- (rand_ub[rand_init] - rand_lb[rand_init]) / 2
    rand_pars[rand_init] <- 
      rtruncnorm(1, 
                 a=rand_lb[rand_init], 
                 b=rand_ub[rand_init], 
                 mean=midway_point, 
                 sd=midway_point
                 ) 
  }
rand_pars
}