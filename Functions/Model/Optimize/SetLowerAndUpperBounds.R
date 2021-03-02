SetLBAndUB <- function(helpers) {
  ### Returns lower and upper bounds for optimization ###
  
  param_labels <- helpers[["param_labels"]]
  full_par_list <- helpers[["full_par_list"]]
  lb <- data.frame(
                    "beta"=0, 
                    "eta"=0,
                    "d"=0,
                    "attn_beta"=0,
                    "color_eta"=0,
                    "shape_eta"=0,
                    "texture_eta"=0,
                    "kappa"=0,
                    "bayes_beta"=0,
                    "eps"=0,
                    "delta"=0,
                    "lambda"=0,
                    "eta_pos"=0,
                    "eta_neg"=0
                  )
  ub <- data.frame(
                   "beta"=40, 
                   "eta"=1,
                   "d"=1,
                   "attn_beta"=5,
                   "color_eta"=1,
                   "shape_eta"=1,
                   "texture_eta"=1,
                   "kappa"=1,
                   "bayes_beta"=5,
                   "eps"=1,
                   "delta"=1,
                   "lambda"=1,
                   "eta_pos"=1,
                   "eta_neg"=1
                   )
  # ub <- c(
  #         40, # beta
  #         1, # eta
  #         1, # d
  #         1, # color_eta
  #         1, # shape_eta
  #         1, # texture_eta
  #         1, # kappa
  #         50, # bayes_beta
  #         1, # eps
  #         1, # delta
  #         1 # lambda
  #         )[which(full_par_list %in% param_labels)]
  
  lb <- lb %>% select(param_labels)
  ub <- ub %>% select(param_labels)
  
list("lb"=lb, "ub"=ub)
}