FindOptResult <- function(model) {
  ### Returns the empirical dataframe specified by the string model ###
  
  # Notes: 
  # Used in 1. simulation from sim_opt for extracting empirical opt results
  # to then create simulations matching them and 2. in parameter recovery to
  # create par combos in the range of the empirical data ###
  
  data_paths <- list()
  # optimization results from cluster live here
  data_paths['one'] <- "./../model_res/opt_res/best_fits/batch1/"
  file_key <- GatherFiles(data_paths)
  par_res <- AssignParRes(file_key, models = model, assignGE_return = 2)
  
par_res  
}