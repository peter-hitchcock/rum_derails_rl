LabelModel <- function(which_model, param_labels, split_vars=0) {
  ### Returns a model label so can save models without hard coding names ###
  # Label is of form: #
  # m_numb_paramsinthemodel_randstr_splitvars,
  # where numb is the number that identifies the model 
  # and the random string prevents file overwriting ###
  GenRandString <- function() round(runif(1, 1000, 9999))
  label <- 
    paste('m', sep = '_', 
          paste(which_model, sep = '_'), 
          paste0(param_labels[1:length(param_labels)], sep = '_', collapse = ''), 
          GenRandString()
  )
  if (nchar(split_vars) > 0) {
    label <- paste0(label, "_split_on_", split_vars)  
  }
label
}