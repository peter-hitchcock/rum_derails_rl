SetUpModelSpecStructsAndLabels <- function(which_model, helpers) {
  ### Returns string vector of params and a label for this model ###
  base <- c('beta', 'eta')
  if (which_model == 1) param_labels <- base
  if (which_model == 2) {
    param_labels <- c(
      base,
      'd'
    )
  }
  if (which_model == 3) {
      param_labels <- c(
        base,
        'd',
        'eps'
      )
  } 
  if (which_model == 4) {
      param_labels <- c(
        base,
        'd',
        'attn_beta'
      ) 
  } 
  if (which_model == 5) {
    param_labels <- c(
      base,
      'd',
      'attn_beta',
      'eps'
    ) 
  } 
  if (which_model == 6) {
      param_labels <- c(
        'beta',
        'd',
        'color_eta',
        'shape_eta',
        'texture_eta'
      )
  } 
  if (which_model == 7) {
      param_labels <- c(
        base,
        'd',
        'delta'
      )
  } 
  if (which_model == 8) {
      param_labels <- c( 
        'bayes_beta'
      )
  } 
  if (which_model == 9) {
    param_labels <- c(
      base,
      'd',
      'attn_beta',
      'kappa'
    ) 
  } 
  if (which_model == 10) {
    param_labels <- c( 
      base,
      'd',
      'bayes_beta',
      'eps' 
    )
  }
  if (which_model == 11) {
    param_labels <- c( 
      base,
      'd',
      'kappa',
      'delta'
    )
  }
  if (which_model == 12) {
    param_labels <- c( 
      'beta',
      'd',
      'eta_pos',
      'eta_neg'
    )
  }
  if (which_model == 13) {
    param_labels <- c(
      base,
      'd',
      'kappa'
    )
  }
  if (which_model == 14) {
    param_labels <- c(
      'beta',
      'd',
      'attn_beta',
      'eta_pos',
      'eta_neg'
    )
  }
  split_vars <- paste0(helpers[["pars_to_split_by_cond"]], collapse = "_")
  model_label <- LabelModel(which_model, param_labels, split_vars)
list(
  "param_labels"=param_labels, 
  "model_label"=model_label
)
}