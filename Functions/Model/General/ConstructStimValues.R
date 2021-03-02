ConstructStimValues <- function(
                                s1, s2, s3,
                                color_RL_weights,
                                shape_RL_weights,
                                texture_RL_weights,
                                param_labels,
                                attn_weights=0) {
  ### Constructs Stimulus values, including incorporating attentional biases ###
  # Places various kinds of weight-based biases (eg. attnl, bayes-attnl) on RL weights before #
  # value construction. Note that these bias the values at construction without ever altering the #
  # weights RL vec. Hence, there is no + feedback between weights and attn over trials #
  if (
    "attn_beta" %in% param_labels |
    "delta" %in% param_labels 
    ) {
    # Bayesian feature + feature attn RL / just feature RL (latter Jaskir et al 17) #
    if ('attn_beta' %in% param_labels) {
      color_RL_weights <- color_RL_weights * attn_weights[1:3]
      shape_RL_weights <- shape_RL_weights * attn_weights[4:6]
      texture_RL_weights <- texture_RL_weights * attn_weights[7:9]
    }
    # Bayesian + dim RL / just dim RL (Daniel et al 20) #
    if ('delta' %in% param_labels) {
      color_RL_weights <- color_RL_weights * as.numeric(attn_weights[1]) 
      shape_RL_weights <- shape_RL_weights * as.numeric(attn_weights[2]) 
      texture_RL_weights <- texture_RL_weights * as.numeric(attn_weights[3]) 
    }
  } else if ("kappa" %in% param_labels) {
      color_RL_weights <- color_RL_weights * attn_weights[1:3]
      shape_RL_weights <- shape_RL_weights * attn_weights[4:6]
      texture_RL_weights <- texture_RL_weights * attn_weights[7:9]
  }
  # Construct V(S) and VS vec 
  vs1 <- color_RL_weights[s1[1]] + shape_RL_weights[s1[2]] + texture_RL_weights[s1[3]]
  vs2 <- color_RL_weights[s2[1]] + shape_RL_weights[s2[2]] + texture_RL_weights[s2[3]]
  vs3 <- color_RL_weights[s3[1]] + shape_RL_weights[s3[2]] + texture_RL_weights[s3[3]]
  vs_vec <- c(vs1, vs2, vs3) 
vs_vec
}