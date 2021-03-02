## PACKAGE LOADING ####
#LoadPackages <- function() {
  ### Load packages we need for graphing and analyses specifically #

  # Notes: Invoked only for side effect of returning package loads to the global environment ###
#   packages_to_load <- c(
#     'ggplot2',
#     'ggthemes',
#     'latex2exp',
#     'lme4',
#     'tictoc',
#     'dplyr',
#     'profvis',
#     'cowplot',
#     'purrr',
#     'tidyverse',
#     'patchwork'
#   )
# lapply(packages_to_load, require, character.only = TRUE)
#}
############################################
#### FILE MANAGEMENT ####
# AssignParRes <- function(file_key, models, assignGE_return) {
#   ### Assigns par result files to the global environment or
#   # returns a single file #
#   
#   # Notes: If assignGE_return == 1, invoked only for side effect
#   # of returning the files to the global environment. 
#   # If == 2, returns the single result file for the model whose 
#   # string was passed in to the models arg
#   if (assignGE_return == 1) {
#     for (mfile in seq_along(models)) {
#       # assign the string as the model's name (eg RL_1) in 
#       # global env
#       
#       assign(models[mfile], 
#              
#              # merge list of same model dfs into single df and
#              # return the best fit for each ID, cond
#              MergeAndTakeBest(
#                # find all the par dfs we have from diff optimization
#                # runs for this model
#                lapply(sapply(file_key[models[mfile]], unlist), read.csv)
#              ),
#              # assign out to global environment
#              envir=.GlobalEnv
#       )
#     }
#   par_res <- NULL  # return null to prevent error on pass out
#   }
#   
#   if (assignGE_return == 2) par_res <- MergeAndTakeBest(
#               lapply(sapply(file_key[models], unlist), read.csv)
#   )
# par_res  
# }
#### SET UP RESULTS FILE PATHS ####
FindOptResult <- function(model) {
  ### Returns the empirical dataframe specified by the string model ###
  
  # Notes: 
  # Used in 1. simulation from sim_opt for extracting empirical opt results
  # to then create simulations matching them and 2. in parameter recovery to
  # create par combos in the range of the empirical data ###
  
  data_paths <- list()
  # optimization results from cluster live here
  data_paths['one'] <-
    './../../model_res/frld_wm_family/opt_res_final/best_fits/2_19_20/'
  file_key <- GatherFiles(data_paths)
  par_res <- AssignParRes(file_key, models = model, assignGE_return = 2)
  
par_res  
}
## Get the best overall fit between the datasets ####
GetBestFit <- function(df) {
  data.frame(df %>% group_by(ID) %>% slice(which.min(nll))) 
  # Below is deprecated since now fitting cond splits in single run 
  # data.frame(df %>% group_by(condition, ID) %>% slice(which.min(nll))) 
}
MergeAndTakeBest <- function(list_of_dfs) {
  ### Binds list of dfs into single df, finds the lowest nll for a given phase (ID, condition pair), 
  # returns a df with nrow=pts*conditions ###
  
  # combine into one df 
  df <- do.call(rbind, list_of_dfs)
  # return only the best nlls for eah phase
  best_df <- data.frame(df %>% group_by(condition, ID) %>% slice(which.min(nll)))  
  
best_df  
}
# LoadAndModifyEmpData <- function(path=2) {
#   
#   if (path==1) bx_df <- read.csv('./../../../master_files/master_df_full_file.csv')
#   if (path==2) bx_df <- read.csv('./../../master_files/master_df_full_file.csv')
#   bx_df$correct <- NA
#   bx_df['correct'] <- ifelse(test=bx_df$pr_rew == .75, yes=1, no=0)
# bx_df  
# } 
# Enables more transparent file names 
Jn <- function(path, file_name) paste0(path, toString(file_name), '.csv')
# for saving vars wo overwriting
GenRandString <- function() paste0('_', toString(round(runif(1, 1000, 9999)))) 
############################################
#### GRAPHING UTILS ####
DefPlotPars <- function() {
  ### Set up some plot aspects we'll reuse across functions ####
  
  # Notes: Invoked only for side effect of returning general plot pars
  # to the global environment ###
  ga <<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  # legend pars #
  lp <<- theme(legend.text = element_text(size = 20),
               legend.title = element_blank(),
               legend.key.size = unit(2.5, 'lines'))
  
  # turn off legend
  tol <<- theme(legend.position='none')
  
  # axis pars #
  ap <<- theme(axis.text = element_text(size=15),
               axis.title = element_text(size=20))
  # title pars #
  tp <<- theme(plot.title = element_text(size = 20, face='bold', hjust = .5))
  
  stp <<- theme(plot.subtitle = element_text(size=14))
  # color pars #
  cf_vals <<- c('orange', 'purple')
}
CompNLLNestedModels <- function(par_df1, par_df2, comparison_string=NULL) { 
  ### Plots a comparison between two nested models #
  
  full_par_list <- c(
    'beta', # inverse temp on weights (incl. weights_mix, which is a mixture of WM + RL weights)
    'eta', # RL learning rate
    'd', # RL decay
    'alpha', # mixture weight on WM weights contribution to WM mix (then RL weights contr. = (1-alpha) * RL)
    'dyn_WM', # scales WM weights down as a fx of trial, enabling decreasing contribution with time
    'adjust_WM', # adjusts WM weights downward following nonreward (otherwise WM weights do nothing after nonrew)
    'decay_WM', # WM decay
    'attn_beta', # softmax weight on RL weights, leading in the limit to winner-take-all dynamics
    'sigma_int', # int of uncertainty modulator
    'sigma_slope', # slope of uncertainty modulator
    'rho_RL', # reward sensitivity for RL (rhos are only meaningful in WM+RL models)
    'rho_WM', # reward sensitivity for WM
    'color_eta', # learning rate specific to color dimension
    'shape_eta', # learning rate specific to shape dimension
    'texture_eta', # learning rate specific to texture dimension
    'left_beta', # inv temp for leftmost stim
    'middle_beta', # inv temp for middle stim
    'right_beta', # inv temp for rightmost stim
    'kappa', # mixture on stat optimal attn weight
    'bayes_beta', # softmax if directly mapping bayes probs to action probs
    'eps', # undirected noise
    'delta', # determines size of phi_RL dimensional attention weight
    'lambda', # mixing param for hybrid bayes-RL model directly mapping bayes probs -> actions
    'dec_eta', # learning rate that doesn't scale reward thus should somewhat decouple from inverse temp
    'color_dec_eta', # decoupled learning rate specific to color dimension
    'shape_dec_eta', # dec. learning rate specific to shape dimension
    'texture_dec_eta' # dec. learning rate specific to texture dimension
  )
  
  # Notes: Model 1 must always be more complex, otherwise will throw an error ###

  # extract the names of the pars in the model
  m1_pars <- names(par_df1)[which(names(par_df1) %in% full_par_list)]
  m2_pars <- names(par_df2)[which(names(par_df2) %in% full_par_list)]
  
  if (length(m1_pars) != length(m2_pars)) {
    if (all(m1_pars %in% m2_pars)) {
      # if the models are fully nested, find which pars are added to the more complex model
      extra_pars <- setdiff(m2_pars, m1_pars)
      fully_nested_model <- 1
    } else {
      # if the models are not completely or at all nested, find which pars are different in m2
      #diff_pars <- setdiff(m2_pars, m1_pars)
      fully_nested_model <- 0
    }
  } else {
    fully_nested_model <- 0
  }
  
  ## create a df for plotting comprising ID, condition, and the diff in NLLs
  # first create a skeletal df of just condition and ID
  comp_df <- par_df2[!names(par_df2) %in% c(m2_pars, 'nll')]
  comp_df['nll_diff'] <- par_df2['nll'] - par_df1['nll'] 
  #browser()
  # calculate the term in bic that scales with the parameter
  # number ie par/2 * log(N). note there were 660 trials for 
  # all pts fit in this project
  if (fully_nested_model) bic_penalty_line <- -length(extra_pars)/2 * log(660)

  if (!fully_nested_model) {
    # ** to del if below works
    # if(length(m2_pars) - length(m1_pars) > 0) {
    #   bic_penalty_line <-  -(length(m2_pars) - length(m1_pars))/2 * log(660)
    # } else {
    #   bic_penalty_line <- 0
    # }
    
    # assign the appropriate bic penalty line (0 if the models have same # of pars)                           
    bic_penalty_line <- ifelse(test = length(m2_pars) - length(m1_pars) > 0,
                               yes = -(length(m2_pars) - length(m1_pars))/2 * log(660),
                               no = 0)
  }
  
  nll_diff_line <- mean(comp_df$nll_diff) 
  
  # limit for x axis
  left_lim <- as.numeric(min(comp_df['nll_diff']) - 2)
  right_lim <- as.numeric(max(comp_df['nll_diff']) + 3)
  
  #mean(as.numeric(unlist(par_df2[extra_pars])))
  
  # If there is a BIC penalty line, make a joint line with it + the empirical average diff amount
  #if (grepl(exists(bic_penalty_line))) {
    joint_line <- geom_vline(xintercept=c(bic_penalty_line,
                                          nll_diff_line), size = 3, color=c('red', 'blue'))
  # } else {
  #   joint_line <- geom_vline(xintercept=nll_diff_line, size = 3, color=c('blue'))
  # }
    
  conditions = c('D', 'I')

  for (cond in seq_along(conditions)) {
    
    if (cond == 1) 
       { 
          if (fully_nested_model) ptitle <- ggtitle(paste('NLL improvement from adding', 
                                                          do.call(paste, c(as.list(extra_pars), sep = " + "))),
                                                          subtitle = 'NEUTRAL') 
          
          if (!fully_nested_model)  {
          
            if (exists('comparison_string')) { 
              ptitle <- ggtitle(comparison_string, subtitle = 'NEUTRAL') 
            } else {
              ptitle <- ggtitle('NLL difference M2 - M1', subtitle = 'NEUTRAL')
            }
            
           # ptitle <- 
           #   ifelse(test = exists('comparison_string'),
           #          # if a comparison string's been supplied, use it..
           #          yes = ggtitle(comparison_string, subtitle = 'NEUTRAL'),
           #          # ..otheriwse assign a generic title
           #          no = ggtitle('NLL difference M2 - M1', subtitle = 'NEUTRAL')
            
           #  )
          }
        
          dot_color <- 'yellow'
    }
    if (cond == 2) {
      
     ptitle <- ggtitle('',
                                     subtitle = (paste('RUMINATION')))
        dot_color <- 'purple'
    }
    
    #{if(exists('bic_penalty_line')) geom_vline(xintercept=bic_penalty_line, size = 3, color=c('red')) }
    
    this_cdf <- data.frame(comp_df %>% filter(condition==conditions[cond]))
    assign(conditions[cond], 
           ggplot(this_cdf, 
                                     aes(x=nll_diff, y=as.factor(ID))) + 
                    #{if(exists('bic_penalty_line')) joint_line } +
                    joint_line +
                    geom_point(size=4, fill=alpha(dot_color, .5), color='black', pch=21) +
                    xlim(left_lim, right_lim) + 
                    #xlim(c(-20, 1)) + # can use for adj_WM
                    ga + ap + tp + stp +
                    xlab('negative log likelihood difference') + ylab('ID') +
                    theme(axis.text.y = element_text(size = 9)) +
                    ptitle
           )
  }
comp_plot <- plot_grid(D, I, ncol=1)
}
CompareResByCond <- function(results_df, comparison_var) {
  ## code if higher or lower in rum to allow colored plots ##

  results_df <- results_df %>% mutate(condition = recode(condition, 
                                                         'D'='NEU',
                                                         'I'='RUM'
    )
  )
  
  results_df['cv'] <- results_df[comparison_var]
 # names(results_df[comparison_var]) <- setNames(results_df[comparison_var], c('cv'))
  
  results_df['lower_rum'] <- 
    rep(as.integer(
      results_df %>% 
        filter(condition == 'RUM') %>% select(comparison_var) < 
        results_df %>% filter(condition == 'NEU') %>% 
        select(comparison_var)), 
      2
    )
  
  results_df[comparison_var]
  #levels(results_df['condition']) <- c('NEU', 'RUM')
  #comparison_var <- 'd'
  
  turn_off_leg <- theme(legend.position = "none")
  plot <- ggplot(data=results_df, 
                 aes(x=condition, y=cv, group=ID, color=as.factor(lower_rum))) + 
    geom_bar(aes(x=condition, y=cv, group=condition), 
             fill='white', color=c('yellow', 'purple'), stat='summary', size=4) +
    geom_point(color='gray57', size=5, pch=21) +
    geom_line(aes(color=as.factor(lower_rum)), size=3, alpha=.2) + 
    scale_color_manual(values=cf_vals) + 
    scale_fill_manual(values=cf_vals,
                      labels=c('NEU', 'RUM')) +
    turn_off_leg +
    theme(axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=25),
          axis.title.x = element_text(size=35),
          axis.title.y = element_text(size=35)) +
    ga + 
    ylab(comparison_var) 
plot 
}
SaveMCPlot <- function(plot, file_string, devi='png', width=12, height=11) {
  ggsave(filename = paste0(fig_path,
                           'mc_',
                           file_string,
                           GenRandString(),
                           '.',
                           devi
  ),
  plot=plot,
  devi='png',
  width=12, height=9,
  dpi='retina'
  ) 
}
SaveVarCompPlot <- function(plot, model_string, par_string, devi='png', width=12, height=9) {
  ggsave(filename = paste0(fig_path,
                           model_string,
                           '_',
                           par_string,
                           GenRandString(),
                           '.',
                           devi
  ),
  plot=plot,
  device=devi,
  width=12, height=9,
  dpi='retina'
  ) 
}
PlotEmpVsSimMultiIter <- function(sim_df, emp_df) {
  ### Summarize group emp and sim data then output a 3-panel plot  #
  # of sim vs emp in each condition with all iters of sim shown, and then 
  # sim vs emp in both conditions with iters collapsed
  
  pcor_sum <- 
    sim_df %>% group_by(condition, trial) %>% summarize(m=mean(correct_vec))
  pcor_sum_iter <- 
    sim_df %>% group_by(condition, trial, iter) %>% summarize(m=mean(correct_vec))
  
  pcor_emp <- data.frame(emp_df %>% group_by(condition, trial) %>% 
                           summarize(m=mean(correct), se=sd(correct)/sqrt(n())))
  
  levels(pcor_emp$condition) <- c('D', 'I')
  levels(pcor_sum$condition) <- c('D', 'I')
  levels(pcor_sum_iter$condition) <- c('D', 'I')
  
  shared_ylim <- ylim(.3, .80)
  # empirically matched pars plot
  rum_plot <- 
    ggplot(pcor_emp %>% filter(condition=='I'), aes(x=trial, y=m)) +
    geom_point(data=pcor_sum_iter %>% filter(condition=='I'), 
               aes(x = trial, y = m, fill=as.factor(iter)),
               alpha=.8, color='black', shape=21, size=4, position='jitter') +
    geom_ribbon(aes(ymin=m-se, ymax=m+se), color='purple', fill='gray57', alpha=.4) +
    geom_line(size=2, color='purple') +
    ga + ap + tol + tp +
    ylab('proportion correct') +
    shared_ylim +
    ggtitle('RUMINATION')
  
  neu_plot <- 
    ggplot(pcor_emp %>% filter(condition=='D'), aes(x=trial, y=m)) +
    geom_point(data=pcor_sum_iter %>% filter(condition=='I'), 
               aes(x = trial, y = m, fill=as.factor(iter)),
               alpha=.8, color='black', shape=21, size=4, position='jitter') +
    geom_ribbon(aes(ymin=m-se, ymax=m+se), color='yellow', fill='gray57', alpha=.4) +
    geom_line(size=2, color='yellow') +
    ga + ap + tol + tp +
    ylab('proportion correct') +
    shared_ylim +
    ggtitle('NEUTRAL')
  
  both_plot <- 
    ggplot(pcor_emp, aes(x=trial, y=m, color=condition)) +
    scale_fill_manual(labels=c('NEU', 'RUM'),
                      values=cf_vals) +
    tol +
    geom_ribbon(aes(ymin=m-se, ymax=m+se), fill='gray57', alpha=.4) +
    geom_line(size=2) +
    scale_color_manual(values=cf_vals) +
    geom_point(data=pcor_sum, aes(x=trial, y=m, fill=condition),
               alpha=.8, color='black', shape=21, size=5) +
    ga + ap + lp +
    ylab('proportion correct') +
    shared_ylim
  
  #plot_grid(rum_plot, neu_plot, both_plot, ncol=1)
  both_plot + (rum_plot / neu_plot)
}
############################################
#### ANALYSIS HELPERS ####
TTestVarsByCond <- function(res_df, var) {
  ### Run a t-test on a variable in the RUM vs NEU cond ### 
  
  xdf <- res_df %>%  filter(condition=='I')
  x <- as.numeric(unlist(xdf[var]))                       
  ydf <- res_df %>%  filter(condition=='D') 
  y <- as.numeric(unlist(ydf[var]))
t.test(x, y, paired=TRUE)  
}
############################################