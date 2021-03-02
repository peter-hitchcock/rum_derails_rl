PlotSimVsRecov <- function(pr_df) {
  ### Return lists of plots of simulated vs recovered values for all plots in df ###
  par_names <- unlist(map(strsplit(names(pr_df)[grep("opt", names(pr_df))], "_o"), 1))
  plot_list <- lapply(par_names, function(x) {
    single_par_df <- na.omit(setNames(data.frame(pr_df %>% select(paste0(x, "_sim")),
                                                 pr_df %>% select(paste0(x, "_opt"))), 
                                      c("Simulated", "Recovered")))
    # Add exact copy of sim'ed to draw a line through to create identity line
    single_par_df$y_sim <- single_par_df$Simulated
    r_value <- round(cor(single_par_df[, 1], single_par_df[, 2]), 3)
    
    tmp_p <- ggplot(single_par_df, aes(x=Simulated, y=Recovered)) + 
      #geom_point(size=4, alpha=.9, pch=21, fill=sample(rainbow(100), 1)) + 
      geom_line(aes(x=Simulated, y=y_sim), linetype="longdash", size=2) +
      geom_point(size=4, alpha=.9, pch=21, fill="grey57") +
      #ggtitle(paste0(paste0(toupper(substr(x, 1, 1)), 
      #                substr(x, 2, nchar(x[1]))), 
      #               ": corr = ", r_value)) + 
      #geom_smooth(method="lm", color="black") +
      xlab("simulated") + ylab("recovered") +
      ga + ap + tp
    # r_str <- toString(r_value)
    tp <- bquote(eta)
    r_str <- toString(r_value)
    if (x == "eta") tmp_p <- tmp_p + ggtitle(paste("LR",  " : r = ", r_value))
    # + ggtitle(substitute(paste(eta,  " : r = ", r_value)))
    if (x == "eta_pos") tmp_p <- tmp_p + ggtitle(paste("+PE LR",  " : r = ", r_value))
    if (x == "eta_neg") tmp_p <- tmp_p + ggtitle(paste("-PE LR",  " : r = ", r_value))
    if (x == "beta") tmp_p <- tmp_p + ggtitle(paste("inverse temp.",  " : r = ", r_value))
    if (x == "d") tmp_p <- tmp_p + ggtitle(paste("decay rate",  " : r = ", r_value))
    # if (x == "eta_pos") tmp_p <- tmp_p + ggtitle(substitute(paste(eta,  "+ : r = ", r_value)))
    # if (x == "eta_neg") tmp_p <- tmp_p + ggtitle(substitute(paste(eta,  "- : r = ", r_value)))
    # if (x == "beta") tmp_p <- tmp_p + ggtitle(substitute(paste(beta,  " : r = ", r_value)))
    # if (x == "d") tmp_p <- tmp_p + ggtitle(substitute(paste(d,  " : r = ", r_value)))
    # #if (x == "d") tmp_p <- tmp_p + ggtitle(substitute(paste(d,  " : r = ", r_value)))
    # if (x == "attn_beta") tmp_p <- tmp_p + ggtitle(substitute(paste(beta^A,  " : r = ", r_value)))
    tmp_p    
  }
  )
plot_list
}
m12s <- PlotSimVsRecov(m12pr)
b <- 
  (m12s[[1]]+ labs(subtitle="B") | m12s[[2]]) / (
    (m12s[[3]] | m12s[[4]] )) 
# ) + 
# plot_annotation(
#   title = 'B fRL + d, different learning rates'
# )
b