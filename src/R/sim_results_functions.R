#Mean
mean_ = function(x){
  mean(x,na.rm=TRUE)
}
#SD
sd_ = function(x){
  sd(x,na.rm=TRUE)
}
#Bias function
bias = function(x,truex){
  mean(x,na.rm=T) - truex
}
#Relative Bias function
rel_bias = function(x,truex){
  (mean(x,na.rm=T) - truex)/truex
}
#CI coverage
lcl = function(x,xse){
  x-(1.96*xse)
}
ucl = function(x,xse){
  x+(1.96*xse)
}
coverage = function(lcl,ucl,truex){
  if (ucl>truex && truex>lcl) 1
  else 0
}
#Power
power = function(x,xse){
  if (abs(x/xse) > 1.96) 1
  else 0 
}
#RMSE
rmse = function(x,truex){
  sqrt(mean_((x-truex)^2))
}

arsde = function(SEhat,SE){
  (SEhat-SE)/SE
}
cv = function(x,SE){
  SE/x
}

get_counts_best_fits <- function(flat_results, fit_stat){
  fit_best_long <- ddply(flat_results, .(nt, np, design, rep), summarize
                         , best_fit = est[grep(min(get(fit_stat), na.rm=T), get(fit_stat))])
  fit_best <- ddply(fit_best_long, .(nt, np, design), summarize, lcm_best_bic = length(which(best_fit == "LCM"))
                    , mlm_best_fit = length(which(best_fit == "MLM"))
                    , ar_best_fit = length(which(best_fit == "AR"))
                    , ma_best_fit = length(which(best_fit == "MA")))
  names(fit_best) <- gsub("fit", fit_stat, names(fit_best))
  #table_best_fits <- get_table_best_fits(fit_best_long)
  return(fit_best)
}
get_table_best_fits <- function(fit_best_long){
  table(fit_best_long$np, fit_best_long$design, fit_best_long$best_fit)
}
get_fit_comparison_results <- function(dat_list, fit_stat_list){
  flat_results <- ldply(dat_list, rbind)
  fit_counts <- sapply(fit_stat_list, function(x) get_counts_best_fits(flat_results, x), simplify=FALSE)
  names(fit_counts) <- fit_stat_list
  return(fit_counts)
}

get_mean_fit_statistics_results <- function(dat_list){
  flat_results <- ldply(dat_list, rbind)
  fit_means <- ddply(flat_results, .(nt, np, design, est), summarize, mean_bic = mean_(Bayesian..BIC.)
                     , mean_aic = mean_(Akaike..AIC.), mean_Nadj_BIC = mean_(Sample.Size.Adjusted.BIC)
                     , mean_rmsea = mean_(RMSEA...Estimate))
  return(fit_means)
}