#!/usr/bin/env Rscript
#

library(reshape2)
library(plyr)
library(ggplot2)
source('src/R/sim_results_functions.R')
source('src/R/mplus_functions.R')

get_sim_results <- function(model_name, dat, pop_values){
  non_converged <- sum(apply(dat, 1, function(x) all(is.na(x))))
  dat <- dat[apply(dat, 1, function(x) !all(is.na(x))), ]
  tmp_names <- get_results_names(model_name)
  dat_long <- melt(dat, id.vars = c("nt", "np", "dg", "est", "rep", "design")
                   , measure.vars = tmp_names
                   , variable.name = "param")
  dat_long_ses <- melt(dat, id.vars = c("nt", "np", "dg", "est", "rep", "design")
                       , measure.vars = sprintf("se_%s", tmp_names)
                       , variable.name = "param")
  dat_long_pops <- merge(pop_values, dat_long, by = "param")
  outs <- ddply(dat_long_pops, .(nt, np, dg, est, design, param), summarize
                , true_value = unique(pop_values)
                , mean_param = mean_(value), sd_param = sd_(value)
                , bias_param = bias(value, unique(pop_values))
                , rel_bias_param = rel_bias(value, unique(pop_values))
                , rmse_param = rmse(value, unique(pop_values)))
  outs_ses <- ddply(dat_long_ses, .(nt, np, dg, est, design, param), summarize
                    , mean_se = mean_(value), sd_param = sd_(value)
                    )
  return(list(outs, outs_ses, non_converged))
}

get_fit_comparison_results <- function(dat_list){
  flat_results <- ldply(dat_list, rbind)
  ddply(flat_results, .(nt, np, design, rep), summarize, best_bic = est[Bayesian..BIC. == min(Bayesian..BIC., na.rm=T)])
}
model_names = c("LCM", "MLM", "AR", "MA")
data_generating_model <- "LCM"
estimated_models = c("LCM", "MLM", "AR", "MA")
result_list <- mapply(function(y, z){
  read.table(file = sprintf("data/output/sim_results/raw_results/dg%s_est%s.csv", y, z)
             , sep = ",", stringsAsFactors = FALSE)
}, data_generating_model, estimated_models)
names(result_list) <- estimated_models
pop_values_all <- get_population_values()
pop_values <- data.frame(param = get_results_names(data_generating_model)
                         , pop_values = get_pop_values_mplus_order(data_generating_model, pop_values_all)
)
out <- get_sim_results(data_generating_model, result_list[[data_generating_model]], pop_values)
t <- out[[1]]
#pdf("data/output/bias_param.pdf")
ggplot(t, aes(x = param, y = bias_param, group = factor(design)
              , colour = factor(design)
              , shape = factor(design))) + geom_line() + geom_point() +
  facet_wrap(~np) + ggtitle(data_generating_model)
#dev.off()


