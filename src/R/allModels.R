rm(list=ls(all=TRUE)) #To remove all stored objects
set.seed(34213455)
########################
########################
########################
#Data simulation for ALL models
library(reshape2)
library(plyr)
library(KFAS)
source('src/R/mplus_functions.R')
source('src/R/matrix_functions.R')
source('src/R/compileMplusScript.R')
source('src/R/diary_designs.R')
source("src/R/model_matrices.R")
source("src/R/simulate_data.R")
source("src/R/population_values.R")

# CONSTANTS
nreps = 2 #number of Monte Carlo runs
# MC conditions
time_points = c(14) #Number of time points
sample_sizes = c(50, 100, 300) #Number of participants
# daily diary designs
all_designs <- c("complete", "CSD_1dayOL", "CSD_2dayOL", "reference"
                 , "TCDM_5", "random_5", "TCDM_8", "random_8")
designs <- all_designs # designs to be used in simulation
##Possible models to choose from:
model_names = c("LCM", "MLM", "AR", "MA", "ARMA", "ALT")
data_generating_model = c("LCM")
estimated_models = c("LCM", "MLM", "AR", "MA")
# features of models
nlv <- get_nlv(data_generating_model) # number of distinct latent variables
ny <- 1 # number of sets of observed variables
#nx <- 0 # number of fixed regressors (covariates)
p <- 1 # number of AR lags
q <- 1 # number of MA lags
nstates <- get_nstates(data_generating_model, nlv, p, q) # number of states in transition matrix (MA(1) has 2, ARMA(1, 1) has 2)

# population values (change in population_values.R)
pop_values_all <- get_population_values()

# setting up list of dataframes to hold results
col_names_results <- sapply(estimated_models, function(x) c(get_sim_info_names(), get_all_names(x), get_mplus_standard_results()), simplify = FALSE)
all_results_list <- lapply(estimated_models, function(x){
                                    tmp <- data.frame(matrix(NA
                                                             , nrow = length(designs)*length(sample_sizes)*nreps
                                                             , ncol = length(col_names_results[[x]])))
                                    names(tmp) <- col_names_results[[x]]
                                    return(tmp)
                                    })
names(all_results_list) <- estimated_models
for (t in 1:length(time_points)){
  nt = time_points[t] 	# number of time points
	for (s in 1:length(sample_sizes)){
	  np = sample_sizes[s]	# number of subjects
		for (model_name in data_generating_model){
      pop_values <- pop_values_all[[match(model_name, names(pop_values_all))]]
			for (run in 1:nreps){
			  model_matrices <- model_matrix_setup(model_name, ny, nt, nlv, pop_values, nstates, p, q)
        simulated_data <- simulate_data(model_matrices, ny, nt, nlv, pop_values, nstates)
        complete_dat = simulated_data$y_all_wide[, 2:(nt + 1)]
				for (d in 1:length(designs)){
          design <- designs[d]
          # get information for missingness given design
					missing_info <- missing_info_for_designs(design, nt, np)
          nonmissall <- missing_info$nonmissall
          # induce missingness into complete data set
					missing_dat <- t(sapply(1:np, function(x){
            tmp <- matrix(NA, 1, nt)
            tmp[nonmissall[x, ][!is.na(nonmissall[x, ])]] <- unlist(complete_dat[x,][nonmissall[x,][!is.na(nonmissall[x, ])]])
            return(tmp)
					  }))          
					# Here is where the data is estimated with a different model
					for (model_est_name in estimated_models){
						print(paste(c(" iter: ", run, " N: ", np, "D: " , design, "est: ", model_est_name)))
						# write out data file with missingness in it (long format for MLM)
            if (model_est_name == "MLM"){
              missing_dat <- melt(missing_dat, varnames = c("id", "time_point"))
              if (all(model_matrices$covariate == 1)){
                model_matrices$covariate <- matrix(runif(np*nt, -4, 4), np, nt)
              }
              covariate_dat <- t(sapply(1:np, function(x){
                tmp <- matrix(NA, 1, nt)
                tmp[nonmissall[x, ][!is.na(nonmissall[x, ])]] <- unlist(model_matrices$covariate[x,][nonmissall[x,][!is.na(nonmissall[x, ])]])
                return(tmp)
              }))
              covariate_dat <- melt(covariate_dat
                                    , varnames = c("id", "time_point")
                                    , value.name = "covariate")
              missing_dat <- merge(missing_dat, covariate_dat, by = c("id", "time_point"))
              missing_dat <- missing_dat[with(missing_dat, order(id, time_point)), ][, c("id", "value", "covariate")]
						  # generate mplus script
              create_mplus_script(model_est_name, model_name, "ML", c("id", "y", "x"))
            } else {
              # generate mplus script
						  create_mplus_script(model_est_name, model_name, "ML", sprintf("y%d", 1:nt))
            }
            # writing out data
						write.table(missing_dat, file = paste0("data/work/prepped/", model_name, ".dat")
						            , row.names = FALSE
						            , col.names = FALSE
						            , sep = ","
						            , na = '.')
						# Opening Mplus
						shell_file = paste("bin/mplus_open_", model_est_name, ".sh", sep = "") #shell script for opening Mplus
						shell_file = shQuote(shell_file)
            system(paste(shell_file), wait = TRUE)
            output <- read_mplus_output(model_est_name)[[1]]
            error_flag = 0
            # the following checks to see if there were errors and values were not saved
						error_phrase <- "No results were saved"
            error_flag_1 = ifelse(any(grepl(error_phrase, output)), 1, 0)
  				  error_flag = ifelse(any(grepl("ERROR", output)), 1, 0)
            if (error_flag == 0 & error_flag_1 == 0){
  						tmp_results <- read_mplus_results(model_est_name)[[1]]
  						results <- data.frame(nt = nt, np = np, dg = model_name, est = model_est_name
  						                      , rep = run, design = design, error_flag = error_flag, t(tmp_results), stringsAsFactors = FALSE)
              #all_results_list[[model_est_name]] <- rbind.fill(all_results_list[[model_est_name]], results)
  						names(results)[!names(results) %in% get_sim_info_names()] <- c(get_all_names(model_est_name), getResultsAfterParms(output))
              all_results_list[[model_est_name]][d + (run - 1)*length(designs) + (s - 1)*length(designs)*nreps
                , match(names(results), names(all_results_list[[model_est_name]]))] <- results
            }
  				}# mest loop
  			}# d loop
			}# run loop
		}# m loop
	}# s loop
}#t loop
# writing out data
invisible(mapply(function(x, y, z){
  write.table(x, file = sprintf("data/output/sim_results/raw_results/dg%s_est%s.csv", y, z), sep = ",")
  }, all_results_list, data_generating_model, estimated_models)
)

