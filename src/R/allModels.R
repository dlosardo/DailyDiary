rm(list=ls(all=TRUE)) #To remove all stored objects
set.seed(34213455)
########################
########################
########################
#Data simulation for ALL models
library(reshape2)
library(plyr)
source('src/R/mplus_functions.R')
source('src/R/matrix_functions.R')
source('src/R/compileMplusScript.R')
source('src/R/diary_designs.R')
source("src/R/model_matrices.R")
source("src/R/simulate_data.R")

# CONSTANTS
nreps = 1 #number of Monte Carlo runs
# MC conditions
time_points = c(14) #Number of time points
sample_sizes = c(500) #Number of participants
##Ordering of Models
### 1 - LCM
### 2 - AR
### 3 - MA
### 4 - ARMA
model_names = c("LCM", "AR", "MA", "ARMA", "ALT")
data_generating_models = c(1)
estimated_models = c(1)
# features of models
nlv <- 2 # number of latent variables
ny <- 1 # number of sets of observed variables
nx <- 0 # number of fixed regressors (covariates)
p <- 1 # number of AR lags
q <- 1 # number of MA lags
nstates <- get_nstates(model_names[data_generating_models], nlv, p, q) # number of states in transition matrix (MA(1) has 2, ARMA(1, 1) has 2)
# population values
pop_values_all <- list(LCM = list(c(.3, .05, .1), c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2), c(1,0.02))
                   , AR = list(c(.7), c(1))
                   , MA = list(c(.7), c(1))
                   , ARMA = list(c(.7), c(.3), c(1))
                   , ALT = list(c()))
######Ordering of Daily Diary Designs
### 1 - Complete Data
### 2 - One-Day Interval Design
### 3 - One-Day Interval Stagger Design
### 4 - One-Day Interval Block Design
### 5 - Week, Staggered, No Overlap Block Design
### 6 - Five-Day Staggered, One Day Overlap Block Design
### 7 - Five-Day Staggered, Two Day Overlap Block Design
### 8 - Skip the Weekend Design
### 9 - Alternating Skip the Weekend Design
### 10 - Reference, Block Design
### 11 - Reference, Two-Day Overlap Block Design
### 12 - Reference One-Day Interval Design
### 13 - Partially Random
designs = c(1, 7, 6, 11, 12, 13)
all_results = data.frame() # Holds Monte Carlo results across conditions
for (t in 1:length(time_points)){
  nt = time_points[t] 	# number of time points
	for (s in 1:length(sample_sizes)){
	  np = sample_sizes[s]	# number of subjects
		for (m in 1:length(data_generating_models)){
			cur_model = data_generating_models[m]
      model_name <- model_names[cur_model]
      pop_values <- pop_values_all[[match(model_name, names(pop_values_all))]]
			for (run in 1:nreps){
			  model_matrices <- model_matrix_setup(model_name, ny, nt, nlv, pop_values, nstates, p, q)
        simulated_data <- simulate_data(model_matrices, ny, nt, nlv, pop_values, nstates)
        complete_dat = simulated_data$y_all_wide[, 2:(nt + 1)]
				for (d in 1:length(designs)){
					design = designs[d] # Daily Diary Design
          # get information for missingness given design
					missing_info <- missing_info_for_designs(design, nt, np)
          nonmissall <- missing_info$nonmissall
          # induce missingness into complete data set
					missing_dat <- t(sapply(1:np, function(x){
            tmp <- matrix(NA, 1, nt)
            tmp[nonmissall[x, ][!is.na(nonmissall[x, ])]] <- unlist(complete_dat[x,][nonmissall[x,][!is.na(nonmissall[x, ])]])
            return(tmp)
					  }))
					# write out data file with missingness in it
					write.table(missing_dat, file = paste0("data/work/prepped/", model_name, ".dat")
					            , row.names = FALSE
					            , col.names = FALSE
					            , sep = ","
                      , na = '.')
				
					# Here is where the data is estimated with a different model
					for (mest in 1:length(estimated_models)){
						print(paste(c(" iter: ", run, " N: ", np, "D: " , d)))
						model_est = estimated_models[mest]
            model_est_name <- model_names[model_est]
						
  					# generate mplus script
						create_mplus_script(model_est_name, model_name, "ML", sprintf("y%d", 1:nt))
						# Opening Mplus
						shell_file = paste("bin/mplus_open_", model_est_name, ".sh", sep = "") #shell script for opening Mplus
						shell_file = shQuote(shell_file)
            system(paste(shell_file), wait = TRUE)
            output <- read_mplus_output(model_est_name)[[1]]
            error_flag = 0
            # the following checks to see if there were errors and values were not saved
            error_terms = output[output %in% c("not", "terminate", "normally.")]
  					if (length(error_terms) == 3) error_flag = 1
  				  error_terms_1 = output[output %in% c("ERROR")]
  					if (length(error_terms_1) == 1) error_flag = 2
						tmp_results <- read_mplus_results(model_est_name)[[1]]
						results <- data.frame(nt = nt, np = np, dg = model_name, est = model_est_name
						                      , rep = run, design = d, error_flag = error_flag, t(tmp_results))
						all_results <- rbind.fill(all_results, results)
            unique_dim_names <- getResultsAfterParms(output)
  				}# mest loop
  			}# d loop
			}# run loop
		}# m loop
	}# s loop
}#t loop

#plot(1:nt, y_all_wide[1, 2:(nt + 1)], type="n", ylim = c(-4, 4))
#tmp <- sapply(1:np, function(x) lines(1:nt, y_all_wide[x, 2:(nt + 1)]))
#write(t(AllMC),file = "C:/Documents and Settings/Diane Losardo/Desktop/DDpaper/newResults/LCM/final/ALLRESULTS.dat",ncol=dim(AllMC)[2],append=F)
