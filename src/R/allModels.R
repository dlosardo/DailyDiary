########################
########################
########################
#Data simulation for ALL models
library(gregmisc) #for getting number of non-missing obs 

# Calculates the number of variables given a vector of unique elements in a covariance matrix
get_num_vars <- function(x){
  (sqrt(8 * x + 1) - 1)/2
}

vector_to_symmetric_cov_matrix <- function(values){
  nvars <- get_num_vars(length(values))
  mat <- matrix(0, nvars, nvars)
  mat[upper.tri(mat, diag = TRUE)] <- values  #full acov matrix
  mat <- mat + t(mat) - diag(diag(mat)) #making acov square
  return(mat)
}
vector_to_non_symmetric_square_matrix <- function(values){
  nvars <- sqrt(length(values))
  mat <- matrix(values, nvars, nvars, byrow = TRUE)
  return(mat)
}

lv_cov_matrix <- function(values){
  vector_to_symmetric_cov_matrix(values)
}

transition_matrix <- function(values){
  vector_to_non_symmetric_square_matrix(values)
}

#Creating shell file
create_shell_script = function(name, dir_in, dir_out, file){
  cat(paste("#!/bin/sh
							
cd ", dir_in, "\n
/Applications/Mplus/mplus ", dir_in, "/", name, ".inp > ", dir_out, "/", name, ".out"
            , sep = ""), file = file, append = FALSE)
}

#constants 
nlv <- 3
ny <- 1
nx <- 0
# lv covs, measurement covs 
pop_values <- list(c(.3, .05, .1), c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2), c(1,0.02))
#rm(list=ls(all=TRUE)) #To remove all stored objects
set.seed(3421345)
nreps = 5 #number of Monte Carlo runs
time_points = c(14) #Number of time points
sample_sizes = c(300) #Number of participants

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

designs = c(1)#,7,6,11,12,13)

##Ordering of Models
### 1 - LCM
### 2 - MLM
### 3 - AR
### 4 - MA
data_generating_models = c(1)
estimated_models = c(3)
ARMA = TRUE
model_names = c("LCM", "MLM", "AR", "MA")

###Setting directories
#directory with data generating and code compiling files
#Mplus directory
mplus_dir = "C:/Program Files/Mplus/"
#directory to store results
results_dir = "data/output"

all_results = NULL # Holds Monte Carlo results across conditions
for (t in 1:length(time_points)){
  nt = time_points[t] 	# number of time points
	for (s in 1:length(sample_sizes)){
	  np = sample_sizes[s]	# number of subjects
		#if (ARMA) {MCfile = matrix(NA,noMC*length(theDesigns)*length(theModelsToEstimate),27)} else {MCfile = matrix(NA,noMC*length(theDesigns)*length(theModelsToEstimate),21)} #Holds Monte Carlo results within one condition
					#21 comes from 18 pieces of output from Mplus plus 3
		for (m in 1:length(data_generating_models)){
			cur_model = data_generating_models[m]
      model_name <- model_names[cur_model]
			for (run in 1:nreps){
			  if (cur_model==1){
          source("src/R/LCMdatagen.R")
			  }
  			if (cur_model==2){ 
          source("src/R/MLMdatagen.R")
  			}
  			if (cur_model==3){ 
          source("src/R/ARdatagen.R")
  			}
  			if (cur_model==4){ 
          source("src/R/MAdatagen.R",sep="")
  			}
			  data = paste("data/work/", model_name, ".data.dat", sep = "") #data file
			  inp = paste("src/mplus/", model_name, ".inp", sep = "") #input file for Mplus
		  	compile = paste("src/R/compile", model_name, "code.R", sep = "") #r code for compiling Mplus code
			  shell_file = paste("bin/", model_name, "mplusopen", model_name,".sh", sep="") #batch file for opening Mplus
		  	shell_file = shQuote(shell_file)
	  		results_mplus = paste("data/work/mplus/item_parameters/", model_name, "_results.dat", sep = "") #results file that mplus outputs
	  		output_mplus = paste("data/work/mplus", model_name, ".out",sep="") #for errors - whole output file
	  		results_dir = paste("data/work/mplus/", model_name, "/", model_name, "D",sep="") #directory path where final results will be stored
			
				for (d in 1:length(designs)){
					design = designs[d] #Daily Diary Design
					############
					##for all data:
					if (design == 1) {
						nonmiss = c(1:nt)
						nonmissall = matrix(nonmiss, np, length(nonmiss), byrow = T)
						combine = nonmiss
					}
					##################################################
					#Creating missingness for One-Day Interval Design#
					##################################################
					if (design == 2) {
						nonmiss = seq(1, nt, 2) #c(1, 3, 5, 7, 9, 11, 13) #vector of non-missing days
						nonmissall = matrix(nonmiss, np, length(nonmiss),byrow = T) #matrix has non-missing data points
						combine = nonmiss
					}
					##########################################################
					#Creating missingness for One-Day Interval Stagger Design#
					##########################################################
					if (design == 3) {
						nonmiss_1 = seq(1, nt, 2)
						nonmiss_2 = seq(2, nt, 2)
						nonmiss = matrix(nonmiss_1, (np/2), length(nonmiss_1), byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss_2, (np/2), length(nonmiss_2), byrow = T)
						nonmissall = rbind(nonmiss, nonmiss1)
						combine = c(nonmiss_1, nonmiss_2)
						combine = unique(combine)
						combine = sort(combine, na.last=NA)
					}
					##########################################################
					#Creating missingness for One-Day Interval Block Design#
					##########################################################
					if (design == 4) {
						nonmiss_1 = seq(1, nt/2, 2) #c(1,3,5,7)
						nonmiss_2 = seq(nt/2 + 1, nt, 2) #c(8,10,12,14)
						nonmiss = matrix(nonmiss_1, (np/2), length(nonmiss_1), byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss_2, (np/2), length(nonmiss_2), byrow = T)
						nonmissall = rbind(nonmiss,nonmiss1)
						combine = c(nonmiss_1, nonmiss_2)
						combine = unique(combine)
						combine = sort(combine,na.last=NA)
					}
					#################################################################
					#Creating missingness for Week,Staggered,No Overlap Block Design#
					#################################################################
					if (design == 5) {
						nonmiss_1 = c(1:(nt/2)) #c(1,2,3,4,5,6,7)
						nonmiss_2 = c((nt/2 + 1):nt) # c(8,9,10,11,12,13,14)
						nonmiss = matrix(nonmiss_1, (np/2), length(nonmiss_1), byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss_2, (np/2), length(nonmiss_2), byrow = T)
						nonmissall = rbind(nonmiss, nonmiss1)
						combine = c(nonmiss_1, nonmiss_2)
						combine = unique(combine)
						combine = sort(combine, na.last=NA)
						}
					#################################################################
					#Five-Day Staggered, (no?) Overlap Block Design#
					#################################################################
					if (design == 6) {
						nonmiss_1 = c(1,2,3,4,5,NA)
						nonmiss_2 = c(5,6,7,8,9,10)
						nonmiss_3 = c(10,11,12,13,14,NA)
						nonmiss = matrix(nonmiss_1,((np/3)+(np%%3)),length(nonmiss_1),byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss_2,(np/3),length(nonmiss_2),byrow = T)
						nonmiss2 = matrix(nonmiss_3,(np/3),length(nonmiss_3),byrow = T)
						nonmissall = rbind(nonmiss,nonmiss1,nonmiss2)
						combine = c(nonmiss_1,nonmiss_2,nonmiss_3)
						combine = unique(combine)
						combine = sort(combine,na.last=NA)
					}
					#################################################################
					#Five-Day Staggered, Two-Day Overlap Block Design#
					#################################################################
					if (design==7) {
						nonmiss.1 = c(1,2,3,4,5)
						nonmiss.2 = c(4,5,6,7,8)
						nonmiss.3 = c(7,8,9,10,11)
						nonmiss.4 = c(10,11,12,13,14)
						nonmiss = matrix(nonmiss.1,((np/4)+(np%%4)),length(nonmiss.1),byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss.2,(np/4),length(nonmiss.2),byrow = T)
						nonmiss2 = matrix(nonmiss.3,(np/4),length(nonmiss.3),byrow = T)
						nonmiss3 = matrix(nonmiss.4,(np/4),length(nonmiss.3),byrow = T)
						nonmissall = rbind(nonmiss,nonmiss1,nonmiss2,nonmiss3)
						combine = c(nonmiss.1,nonmiss.2,nonmiss.3,nonmiss.4)
						combine = unique(combine)
						combine = sort(combine,na.last=NA)
						}
					#################################################################
					#Skip the Weekend Design#
					#################################################################
					if (design==8) {
						nonmiss.1 = c(1,2,3,4,7,8,9,10,11,14)
						nonmissall = matrix(nonmiss.1,(np),length(nonmiss.1),byrow = T) #matrix has non-missing data points
						combine = nonmiss.1
						}
					#################################################################
					#Alternating Skip the Weekend Design#
					#################################################################
					if (design==9) {
						nonmiss.1 = c(1,3,7,9,11,14)
						nonmissall = matrix(nonmiss.1,(np),length(nonmiss.1),byrow = T) #matrix has non-missing data points
						combine = nonmiss.1
					}
					#################################################################
					#The Reference, Block design#
					#################################################################
					if (design==10) {
						nonmiss.1 = c(1,2,3,4,5,14,NA)
						nonmiss.2 = c(1,6,7,8,9,10,14)
						nonmiss.3 = c(1,11,12,13,14,NA,NA)
						nonmiss = matrix(nonmiss.1,((np/3)+(np%%3)),length(nonmiss.1),byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss.2,(np/3),length(nonmiss.2),byrow = T)
						nonmiss2 = matrix(nonmiss.3,(np/3),length(nonmiss.3),byrow = T)
						nonmissall = rbind(nonmiss,nonmiss1,nonmiss2)
						combine = c(nonmiss.1,nonmiss.2,nonmiss.3)
						combine = unique(combine)
						combine = sort(combine,na.last=NA)
						}
					#################################################################
					#Reference, Two-Day Overlap Block Design#
					#################################################################
					if (design==11) {
						nonmiss.1 = c(1,2,3,4,5,14,NA)
						nonmiss.2 = c(1,4,5,6,7,8,14)
						nonmiss.3 = c(1,7,8,9,10,11,14)
						nonmiss.4 = c(1,10,11,12,13,14,NA)
						nonmiss = matrix(nonmiss.1,((np/4)+(np%%4)),length(nonmiss.1),byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss.2,(np/4),length(nonmiss.2),byrow = T)
						nonmiss2 = matrix(nonmiss.3,(np/4),length(nonmiss.3),byrow = T)
						nonmiss3 = matrix(nonmiss.4,(np/4),length(nonmiss.3),byrow = T)
						nonmissall = rbind(nonmiss,nonmiss1,nonmiss2,nonmiss3)
						combine = c(nonmiss.1,nonmiss.2,nonmiss.3,nonmiss.4)
						combine = unique(combine)
						combine = sort(combine,na.last=NA)
					}
					#################################################################
					#Reference One-Day Interval Design                              #
					#################################################################
					if (design==12) {
						nonmiss.1 = c(1,2,3,5,7,9,11,13,14)
						nonmiss.2 = c(1,2,4,6,8,10,12,13,14)
						nonmiss = matrix(nonmiss.1,(np/2),length(nonmiss.1),byrow = T) #matrix has non-missing data points
						nonmiss1 = matrix(nonmiss.2,(np/2),length(nonmiss.2),byrow = T)
						nonmissall = rbind(nonmiss,nonmiss1)
						combine = c(nonmiss.1,nonmiss.2)
						combine = unique(combine)
						combine = sort(combine,na.last=NA)
						}
					#########################################################
					#################################################################
					#Randomly Sampled Design	                                  #
					#################################################################
					#if (design==13) {
						#nonmissall = NULL
						#for (m in 1:np){ 
							#samp = 1:14
							#nonmiss.1 = sample(samp,9)
							#nonmiss.1 = sort(nonmiss.1)
							#nonmissall = rbind(nonmissall,nonmiss.1)
						#}	
					#}

					if (design==13) {
						nonmissall = NULL
						combine = NULL
						for (m in 1:np){
							samp1 = c(1,3,5,7,9,11,13)
							nonmiss.1 = sample(samp1,1)
							nonmiss.1 = c(nonmiss.1,nonmiss.1+1)
							samp2 = 1:14
							samp3 = samp2[-(nonmiss.1)]
							nonmiss.2 = sample(samp3,3)
							nonmiss.3 = c(nonmiss.1,nonmiss.2)
							nonmiss.3 = sort(nonmiss.3)
							nonmissall = rbind(nonmissall,nonmiss.3)
							combine = c(combine,nonmiss.3)
							combine = unique(combine)
							combine = sort(combine,na.last=NA)
							}
						}
					#########################################################
					###################################################################
					#Two Consecutive Days Max With Number Data Points Equal to Random #
					###################################################################
					if (design==14) {
					  nonmiss.1 = c(1,2,3,5,7,9,11,13,14)
					  nonmiss.2 = c(1,2,4,6,8,10,12,13,14)
					  samp1 = sort(sample(nonmiss.1[3:7], 2))
					  samp2 = sort(sample(nonmiss.2[3:7], 2))
					  
					  nonmiss.1 = c(c(1,2), samp1, c(13, 14))
					  nonmiss.2 = c(c(1,2), samp2, c(13, 14))
					  nonmiss = matrix(nonmiss.1,(np/2),length(nonmiss.1),byrow = T) #matrix has non-missing data points
					  nonmiss1 = matrix(nonmiss.2,(np/2),length(nonmiss.2),byrow = T)
					  nonmissall = rbind(nonmiss,nonmiss1)
					  combine = c(nonmiss.1,nonmiss.2)
					  combine = unique(combine)
					  combine = sort(combine,na.last=NA)
					}
					#########################################################
					###################################################################
					#Random with 8 time points #
					###################################################################
					if (design==15) {
					  nonmissall = NULL
					  combine = NULL
					  for (m in 1:np){
					    samp1 = c(1,3,5,7,9,11,13)
					    nonmiss.1 = sample(samp1,1)
					    nonmiss.1 = c(nonmiss.1,nonmiss.1+1)
					    samp2 = 1:14
					    samp3 = samp2[-(nonmiss.1)]
					    nonmiss.2 = sample(samp3,6)
					    nonmiss.3 = c(nonmiss.1,nonmiss.2)
					    nonmiss.3 = sort(nonmiss.3)
					    nonmissall = rbind(nonmissall,nonmiss.3)
					    combine = c(combine,nonmiss.3)
					    combine = unique(combine)
					    combine = sort(combine,na.last=NA)
					  }
					}
					
					#########################################################
	
					yx=yall
					thedata1 = NULL
					for (j in 1:np){
						temp = nonmissall[j,]
						y2 = (yx[(1+(j-1)*nt):(j*nt),1]) #Taking the first person's observations
						y3 = y2[nonmissall[j,1:nobs(temp)]]   #Now taking just the values corresponding to nonmissing time points
						if (modelDG==2){
							x2 = (yx[(1+(j-1)*nt):(j*nt),2])
							x3 = x2[nonmissall[j,1:nobs(temp)]]
							yx4 = matrix(NA,nt,3)
							temp1 = nonmissall[j,1:nobs(temp)]
							yx4[temp1,1] = y3
							yx4[temp1,2] = x3
							yx4[,3] = j
							thedata1 = rbind(thedata1,yx4)
						}
						else{
							y4 = matrix(NA,nt,1)
							temp1 = nonmissall[j,1:nobs(temp)]
							y4[temp1,1] = y3
							thedata1 = rbind(thedata1,y4)
						}
					}
					#Transforming to wide format
					thedata1[is.na(thedata1)] = "."
					if (modelDG==1 | modelDG==3 | modelDG==4){
						Wide1 = NULL
						for (j in 1:np){
							personi = thedata1[(1+(j-1)*nt):(j*nt),1]
							Wide1 = rbind(Wide1,personi)
						}
						id = c(1:np)
						Wide1 = cbind(Wide1,id)
						#exporting
						write(t(Wide1),file=data,ncolumn=(nt+1),append=FALSE)
					}
					else {write(t(thedata1),file=data,ncolumn=(ny+2),append=FALSE)
						}
				
					#Here is where the data is estimated with a different model
					#if (length(theModelsToEstimate)>1){
						for (mest in 1:length(theModelsToEstimate)){
								print(paste(c(" iter ", run," N ", np, "D" ,d)))
								modelEst = theModelsToEstimate[mest]
								compile = paste(masterDir,"compile",nams[modelEst],"code.R",sep="") #r code for compiling Mplus code

								#getting the proper data formation if MLM is estimated
								if (modelEst==2){
									X=matrix(NA,nt*np,1)
									idk=matrix(NA,nt*np,1)
									for (k in 1:np){
										X[(1+(k-1)*nt):(nt*k),1]=runif(nt)
										idk[(1+(k-1)*nt):(nt*k),1]=k
										}
									thedata1=cbind(thedata1,X,idk)
									write(t(thedata1),file=data,ncolumn=(ny+2),append=FALSE)
									}
							
								#Opening Mplus
								source(paste(c(compile),sep="",collapse=""))
								system(paste(batch),wait = TRUE)
								file1 = outputmplus
								MCfile[length(theDesigns)*noMC*(mest-1)+d+(run-1)*length(theDesigns),1] = paste("DG",nams[modelDG],"EST",nams[modelEst],run,"D",d,"N",np,sep="")#theTs[t]
								MCfile[length(theDesigns)*noMC*(mest-1)+d+(run-1)*length(theDesigns),2] = paste("DG",nams[modelDG],sep="")#sampleSizes[s]
								MCfile[length(theDesigns)*noMC*(mest-1)+d+(run-1)*length(theDesigns),3] = paste("EST",nams[modelEst],sep="")#theDesigns[d]
								results = scan(file1,what="")
								#the following checks to see if there were errors and values were not saved
								output = outputfile
								output1 = scan(output,what="")
								output2 = output1[output1 %in% c("not", "terminate", "normally.")]
								if (length(output2)==3) results = rep(NA,length(results))
								output3 = output1[output1 %in% c("ERROR")]
								if (length(output3)==1) results = rep(NA,length(results))
								MCfile[length(theDesigns)*noMC*(mest-1)+d+(run-1)*length(theDesigns),4:(length(results)+3)] = results #Mplus gives 18 pieces of results for this particular model - varies depending on model and number of parameters estimated
					
								}
							
					}#d loop
				}# MC loop
			AllMC = rbind(AllMC,MCfile)
			write(t(MCfile),file = paste(c(resultsdir,"N",sampleSizes[s],"T",theTs[t],".txt"),sep="",collapse=""),ncol=dim(MCfile)[2],append=F)
			}# mo loop
		}# s loop
	}#t loop

#plot(1:nt,Wide1[1,1:nt],type="n")#,ylim=c(min(Wide1[,1:nt]),max(Wide1[,1:nt])))
#for (i in 1:np){
#	temp = Wide1[i,1:nt]
#	lines(1:nt,temp)
#	}

#write(t(AllMC),file = "C:/Documents and Settings/Diane Losardo/Desktop/DDpaper/newResults/LCM/final/ALLRESULTS.dat",ncol=dim(AllMC)[2],append=F)
