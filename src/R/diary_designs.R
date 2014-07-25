missing_info_for_designs <- function(design, nt, np){
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
    #nonmiss_1 <- 1:(floor(nt/3))
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
  return(list(nonmissall = nonmissall, combine = combine))
}