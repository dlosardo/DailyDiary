missing_info_for_designs <- function(design, nt, np){
  #################################################################
  ## Complete Case Data
  #################################################################
  if (design == "complete") {
    nonmiss = c(1:nt)
    nonmissall = matrix(nonmiss, np, length(nonmiss), byrow = T)
    combine = nonmiss
  }
  #################################################################
  ## Cohort Sequential Design One-Day Overlap
  #################################################################
  # 5.5
  if (design == "CSD_1dayOL") {
    #nonmiss_1 <- 1:(floor(nt/3))
    nonmiss_1 = c(1, 2, 3, 4, 5)
    nonmiss_2 = c(5, 6, 7, 8, 9, 10)
    nonmiss_3 = c(10, 11, 12, 13, 14)
    nonmiss = matrix(nonmiss_1,((np/3)+(np%%3)),length(nonmiss_1),byrow = T) #matrix has non-missing data points
    nonmiss1 = matrix(nonmiss_2,(np/3),length(nonmiss_2),byrow = T)
    nonmiss2 = matrix(nonmiss_3,(np/3),length(nonmiss_3),byrow = T)
    nonmissall = rbind.fill.matrix(nonmiss,nonmiss1,nonmiss2)
    combine = c(nonmiss_1,nonmiss_2,nonmiss_3)
    combine = unique(combine)
    combine = sort(combine,na.last=NA)
  }
  #################################################################
  ## Cohort Sequential Design Two-Day Overlap
  #################################################################
  # 5
  if (design == "CSD_2dayOL") {
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
  ## Reference Day 
  #################################################################
  # 6.5
  if (design == "reference") {
    nonmiss.1 = c(1,2,3,4,5,14)
    nonmiss.2 = c(1,4,5,6,7,8,14)
    nonmiss.3 = c(1,7,8,9,10,11,14)
    nonmiss.4 = c(1,10,11,12,13,14)
    nonmiss = matrix(nonmiss.1,((np/4)+(np%%4)),length(nonmiss.1),byrow = T) #matrix has non-missing data points
    nonmiss1 = matrix(nonmiss.2,(np/4),length(nonmiss.2),byrow = T)
    nonmiss2 = matrix(nonmiss.3,(np/4),length(nonmiss.3),byrow = T)
    nonmiss3 = matrix(nonmiss.4,(np/4),length(nonmiss.4),byrow = T)
    nonmissall = rbind.fill.matrix(nonmiss,nonmiss1,nonmiss2,nonmiss3)
    combine = c(nonmiss.1,nonmiss.2,nonmiss.3,nonmiss.4)
    combine = unique(combine)
    combine = sort(combine,na.last=NA)
  }
  #################################################################
  # Two Consecutive Days Max                              
  #################################################################
  # 8
  if (design == "TCDM_8") {
    nonmiss.1 = c(1,3,5,7,9,11,13,14)
    nonmiss.2 = c(1,2,4,6,8,10,12,14)
    nonmiss = matrix(nonmiss.1, (np/2), length(nonmiss.1), byrow = T) #matrix has non-missing data points
    nonmiss1 = matrix(nonmiss.2, (np/2), length(nonmiss.2), byrow = T)
    nonmissall = rbind(nonmiss, nonmiss1)
    combine = c(nonmiss.1, nonmiss.2)
    combine = unique(combine)
    combine = sort(combine, na.last=NA)
  }
  #########################################################
  #################################################################
  # Randomly Sampled Design	                                  #
  #################################################################
  # 5
  if (design == "random_5") {
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
  ###################################################################
  #Two Consecutive Days Max With Number Data Points Equal to Random #
  ###################################################################
  # 5
  if (design == "TCDM_5") {
    nonmiss.1 = c(1,3,5,7,9,11,13,14)
    nonmiss.2 = c(1,2,4,6,8,10,12,14)
    samp1 = sort(sample(nonmiss.1[2:(length(nonmiss.1) - 1)], 2))
    samp2 = sort(sample(nonmiss.2[2:(length(nonmiss.1) - 1)], 2))
    
    nonmiss.1 = c(c(1), samp1, c(13, 14))
    nonmiss.2 = c(c(1, 2), samp2, c(14))
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
  if (design == "random_8") {
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