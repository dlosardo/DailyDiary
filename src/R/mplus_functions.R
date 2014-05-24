# TODO: Add comment
# 
# Author: dlosardo
###############################################################################
#####################
# General Functions #
#####################
#Creating shell file
create_shell_script = function(name, cd, dir_in, dir_out, file){
  cat(paste("#!/bin/sh
  						
cd ", cd, "\n
/Applications/Mplus/mplus ", dir_in, "/", name, ".inp > ", dir_out, "/", name, ".out"
            , sep = ""), file = file, append = FALSE)
}

# Trims whitespace from front and end of character field
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Calculates the number of variables given a vector of unique elements in a covariance matrix
get_num_vars <- function(x){
  (sqrt(8 * x + 1) - 1)/2
}

# inputs a matrix and a vector to order each column of the matrix.
# outputs the matrix in the order specified by the input variable order.
changeOrder <- function(matrix, order){
  for (i in 1:ncol(matrix)){
    matrix[,i] <- matrix[,i][order]
  }
  return(matrix)
}

# Calculates the number of unique elements in a covaraince matrix
# inputs: n: a numeric value representing number of variables in covaraince matrix
# returns: a numeric value representing number of unique elements in a covariance matrix
getNumUniqueElementsInCovMatrix = function(n){
  return((n*(n+1))/2)
}

getFromFrontOfVector = function(vec, n){
  return(vec[c(1:n)])
}
removeFromFrontOfVector = function(vec, n){
  return(vec[-c(1:n)])
}

reformatMatrices = function(matrices, varNames, factorNames, ny, nf){
  names(matrices$tau) <- varNames
  names(matrices$nu) <- varNames
  matrices$lambda <- matrix(matrices$lambda, ny, nf, byrow = TRUE, dimnames = list(varNames, factorNames))
  matrices$theta <- transformListOfCorrsToMatrix(list(matrices$theta))[[1]]
  dimnames(matrices$theta) <- list(varNames, varNames)
  matrices$alpha <- matrix(matrices$alpha, nf, 1, byrow = TRUE, dimnames = list(factorNames, NULL))
  matrices$beta <- matrix(matrices$beta, nf, nf, byrow = TRUE, dimnames = list(factorNames, factorNames))
  matrices$psi <- transformListOfCorrsToMatrix(list(matrices$psi))[[1]]
  dimnames(matrices$psi) <- list(factorNames, factorNames)
  return(matrices)
}

getEstimatesInMatrixForm <- function(info, estimates, output, varNames, factorNames, ny, nf){
  matrices <- list()
  for (i in 1:length(info[[1]])){
    matrices[[info[[1]][i]]] <- getFromFrontOfVector(estimates, info[[2]][i])
    estimates <- removeFromFrontOfVector(estimates, info[[2]][i])
  }
  # get variable names
  varNames <- getVarNames(output)
  # create factor names (conventionally f1, f2, etc.)
  factorNames <- sprintf("f%s", 1:nf)
  matrices <- reformatMatrices(matrices, varNames, factorNames, ny, nf)
  return(matrices)
}

############################################################
# GENERAL MPLUS FUNCTION FOR GETTING PIECES OF INFORMATION #
############################################################

# Given an Mplus .out file, returns a list of results that are #
# stored after the parameter estimates and standard errors #
getResultsAfterParms = function(output){
  findOrder = "RESULTS SAVING INFORMATION"
  findOrderEnd = "Save file"
  outputLine = grep(findOrder,output)
  outputLineEnd = grep(findOrderEnd,output)
  if (length(outputLineEnd) < 1){
    return(NULL)
  }
  orderResults = output[outputLine:(outputLineEnd[1]-1)]
  return(sub("^\\s+", "", orderResults[7:length(orderResults)]))
}

# Getting order of parameter estimates as displayed in results output
getTech1Output = function(output, whichMatrix){
  techStart="TECHNICAL 1 OUTPUT"
  techEnd="STARTING VALUES"
  techOutputStart=grep(techStart,output)
  techOutputEnd=grep(techEnd,output)
  outputTECH=output[techOutputStart:(techOutputEnd-1)]
  
  tau="TAU"
  nu="NU"
  lambda="LAMBDA"
  theta="THETA"
  alpha="ALPHA"
  beta="BETA"
  psi="PSI"
  
  tauStart=grep(tau,outputTECH)[1]
  nuStart=grep(nu,outputTECH)[1]
  lambdaStart=grep(lambda,outputTECH)[1]
  thetaStart=grep(theta,outputTECH)[1]
  alphaStart=grep(alpha,outputTECH)[1]
  betaStart=grep(beta,outputTECH)[1]
  psiStart=grep(psi,outputTECH)[1]
  
  if (whichMatrix == "tau") return(outputTECH[tauStart:(nuStart-1)])
  if (whichMatrix == "nu") return(outputTECH[nuStart:(lambdaStart-1)])
  if (whichMatrix == "lambda") return(outputTECH[lambdaStart:(thetaStart-1)])
  if (whichMatrix == "theta") return(outputTECH[thetaStart:(alphaStart-1)])
  if (whichMatrix == "alpha") return(outputTECH[alphaStart:(betaStart-1)])
  if (whichMatrix == "beta") return(outputTECH[betaStart:(psiStart-1)])
  if (whichMatrix == "psi") return(outputTECH[psiStart:length(outputTECH)])
  return(NULL)
}

# Gets the number of observations given an Mplus .out file #
getNumberObservations = function(output){
  nObs = "Number of observations"
  nObsMatch = grep(nObs,output)
  nObsOutput = output[nObsMatch[1]]
  startNObs = unlist(gregexpr("[0-9]+",nObsOutput))
  return(as.numeric(substr(nObsOutput,startNObs,nchar(nObsOutput))))
}

# Gets the number of free parameters given an Mplus .out file #
getNumberFreeParms = function(output){
  freeParms="Number of Free Parameters"
  newParms <- "PARAMETER SPECIFICATION FOR THE ADDITIONAL PARAMETERS"
  freeParmsMatch=grep(freeParms,output)
  freeParmsOutput=output[freeParmsMatch[1]]
  startNfreeParms=unlist(gregexpr("[0-9]+",freeParmsOutput))
  numFreeParms <- as.numeric(substr(freeParmsOutput,startNfreeParms,nchar(freeParmsOutput)))
  if (any(grepl(newParms, output))){
    newParmsEnd <- "STARTING VALUES FOR THE ADDITIONAL PARAMETERS"
    newParmsOutput <- output[grep(newParms, output):grep(newParmsEnd, output)]
    newParmsTemp <- grep("NEW/ADDITIONAL PARAMETERS", newParmsOutput)
    newParmsLastLine <- grep("[0-9]+",unlist(strsplit(newParmsOutput[newParmsTemp[length(newParmsTemp)] + 3], " ")), value=T)
    numFreeParms <- as.numeric(newParmsLastLine[length(newParmsLastLine)])
  }
  return(numFreeParms)
}

# Gets the number of dependent variables in model #
getNumberDependentVars = function(output){
  depVars = "Number of dependent variables"
  depVarsMatch = grep(depVars, output)
  depVarsLine = output[depVarsMatch]
  tokenized <- unlist(strsplit(depVarsLine, " "))
  depVarsNum <- tokenized[grep("[0-9]+", tokenized)]
  return(as.numeric(depVarsNum))
}

# Gets the number of independent variables in model #
getNumberIndependentVars = function(output){
  indepVars = "Number of independent variables"
  indepVarsMatch = grep(indepVars, output)
  indepVarsLine = output[indepVarsMatch]
  tokenized <- unlist(strsplit(indepVarsLine, " "))
  indepVarsNum <- tokenized[grep("[0-9]+", tokenized)]
  return(as.numeric(indepVarsNum))
}

# Gets the number of categorical latent variables in model #
getNumberCatLVs = function(output){
  catLVs = "Number of categorical latent variables"
  if (length(grep(catLVs, output)) == 0){
    return(0)
  }
  catLVsMatch = grep(catLVs, output)
  catLVsLine = output[catLVsMatch]
  tokenized <- unlist(strsplit(catLVsLine, " "))
  catLVsNum <- tokenized[grep("[0-9]+", tokenized)]
  return(as.numeric(catLVsNum))
}

# Gets the number of independent variables in model #
getNumberConLVs = function(output){
  conLVs = "Number of continuous latent variables"
  conLVsMatch = grep(conLVs, output)
  conLVsLine = output[conLVsMatch]
  tokenized <- unlist(strsplit(conLVsLine, " "))
  conLVsNum <- tokenized[grep("[0-9]+", tokenized)]
  return(as.numeric(conLVsNum))
}

getAuxiliaryVarNames <- function(output){
  vars = "Observed auxiliary variables" 
  varsMatch = grep(vars, output) 
  varsEnd = "Continuous latent variables"
  varsEndMatch = grep(varsEnd, output)
  if (length(varsEndMatch)==0){
    varsEndMatch = grep("Categorical latent variables", output)
  }
  varsLine = output[(varsMatch + 1):(varsEndMatch - 1)]
  tokenized <- unlist(strsplit(varsLine, " "))
  auxVars <- tokenized[grep("[a-zA-Z0-9]+", tokenized)]
  return(auxVars)
}

# Gets a vector of observed variable names #
getVarNames <- function(output, is_auxs_exist = TRUE){
  vars_cats = "Binary and ordered categorical" 
  vars_nominal = "Unordered categorical \\(nominal\\)"
  varsMatch_cats = grep(vars_cats, output) 
  varsMatch_nominal = grep(vars_nominal, output)
  varsMatch = ifelse(length(varsMatch_cats) == 0, varsMatch_nominal, varsMatch_cats)
  if(is_auxs_exist){
    varsEnd = "Observed auxiliary variables"
  } else {
    varsEnd = "Continuous latent variables"
    if (length(grep(varsEnd, output)) == 0){
      varsEnd = "Categorical latent variables"
    }
  }
  varsEndMatch = grep(varsEnd, output)
  varsLine = output[(varsMatch + 1):(varsEndMatch - 1)]
  tokenized <- unlist(strsplit(varsLine, " "))
  allVars <- tokenized[grep("[a-zA-Z0-9]+", tokenized)]
  return(allVars)
}

# Given an Mplus model, returns fit statistics #
getFitStatistics <- function(output, results){
  resultsAfterParms <- getResultsAfterParms(output)
  if (is.null(resultsAfterParms)){
    return(NULL)
  }
  nFreeParms <- getNumberFreeParms(output)
  nObs <- getNumberObservations(output)    
  #Length of results output
  lengthOutputFile = (as.numeric(nFreeParms) * 2) + length(resultsAfterParms)
  results = data.frame(matrix(results, 1, lengthOutputFile, byrow = TRUE))  
  fitStats = results[(nFreeParms * 2 + 1):length(results)]
  names(fitStats) <- resultsAfterParms
  return(fitStats)
}

# Gets a vector of factor variable names #
getFactorNames <- function(output){
  fVars = "Continuous latent variables" 
  fVarsMatch = grep(fVars, output) 
  if (length(fVarsMatch) == 0){
    fVarsMatch = grep("Categorical latent variables", output)
  }
  fVarsEnd = "Estimator"
  fVarsEndMatch = grep(fVarsEnd, output)
  fVarsLine = output[(fVarsMatch + 1):(fVarsEndMatch - 1)]
  tokenized <- unlist(strsplit(fVarsLine, " "))
  allFVars <- tokenized[grep("[a-zA-Z0-9]+", tokenized)]
  return(allFVars)
}

# Gets the estimator used #
getEstimator = function(output){
  est = "Estimator"
  estMatch = grep(est, output)
  estLine = output[estMatch[1]]
  tokenized <- unlist(strsplit(estLine, " "))
  return(tokenized[length(tokenized)])
}

# loads an mplus file in with the .gh5 extension (from mplus website)
mplus.load <- function(file) {
  require(hdf5, quietly=TRUE)
  if (file.exists(file)) {
    hdf5load(file, load=TRUE)    
    if (exists("process_data", mode="list") && exists("means_and_variances_data", mode="list")) {
      np <- attr(process_data,"num_processes")
      for (i in c(1:np)) {
        cstr <- paste(c("process"), as.character(i), sep="")
        proc <- process_data[[cstr]]
        prop <- attr(proc,"properties")
        values <- attr(means_and_variances_data,"names")
        if (prop[1] == 1) {
          sm_ind <- pmatch("y_observed_means",values,nomatch=0)
          em_ind <- pmatch("y_estimated_means",values,nomatch=0)
        } else if (prop[1] == 2) {
          em_ind <- pmatch("e_estimated_means",values,nomatch=0)
        } else if (prop[1] == 3) {
          em_ind <- pmatch("observed_probs",values,nomatch=0)
          em_ind <- pmatch("estimated_probs",values,nomatch=0)
        }
      }
    } else {
      cstr <- paste("File does not exist:",file,"\n")
      cat(cstr)
    }
  }
}

##############################
###### LONGITUDINAL IRT ######
##############################
# returns a list of point estimates associated with a longitudinal IRT with two time points (i.e., two factors)
# model: can be Rasch, 1PL, or 2PL
# output: the contents of the corresponding .out Mplus file
# results: the contents of the corresponding results Mplus file
# resultsOrder: the order of the results NOT including common items across time points
# commonNames: vector of the names of the common items
getPointEstimatesLongIRT <- function(model, output, results, resultsOrder, commonNames, is_one_lv = FALSE){
  estimateList <- list()
  # Rasch
  if (model == "Rasch"){
    if (!is_one_lv){
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("corr", commonNames, sep = ""), "meanF2", "covF1F2", "varF2")
    } else {
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("corr", commonNames, sep = ""))
    }
  }
  # 1PL
  else if (model == "1PL"){
    if (!is_one_lv){
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), "disc", paste("corr", commonNames, sep = ""), "meanF2", "covF1F2", "varF2")
    } else {
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), "disc", paste("corr", commonNames, sep = ""))
    }
  }
  # 2PL
  else if (model == "2PL"){
    if (!is_one_lv){
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("disc", resultsOrder, sep=""), paste("corr", commonNames, sep = ""), "meanF2", "covF1F2", "varF2")
    } else {
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("disc", resultsOrder, sep=""), paste("corr", commonNames, sep = ""))
    }
  }
  nItems <- length(resultsOrder)
  nCommonItems <- length(commonNames)
  resultsAfterParms <- getResultsAfterParms(output)
  if (is.null(resultsAfterParms)){
    return(NULL)
  }
  nFreeParms <- getNumberFreeParms(output)
  #Length of results output
  lengthOutputFile = (as.numeric(nFreeParms) * 2) + length(resultsAfterParms)
  outputOrder <- c(parmsOrder, paste("SE_", parmsOrder, sep=""), resultsAfterParms)
  
  results = data.frame(matrix(results, 1, lengthOutputFile, byrow = TRUE))
  names(results) <- outputOrder
  ##### FOR RASCH #######
  if (model == "Rasch"){
    estimateList$b <- unlist(results[1:nItems])
    estimateList$a <- rep(1, nItems)
    if (is_one_lv){
      estimateList$b <- estimateList$b/estimateList$a
    }
    estimateList$c <- rep(0, nItems)
    estimateList$resCovs <- results[grep("corr", names(results))][1:nCommonItems]
    if (!is_one_lv){
      estimateList$meanF2 <- results["meanF2"]
      estimateList$varF2 <- results["varF2"]
      estimateList$covF1F2 <- results["covF1F2"]
    }
  }
  else if (model == "2PL"){
    #### FOR 2PL #####
    estimateList$b <- unlist(results[1:nItems])
    estimateList$a <- unlist(results[(nItems+1):(nItems*2)])
    if (is_one_lv){
      estimateList$b <- estimateList$b/estimateList$a
    }
    estimateList$c <- rep(0, nCommonItems)
    estimateList$resCovs <- results[grep("corr", names(results))][1:nCommonItems]
    if (!is_one_lv){
      estimateList$meanF2 <- results["meanF2"]
      estimateList$varF2 <- results["varF2"]
      estimateList$covF1F2 <- results["covF1F2"]
    }
  }
  else if (model == "1PL"){
    ########### 1 PL########
    estimateList$b <- unlist(results[1:nItems])
    estimateList$a <- unlist(results[nItems+1])
    if (is_one_lv){
      estimateList$b <- estimateList$b/estimateList$a
    }
    estimateList$c <- rep(0, nItems)
    estimateList$resCovs <- results[grep("corr", names(results))][1:nCommonItems]
    if (!is_one_lv){
      estimateList$meanF2 <- results["meanF2"]
      estimateList$varF2 <- results["varF2"]
      estimateList$covF1F2 <- results["covF1F2"]
    }
  }
  return(estimateList)
}

# Given a longitudinal IRT model, returns SEs #
# model may be equal to Rasch, 1PL, or 2PL #
getSEsLongIRT = function(model, output, results, resultsOrder, commonNames, is_one_lv = FALSE){
  resultsAfterParms <- getResultsAfterParms(output)
  nFreeParms <- getNumberFreeParms(output)
  nObs <- getNumberObservations(output)
  ny <- getNumberDependentVars(output) + getNumberIndependentVars(output)
  obsVars <- getVarNames(output)
  nItems <- length(obsVars)
  estimateList <- list()
  # Rasch
  if (model == "Rasch"){
    if (!is_one_lv){
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("corr", commonNames, sep = ""), "meanF2", "covF1F2", "varF2")
    } else {
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("corr", commonNames, sep = ""))
    }
  }
  # 1PL
  else if (model == "1PL"){
    if (!is_one_lv){
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), "disc", paste("corr", commonNames, sep = ""), "meanF2", "covF1F2", "varF2")
    } else {
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), "disc", paste("corr", commonNames, sep = ""))
    }
  }
  # 2PL
  else if (model == "2PL"){
    if (!is_one_lv){
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("disc", resultsOrder, sep=""), paste("corr", commonNames, sep = ""), "meanF2", "covF1F2", "varF2")
    } else {
      parmsOrder <- c(paste("diff", resultsOrder, sep=""), paste("disc", resultsOrder, sep=""), paste("corr", commonNames, sep = ""))
    }  
  }
  nParms <- length(commonNames)
  outputOrder <- c(parmsOrder, paste("SE_", parmsOrder, sep=""), resultsAfterParms)
  
  #Length of results output
  lengthOutputFile = (as.numeric(nFreeParms) * 2) + length(resultsAfterParms)
  
  results = data.frame(matrix(results, 1, lengthOutputFile, byrow=TRUE))
  names(results) <- outputOrder
  ##### FOR RASCH #######
  if (model == "Rasch"){
    ses <- unlist(results[(nFreeParms + 1):(nFreeParms * 2)])
    if (is_one_lv){
      a <- rep(0, nItems)
      b <- ses
      return(list(a, b))
    } else {
      return(ses)
    }
  }
  else if (model == "2PL"){
    #### FOR 2PL #####
    ses <- unlist(results[(nFreeParms + 1):(nFreeParms * 2)])
    if (is_one_lv){      
      a <- ses[grep("disc", names(ses))]
      b <- ses[grep("diff", names(ses))]
      return(list(a, b))
    } else {
      return(ses)
    }
  }
  else if (model == "1PL"){
    ########### 1 PL########
    ses <- unlist(results[(nFreeParms + 1):(nFreeParms * 2)])
    if (is_one_lv){
      a <- rep(ses[1], nItems)
      b <- ses[2:length(ses)]
      return(list(a, b))
    } else {
      return(ses)
    }
  }
  return(list(a, b))
}
#################
###### EFA ######
#################
# inputs a list of vectors of numbers to be transformed into a matrix.
# The numbers are in the order starting from left to right and then top to bottom on the lower triangle (including diagonal)
# example: a 3x3 matrix would have the values in the order:
#   1. [1,1] 2. [2,1] 3. [2,2] 4. [3,1] 5. [3,2] 6. [3,3]
# outputs a list of matrices 
transformListOfCorrsToMatrix = function(asList){
  matrixList <- list()
  for (i in 1:length(asList)){
    temp <- asList[[i]]
    nVars <- (sqrt(8 * length(temp) + 1) - 1)/2
    corrMatrix <- matrix(0, nVars, nVars)
    count <- 1
    indexFront <- 1
    indexEnd <- 1
    for (p in 1:nVars){
      tempP <- p * (p + 1)/2
      corrMatrix[p, 1:count] <- temp[indexFront:indexEnd]
      count = count + 1
      indexFront <- indexEnd + 1
      indexEnd <- indexEnd + count
    }
    tempDiag <- diag(corrMatrix)
    corrMatrix <- corrMatrix + t(corrMatrix)
    diag(corrMatrix) <- tempDiag
    matrixList[[i]] <- corrMatrix
  }
  return(matrixList)
}

# returns a list of factor loadings. Each list index corresponds to a number
# of factors estimated for a given model.
# model is the name of the output file (including .out)
# nModels is the number of models estimated
getLoadingsEFA = function(output, nModels){
  nItems <- getNumberDependentVars(output)
  findOrder = "ROTATED LOADINGS"
  outputLine = grep(findOrder, output)
  forLoadings <- outputLine[seq(1, length(outputLine), 3)]
  loadings <- NULL
  for (m in 1:nModels){
    for (i in 1:(nItems)){
      if (m >= 6){
        tokenized <- strsplit(output[forLoadings[m] + 3 + (i - 1)], " ")[[1]]
        loadings <- c(loadings, tokenized[grep("^[-0-9]+", tokenized)])
        tokenized <- strsplit(output[outputLine[3 * (m-1) + 2] + 3 + (i - 1)], " ")[[1]]
        loadings <- c(loadings, tokenized[grep("^[-0-9]+", tokenized)])
      } else {
        tokenized <- strsplit(output[forLoadings[m] + 3 + (i - 1)], " ")[[1]]
        loadings <- c(loadings, tokenized[grep("^[-0-9]+", tokenized)])
      }
    }
  }
  loadings <- gsub("[*]", "", loadings)  
  indexStart <- 1
  indexEnd <- 0
  listAllLoadings <- list()
  for (m in 1:nModels){
    toAddStart <- (m - 1) * nItems
    indexStart <- indexStart + toAddStart
    toAddEnd <- nItems * m
    indexEnd <- indexEnd + toAddEnd
    temp <- loadings[indexStart:indexEnd]
    allLoadings <- NULL
    for (i in 1:m){
      allLoadings <- cbind(allLoadings, temp[seq(i, length(temp), m)])
    }
    allLoadings <- apply(allLoadings, 2, as.numeric)
    listAllLoadings[[m]] <- allLoadings
  }
  return(listAllLoadings)
}

# gets fit statistics when using a FIML model for EFA
getFitStatisticsEFA <- function(output, nModels){
  est <- getEstimator(output)
  numNoConv = NULL
  isNonConv <- "NO CONVERGENCE"
  convLine <- grep(isNonConv, output)
  if (length(convLine) > 0){
    noConv <- "PROBLEM OCCURRED IN EXPLORATORY FACTOR ANALYSIS"
    convLine <- grep(noConv, output)
    convToken <- unlist(strsplit(output[convLine], " "))
    numNoConv <- as.numeric(convToken[grep("^[-0-9]+", convToken)])
  }
  allFits <- list()
  if (est == "MLR" | est == "ML"){
    findOrder = "MODEL FIT INFORMATION"
    outputLine = grep(findOrder, output)
    for (m in 1:nModels){
      if (m %in% numNoConv){
        for (i in nModels:m){
          outputLine[i] <- outputLine[i - 1]
        }
        temp <- rep(NA, 4)
        names(temp) <- c("H0 Value", "AIC", "BIC", "NadjBIC")
        allFits[[m]] <- temp
        next
      } 
      fitStats <- output[(outputLine[m]+3):(outputLine[m]+9)][c(1, 5:7)]
      if (!grepl("H0 Value", fitStats[1])){
        allFits[[m]] <- rep(NA, 4)
        next
      }
      tokenized <- unlist(strsplit(fitStats, " "))
      fitStats1 <- tokenized[grep("^[-0-9]+", tokenized)]
      fitStats1 <- as.numeric(fitStats1)
      names(fitStats1) <- c("H0 Value", "AIC", "BIC", "NadjBIC")
      allFits[[m]] <- fitStats1
    }
  }
  if (est == "WLSMV"){
    findOrder = "Root Mean Square Error Of Approximation"
    outputLine = grep(findOrder, output)
    for (m in 1:nModels){
      if (m %in% numNoConv){
        for (i in nModels:m){
          outputLine[i] <- outputLine[i - 1]
        }
        temp <- rep(NA, 5)
        names(temp) <- c("RMSEA", "LowerCIRMSEA", "UpperCIRMSEA", "CFI", "TLI")
        allFits[[m]] <- temp
        next
      } 
      fitStats <- output[(outputLine[m]+1):(outputLine[m]+7)][c(1:2, 5:6)]
      tokenized <- unlist(strsplit(fitStats, " "))
      fitStats1 <- as.numeric(tokenized[grep("^[-0-9]+", tokenized)])[c(1,3:6)]
      names(fitStats1) <- c("RMSEA", "LowerCIRMSEA", "UpperCIRMSEA", "CFI", "TLI")
      allFits[[m]] <- fitStats1
    }
  }
  return(allFits)
}

# get eigenvalues when running an EFA
getEigenvaluesEFA <- function(output){
  nItems <- getNumberDependentVars(output)
  findOrder = "EIGENVALUES"
  outputLine = grep(findOrder, output)
  allValues <- NULL
  for (i in 1:length(outputLine)){
    temp <- output[outputLine[i] + 3]
    tokenized <- unlist(strsplit(temp, " "))
    values <- tokenized[grep("^[-0-9]+", tokenized)]
    allValues <- c(allValues, values[2:length(values)])
  }
  return(as.numeric(allValues))
}

# get correlations among factors when running an EFA
getCorrelationsEFA <- function(output, nModels){
  numNoConv = NULL
  isNonConv <- "NO CONVERGENCE"
  convLine <- grep(isNonConv, output)
  if (length(convLine) > 0){
    noConv <- "PROBLEM OCCURRED IN EXPLORATORY FACTOR ANALYSIS"
    convLine <- grep(noConv, output)
    convToken <- unlist(strsplit(output[convLine], " "))
    numNoConv <- as.numeric(convToken[grep("^[-0-9]+", convToken)])
  }
  findOrder <- "FACTOR CORRELATIONS"
  outputLine <- grep(findOrder, output)
  forCorrs <- outputLine[seq(1, length(outputLine), 3)]
  allCorrs <- list()
  for (m in 1:nModels){
    if (m %in% numNoConv){
      for (i in nModels:m){
        forCorrs[i] <- forCorrs[i - 1]
      }
      allCorrs[[m]] <- rep(NA, m - 1)
      next
    } 
    if (m >= 6){
      corrsTemp <- output[(forCorrs[m] + 3):(forCorrs[m] + 3 + m - 1 )]
      tokenized <- unlist(strsplit(corrsTemp, " "))
      values <- tokenized[grep("[.]", tokenized)]
      corrsTemp <- output[((forCorrs[m] + m + 3) + 3):((forCorrs[m] + m + 3) + 3 + m - 6 )]
      tokenized <- unlist(strsplit(corrsTemp, " "))
      values <- c(values, tokenized[grep("[.]", tokenized)])
    } else { 
      corrsTemp <- output[(forCorrs[m] + 3):(forCorrs[m] + 3 + m - 1 )]
      tokenized <- unlist(strsplit(corrsTemp, " "))
      values <- tokenized[grep("[.]", tokenized)]
    }
    values <- gsub("[*]", "", values)  
    values <- as.numeric(values)
    allCorrs[[m]] <- values
  }
  return(allCorrs)
}

###########################
###### FACTOR SCORES ######
###########################

# Determining whether Mplus outputted the SEs of the factor scores in the #
# Factor Score file. Input is an Mplus .out file #
hasSEForFactors = function(output){
  findVars = "Order and format of variables"
  findVarsEnd = "Save file format"
  outputLine = grep(findVars, output)
  outputLineEnd = grep(findVarsEnd, output)
  allVars = output[outputLine:(outputLineEnd[length(outputLineEnd)])]
  return(any(grepl("_SE", allVars)))
}

#Given the name of a model, returns factor scores using the .gh5 file #
getFactorScoresFromGh5 = function(currentModel){
	mplus.load(paste(PREPPED, "/", currentModel, ".gh5", sep=""))
	ny <- attr(individual_data, "n_y")
	nf <- attr(individual_data, "n_fscores")
	varNames <- trim(attr(individual_data, "var_names"))
	obsVars <- trim(varNames[1:ny])
	factors <- trim(varNames[(ny+1):length(varNames)])
	fscores <- mplus.get.data(factors)	
	return(fscores)
}

# Returns factor scores given an fscores dataset #
# outputted by Mplus, the number of factors, and #
# whether or not standard errors were included in #
# the given dataset #
getFactorScores <- function(data, nf, output){
  scores <- NULL
  if (hasSEForFactors(output)){
    count <- 1
    for (i in 1:nf){
      temp <- data[,ncol(data) - count]
      count = count + 2
      scores = cbind(temp, scores)
    }  
  } else{
    count <- 0
    for (i in 1:nf){
      temp <- data[,ncol(data) - count]
      count = count + 1
      scores = cbind(temp, scores)
    }
  }
  colnames(scores) <- sprintf("FSCORES%d", 1:nf)
  return(scores)
}

getIdOrderForFscores = function(data, nf, output, nAuxs){
  id <- NULL
  if (hasSEForFactors(output)){
    ids <- data[, (ncol(data) - (nAuxs) -nf):(ncol(data) - (nf*2))]  
    if (nAuxs == 1){
      ids <- as.data.frame(ids)
    }
  } else{
    ids <- data[, (ncol(data) - (nAuxs - 1) - nf):(ncol(data) - nf)]
    if (nAuxs == 1){
      ids <- as.data.frame(ids)
    }
  }
  names(ids) <- getAuxiliaryVarNames(output)
  return(ids)
  
}
# Gets SEs of factor scores given an fscores dataset #
# outputted by Mplus, the number of factors, and #
# whether or not standard errors were included in #
# the given dataset #
getFactorScoresSEs <- function(data, nf, output){
  if (!hasSEForFactors(output)){
    return(NULL)
  } else{
    ses <- NULL
    count <- 0
    for (i in 1:nf){
      temp <- data[,ncol(data) - count]
      count = count + 2
      ses = cbind(temp, ses)
    }
    return(ses)
  }
}

#################################
###### CLASS PROBABILITIES ######
#################################

# Gets the class probabilities 
# inputs: data: a dataframe of a Mplus savedata file containing class probabilities
#         n_classes: a numeric value representing number of classes
# returns: an n_students by n_classes dataframe of posterior probabilities for each class for each student 
getClassProbs <- function(data, n_classes){
  return(data[, (ncol(data) - n_classes):(ncol(data) - 1)])
}
# Gets the auxillary variables from a class probabilities Mplus results output
# inputs: data: a dataframe of a Mplus savedata file containing class probabilities
#         output: a String vector of an Mplus .out file
# returns: a dataframe of auxillary variables 
getAuxVarsFromClassProbsResults <- function(data, output){
  nVars <- length(getVarNames(output))
  auxNames <- getAuxiliaryVarNames(output)
  nAuxVars <- length(auxNames)
  auxVars <- data.frame(data[, nVars+nAuxVars])
  names(auxVars) <- auxNames
  return(auxVars)
}
# Gets the most likely class category from a class probabilities Mplus results output
# inputs: data: a dataframe of a Mplus savedata file containing class probabilities
# returns: a dataframe of the most likely class category based on posterior probabilities
getMostLikelyClassCategories <- function(data){
  return(data.frame(class = data[, ncol(data)]))
}

#################
###### DCM ######
#################
# Creates a dataframe that contains skill probabilities and class assignment for students
# inputs: pmatrix: a n_classes by n_attributes matrix of all possible attribute profiles\
#         output: a String vector of an Mplus .out file
#         cprobs: a dataframe of a Mplus savedata file containing class probabilities
#         attribute_names: a String vector of the names of the attributes
# results: an n_student by (number auxilery vars + n_attributes + 1) dataframe of:
#           auxillery variables used in mplus model, probability of mastery of each attribute, class assignment
get_skill_mastery_probs <- function(pmatrix, output, cprobs, attribute_names){
  n_classes <- 2^ncol(pmatrix)
  cprobs_only <- getClassProbs(cprobs, n_classes)
  skill_probs <- as.matrix(cprobs_only) %*% pmatrix
  skill_probs <- cbind(getAuxVarsFromClassProbsResults(cprobs, output)
                       , skill_probs
                       , getMostLikelyClassCategories(cprobs))
  colnames(skill_probs) <- c(getAuxiliaryVarNames(output), attribute_names, 'class')
  return(skill_probs)
}

# Labels a results savedata Mplus file for a DCM model
# inputs: output: a String vector of an Mplus .out file
#         results: a dataframe of the content of an unlabeled Mplus results savedata file
#         parmNames: a String vector of parameter names in the correct order that Mplus outputs them.
# returns: a properly labeled results dataframe
labelResultsDCM <- function(output, results, parmNames){
  nFreeParms <- getNumberFreeParms(output)
  resultsAfterParms <- getResultsAfterParms(output)
  lengthOutputFile = (as.numeric(nFreeParms) * 2) + length(resultsAfterParms)
  results = data.frame(matrix(results, 1, lengthOutputFile, byrow = TRUE))
  names(results) <- c(parmNames, sprintf("SE_%s", parmNames), resultsAfterParms)
  return(results)
}

#################
###### IRT ######
#################

# Given a unidimensional IRT model, returns point estimates #
# model may be equal to Rasch, 1PL, 2PL, or Nominal #
getPointEstimatesUni <- function(model, output, results, ncategories= 0){
  resultsAfterParms <- getResultsAfterParms(output)
  nFreeParms <- getNumberFreeParms(output)
  nObs <- getNumberObservations(output)
  ny <- getNumberDependentVars(output) + getNumberIndependentVars(output)
  obsVars <- getVarNames(output)
  #from examining the output extracted above
  #for rasch
  if (model == "Rasch"){
    parmsOrder <- c(paste("diff", obsVars, sep=""))
  }
  #next line is a 1PL allowing disc to be freely estimated but constrained across items
  else if (model == "1PL"){
    parmsOrder <- c("disc", paste("diff", obsVars, sep = ""))
  }
  #for 2 PL post
  else if (model == "2PL"){
    parmsOrder <- c(paste("disc", obsVars, sep = ""), paste("diff", obsVars, sep=""))
  }
  else if (model == "Nominal"){
  parmsOrder <- c()
    for(i in 1:length(obsVars)){
      parmsOrder = c(parmsOrder, sprintf("c%s#%d", obsVars[i], 1:(ncategories-1)))
    }
    for(i in 1:length(obsVars)){
      parmsOrder = c(parmsOrder, sprintf("a%s#%d", obsVars[i], 1:(ncategories-1)))
    }
  }
  else {
    return("MODEL NOT MATCHED")
  }
  outputOrder <- c(parmsOrder, paste("SE_", parmsOrder, sep=""), resultsAfterParms)
  nItems <- length(obsVars)
  
  #Length of results output
  lengthOutputFile = (as.numeric(nFreeParms) * 2) + length(resultsAfterParms)
  
  results = data.frame(matrix(results, 1, lengthOutputFile, byrow = TRUE))
  names(results) <- outputOrder
  ##### FOR RASCH #######
  if (model == "Rasch"){
    b <- unlist(results[1:nItems])
    a <- rep(1, nItems)
    names(a) <- rep("disc", nItems)
    c <- rep(0, nItems)
  }
  else if (model == "2PL"){
    #### FOR 2PL #####
    disc <- results[1:nItems]
    a <- unlist(disc)
    b <- unlist(results[(nItems+1):(nItems*2+1)])
    b <- unlist(b/results[1:nItems])
    c <- rep(0, nItems)
  }
  else if (model == "1PL"){
    ########### 1 PL########
    disc <- results[1]
    a <- unlist(rep(disc, nItems))
    c <- rep(0, nItems)
    b <- unlist(results[2:(nItems+1)])
    b <- unlist(b/a)
  }
  else if (model == "Nominal"){
    c <- unlist(results[1:(nItems*(ncategories-1))])
    a <- unlist(results[((nItems*(ncategories-1))+1):((nItems*(ncategories-1))*2)])
    c_last_category <- c()
    a_last_category <- c()
    #for(i in 1:length(obsVars)){
    #  c_last_category <- c(c_last_category, -sum(c[grep(obsVars[i], names(c))]))
    #  a_last_category <- c(a_last_category, -sum(a[grep(obsVars[i], names(a))]))
    #}
    
    a_last_category <- rep(0, length(obsVars))
    c_last_category <- rep(0, length(obsVars))
    
    names(c_last_category) <- sprintf("c%s#%d", obsVars, ncategories)
    names(a_last_category) <- sprintf("a%s#%d", obsVars, ncategories)

    a <- c(a, a_last_category)
    c <- c(c, c_last_category)
    return(list(a[order(names(a))],c[order(names(c))]))
  }
  return(list(a, b, c))
}
# Given a unidimensional IRT model, returns SEs #
# model may be equal to Rasch, 1PL, or 2PL #
getSEs = function(model, output, results){
  
	resultsAfterParms <- getResultsAfterParms(output)
	nFreeParms <- getNumberFreeParms(output)
	nObs <- getNumberObservations(output)
  ny <- getNumberDependentVars(output) + getNumberIndependentVars(output)
  obsVars <- getVarNames(output)

	#from examining the output extracted above
	#for rasch
	if (model == "Rasch"){
		parmsOrder <- c(paste("diffSE", obsVars, sep=""))
	}
	#next line is a 1PL allowing disc to be freely estimated but constrained across items
	else if (model == "1PL"){
		parmsOrder <- c("discSE", paste("diffSE", obsVars, sep = ""))
	}
	#for 2 PL post
	else if (model == "2PL"){
		parmsOrder <- c(paste("discSE", obsVars, sep = ""), paste("diffSE", obsVars, sep=""))
	}
	else {
		return("MODEL NOT MATCHED")
	}
	outputOrder <- c(parmsOrder, paste("SE_", parmsOrder, sep=""), resultsAfterParms)
	nItems <- length(obsVars)
	
	#Length of results output
	lengthOutputFile = (as.numeric(nFreeParms) * 2) + length(resultsAfterParms)
	
  results = data.frame(matrix(results, 1, lengthOutputFile, byrow=TRUE))
	names(results) <- outputOrder
	##### FOR RASCH #######
	if (model == "Rasch"){
		a <- rep(0, nItems)
    b <- unlist(results[(nItems + 1):(nItems * 2)])
    c <- rep(0, nItems)
		return(list(a, b, c))
	}
	else if (model == "2PL"){
		ses <- unlist(results[(nItems * 2 + 1):(nItems * 2 * 2)])
    a <- ses[grep("disc", names(ses))]
    b <- ses[grep("diff", names(ses))]
    c <- rep(0, nItems)
    return(list(a, b, c))
	}
	else if (model == "1PL"){
		########### 1 PL########
		ses <- unlist(results[(nItems + 2):(nItems * 2 + 1)])
    a <- rep(ses[1], nItems)
    b <- ses[2:length(ses)]
    c <- rep(0, nItems)
		return(list(a, b, c))
	}
	return(ses)
}

##############################
### READING IN MPLUS FILES ###
##############################

#### reading in mplus output files
read_mplus_output <- function(model_names){
  if (!is.list(model_names)){
    model_names <- as.list(model_names)
  }
  out <- llply(model_names, function(x){
    scan(sprintf("data/work/mplus/%s.out", x)
         , what = ""
         , sep="\n"
         , quiet = TRUE)})
}
# function for reading irt mplus fscores output files
read_mplus_fscores <- function(test_name, lesson_name, model_names){
  if (!is.list(model_names)){
    model_names <- as.list(model_names)
  }
  mplus_model_prefix <- paste0(test_name,".", lesson_name)
  irt_fscores <- llply(model_names, function(x){
    read.table(sprintf("data/work/mplus/factor_scores/fscores.%s.%s.dat", mplus_model_prefix, x))})
}
# function for reading irt mplus results output files
read_mplus_results <- function(model_names){
  if (!is.list(model_names)){
    model_names <- as.list(model_names)
  }
  #mplus_model_prefix <- paste0(test_name,".", lesson_name)
  results <- llply(model_names, function(x){
    scan(sprintf("data/work/mplus/item_parameters/results.%s.dat", x)
         , what=double()
         , quiet = TRUE)})
}
# function for reading irt mplus cprobs output files
read_mplus_cprobs <- function(test_name, lesson_name, model_names){
  if (!is.list(model_names)){
    model_names <- as.list(model_names)
  }
  mplus_model_prefix <- paste0(test_name,".", lesson_name)
  irt_fscores <- llply(model_names, function(x){
    read.table(sprintf("data/work/mplus/class_probabilities/cprobs.%s.%s.dat", mplus_model_prefix, x))})
}
