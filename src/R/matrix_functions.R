# Calculates the number of variables given a vector of unique elements in a covariance matrix
get_num_vars <- function(x){
  (sqrt(8 * x + 1) - 1)/2
}
get_unique_elements <- function(x){
  (x*(x+1))/2
}
#writing function for 'vec' operator (stacking columns of matrices)
vecop = function(M){
  sM = NULL
  for (i in 1:ncol(M)){
    Mtemp = M[,i]
    sM = c(sM,Mtemp)
  }
  return(sM)
}

#writing function for oppposite of 'vec' operator (making a matrix out of a stack of columns)
vec2mat = function(V,n){
  sV = NULL
  for (i in 1:(length(V)/n)){
    Vtemp = V[(1+(i-1)*n):(i*n)]
    sV = rbind(sV,Vtemp)
  }
  return(sV)
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

initial_cov_matrix <- function(nstates, Tmat, Rmat, Vmat){
  I <- diag(1, nstates^2)
  vals <- solve(I - (Tmat%x%Tmat))%*%matrix(vecop(Rmat%*%Vmat%*%t(Rmat)), nstates^2, 1)
  return(matrix(c(vals), nstates, nstates, byrow=T))
}

get_ar_lags <- function(y, items, nt){
  sapply(y:nt, function(x) paste0(items[x], " on ", items[x - (y - 1)], " (", y - 1, ");\n"))
}

arma_trans_first_row <- function(pop_values){
  if (length(pop_values) == 1){
    return(c(pop_values, 0))
  } else{
    return(pop_values)
  }
}

get_nstates <- function(model_type, nlv, p, q){
  if (model_type == "LCM"){
    return(nlv)
  } else if (model_type == "MLM"){
    return(nlv)
  } else if (model_type == "AR"){
    return(nlv)
  } else if (model_type == "MA"){
    return(nlv + 1)
  } else if (model_type == "ARMA"){
    return(nlv + 1)
  } else{
    return(NULL)
  }
}