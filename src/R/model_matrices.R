# y = c + Z*lvs + error, error ~ N(0, U)
# lvs_t = d + T*lvs_{t-1} + R*disturbance, disturbance ~ N(0, V)
# initial condition covariance structure can be solved for stationary processes:
# vec(P0) = (Im^2 - TkronT`)^-1 + vec(RVR`) where m = # lvs

get_model_matrices_lcm <- function(ny, nt, nlv, pop_values){
  # Z
  lv_coef <- matrix(c(rep(1, nt), 0:(nt-1)), nt, nlv)
  covariate <- matrix(1, np, nt)
  # V
  lv_covs = lv_cov_matrix(rep(0, get_unique_elements(nlv)))
  # T
  lv_transition = diag(nlv)
  # U
  measurement_covs = diag(pop_values[["MeasurementCov"]], ny*nt, ny*nt)
  # c
  lv_intercepts = matrix(rep(0, nlv), nlv, 1, byrow = TRUE)
  # d
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
  Rmat <- diag(1, nlv)
  # states a t = 1 
  lv0 = matrix(rep(pop_values[["LVMeans"]], nlv), nlv, 1, byrow=T)
  lv_covs0 = lv_cov_matrix(pop_values[["LVCov"]])
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat, covariate = covariate))
}

get_model_matrices_mlm <- function(ny, nt, nlv, pop_values){
  # Z
  lv_coef <- matrix(c(rep(1, nt), c(rep(1, nt))), nt, nlv)
  covariate <- matrix(runif(np*nt, -4, 4), np, nt)
  # V
  lv_covs = lv_cov_matrix(c(0, 0, 0))
  # T
  lv_transition = diag(nlv)
  # U
  measurement_covs = diag(pop_values[["MeasurementCov"]], ny*nt, ny*nt)
  # c
  lv_intercepts = matrix(rep(0, nlv), nlv, 1, byrow = TRUE)
  # d
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
  Rmat <- diag(1, nlv)
  # states a t = 1 
  lv0 = matrix(rep(pop_values[["LVMeans"]], nlv), nlv, 1, byrow=T)
  lv_covs0 = lv_cov_matrix(pop_values[["LVCov"]])
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat, covariate = covariate))
}

#for AR(1)
get_model_matrices_ar <- function(ny, nt, nlv, pop_values, nstates){
  covariate <- matrix(1, np, nt)
  I = diag(1, nstates - 1)
  lower_trans <- matrix(cbind(diag(1, nstates - 1), rep(0, nstates - 1)), nstates - 1, nstates)
  lv_transition <- rbind(pop_values[["ARParm1"]], lower_trans)
  lv_covs <- lv_cov_matrix(pop_values[["ProcessNoise"]])
  r_vals <- c(1, rep(0, nstates - 1))
  Rmat <- matrix(r_vals, nrow = nstates)
  #state_covs <- Rmat%*%lv_covs%*%t(Rmat)
  lv_covs0 <- initial_cov_matrix(nstates, lv_transition, Rmat, lv_covs)
  lv0 = matrix(0, nstates, 1, byrow=T)
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
  measurement_covs = diag(0, ny*nt, ny*nt)
  lv_coef <- matrix(c(1, rep(0, nstates - 1)), nt, nstates, byrow = TRUE)
  lv_intercepts = matrix(rep(0, nstates), nstates, 1, byrow = TRUE)
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat, covariate = covariate))
}

########
# y_t = [1 0][y_t beta*z_t]
# a_t = [0 1, 0 0]a_t-1 + [1 beta]z_t
# 
# 
# y = c + Z*lvs + error, error ~ N(0, U)
# lvs_t = d + T*lvs_{t-1} + R*disturbance, disturbance ~ N(0, V)
# initial condition covariance structure can be solved for stationary processes:
# vec(P0) = (Im^2 - TkronT)^-1 * vec(RVR`) where m = # lvs
# a_0 = [y_0, beta*v_0]
# y_0 = beta*y_-1 + v_0

get_model_matrices_ma <- function(ny, nt, nlv, pop_values, nstates){
  covariate <- matrix(1, np, nt)
  lv_transition <- matrix(c(0, 1, 0, 0), nstates, nstates, byrow=T)
  Rmat <- matrix(c(1, pop_values[["MAParm1"]]), nstates, 1)
  lv_covs <- matrix(pop_values[["ProcessNoise"]], nlv, nlv)
  lv_covs0 <- initial_cov_matrix(nstates, lv_transition, Rmat, lv_covs)
  lv0 = matrix(0, nstates, 1, byrow=T)
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 2, byrow = TRUE)
  measurement_covs = diag(0, ny*nt, ny*nt)
  lv_coef <- matrix(rep(c(1, 0), nt), nt, 2, byrow=T)
  lv_intercepts = matrix(rep(0, nstates), nstates, 1, byrow = TRUE)
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat, covariate = covariate))
}
get_model_matrices_arma <- function(ny, nt, nlv, pop_values, nstates, p, q){
  covariate <- matrix(1, np, nt)
  m <- max(p, q + 1)
  lv_coef <- matrix(c(1, rep(0, m - 1)), nt, m, byrow=T)
  lv_transition <- cbind(arma_trans_first_row(pop_values[["ARParm1"]])
                         , rbind(diag(1, m - 1), rep(0, m - 1)))
  Rmat <- matrix(c(1, pop_values[["MAParm1"]]), 2, 1)
  lv_covs <- matrix(pop_values[["ProcessNoise"]], 1, 1)
  lv_covs0 <- initial_cov_matrix(nstates, lv_transition, Rmat, lv_covs)
  lv0 = matrix(0, nstates, 1, byrow=T)
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 2, byrow = TRUE)
  measurement_covs = diag(0, ny*nt, ny*nt)
  lv_coef <- matrix(rep(c(1, 0), nt), nt, 2, byrow=T)
  lv_intercepts = matrix(rep(0, nstates), nstates, 1, byrow = TRUE)
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat, covariate = covariate))
}

get_model_matrices_alt <- function(ny, nt, nlv, pop_values){
  # Z
  lv_coef <- matrix(c(rep(1, nt), rep(1, nt), 0:(nt-1)), nt, nlv)
  covariate <- matrix(1, np, nt)
  # V
  lv_covs = diag(c(pop_values[["ProcessNoise"]], rep(0, nlv - 1)))
  # T
  lv_transition = diag(c(pop_values[["ARParm1"]], rep(1, nlv - 1)))
  # U
  #measurement_covs = diag(pop_values[["MeasurementCov"]], ny*nt, ny*nt)
  measurement_covs = diag(0, ny*nt, ny*nt)
  # c
  lv_intercepts = matrix(rep(pop_values[["LVMeans"]], nlv), nlv, 1, byrow = TRUE)
  # d
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
  Rmat <- diag(1, nlv)
  # states a t = 1 
  lv_covs0 = lv_cov_matrix(pop_values[["LVCov"]])
  #lv_covsAR0 <- initial_cov_matrix(1, matrix(pop_values[["ARParm1"]]), matrix(1), matrix(pop_values[["ProcessNoise"]]))
  #lv_covs0[1, 1] <- lv_covsAR0
  lv0 = matrix(rep(pop_values[["LVMeans"]], nlv), nlv, 1, byrow=T)
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat, covariate = covariate))
}

model_matrix_setup <- function(model_name, ny, nt, nlv, pop_values, nstates, p, q){
  if (model_name == "AR"){
    matrices <- get_model_matrices_ar(ny, nt, nlv, pop_values, nstates)
  } else if (model_name == "MA"){
    matrices <- get_model_matrices_ma(ny, nt, nlv, pop_values, nstates)
  } else if (model_name == "LCM"){
    matrices <- get_model_matrices_lcm(ny, nt, nlv, pop_values)
  } else if (model_name == "MLM"){
    matrices <- get_model_matrices_mlm(ny, nt, nlv, pop_values)
  } else if (model_name == "ARMA"){
    matrices <- get_model_matrices_arma(ny, nt, nlv, pop_values, nstates, p, q)
  } else if (model_name == "ALT"){
    matrices <- get_model_matrices_alt(ny, nt, nlv, pop_values)
  }
  matrices$chol_measurement_covs <- if(det(matrices[["measurement_covs"]] > 0)) chol(matrices[["measurement_covs"]]) else matrix(0, ny*nt, ny*nt)
  matrices$chol_lv_covs <- if(det(matrices[["lv_covs"]]) > 0) chol(matrices[["lv_covs"]]) else ldl(matrices[["lv_covs"]])
  matrices$chol_lv_covs0 <- chol(matrices[["lv_covs0"]])
  return(matrices)
}

	