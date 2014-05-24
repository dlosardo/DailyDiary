if(!INIT){
  npad = 50 # start up
  ist = npad + 1   
  ntt = nt + npad
}
if(INIT){
  ntt = nt
  ist = 1
}

# y = c + Z*lvs + error, error ~ N(0, U)
# lvs_t = d + T*lvs_{t-1} + R*disturbance, disturbance ~ N(0, V)
# initial condition covariance structure can be solved for stationary processes:
# vec(P0) = (Im^2 - TkronT`)^-1 + vec(RVR`) where m = # lvs

get_model_matrices_lcm <- function(ny, nt, nlv, pop_values){
  # Z
  lv_coef <- matrix(c(rep(1, nt), 0:(nt-1)), nt, nlv)
  # V
  lv_covs = lv_cov_matrix(c(0, 0, 0))
  # T
  lv_transition = diag(nlv)
  # U
  measurement_covs = diag(pop_values[[2]], ny*nt, ny*nt)
  # c
  lv_intercepts = matrix(rep(0, nlv), nlv, 1, byrow = TRUE)
  # d
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
  Rmat <- diag(1, nlv)
  # states a t = 1 
  lv0 = matrix(rep(pop_values[[3]], nlv), nlv, 1, byrow=T)
  lv_covs0 = lv_cov_matrix(pop_values[[1]])
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat))
}

#for AR(1)
get_model_matrices_ar <- function(ny, nt, nlv, pop_values){
  I = diag(1, nlv)
  lv_transition <- vector_to_non_symmetric_square_matrix(pop_values[[1]])
  lv_covs <- lv_cov_matrix(nlv)
  Rmat <- diag(1, nlv)
  lv_covs0 <- initial_cov_matrix(nlv, lv_transition, Rmat, lv_covs)
  lv0 = matrix(0, nlv, 1, byrow=T)
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
  measurement_covs = diag(0, ny*nt, ny*nt)
  lv_coef <- matrix(1, nt, nlv)
  lv_intercepts = matrix(rep(0, nlv), nlv, 1, byrow = TRUE)
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat))
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

get_model_matrices_ma <- function(ny, nt, nlv, pop_values){
  lv_transition <- matrix(c(0, 1, 0, 0), 2, 2, byrow=T)
  Rmat <- matrix(c(1, pop_values[[1]]), 2, 1)
  lv_covs <- matrix(pop_values[[2]], 1, 1)
  lv_covs0 <- initial_cov_matrix(nlv, lv_transition, Rmat, lv_covs)
  lv0 = matrix(0, 2, 1, byrow=T)
  measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 2, byrow = TRUE)
  measurement_covs = diag(0, ny*nt, ny*nt)
  lv_coef <- matrix(rep(c(1, 0), nt), nt, 2, byrow=T)
  lv_intercepts = matrix(rep(0, 2), nlv, 1, byrow = TRUE)
  return(list(lv_coef = lv_coef, lv_covs = lv_covs, lv_transition = lv_transition
              , measurement_covs = measurement_covs, lv_intercepts = lv_intercepts
              , measurement_intercepts = measurement_intercepts, lv0 = lv0
              , lv_covs0 = lv_covs0, Rmat = Rmat))
}
if (model_name == "AR"){
  matrices <- get_model_matrices_ar(ny, nt, nlv, pop_values)
} else if (model_name == "MA"){
  matrices <- get_model_matrices_ma(ny, nt, nlv, pop_values)
} else if (model_name == "LCM"){
  matrices <- get_model_matrices_lcm(ny, nt, nlv, pop_values)
}

lv_intercepts <- matrices[["lv_intercepts"]]
measurement_intercepts <- matrices[["measurement_intercepts"]]
lv_coef <- matrices[["lv_coef"]]
lv_transition <- matrices[["lv_transition"]]
lv0 <- matrices[["lv0"]]
Rmat <- matrices[["Rmat"]]

chol_measurement_covs <- if(det(matrices[["measurement_covs"]] > 0)) chol(matrices[["measurement_covs"]]) else matrix(0, ny*nt, ny*nt)
chol_lv_covs <- if(det(matrices[["lv_covs"]]) > 0) chol(matrices[["lv_covs"]]) else matrix(0, nlv, nlv)
chol_lv_covs0 <- chol(matrices[["lv_covs0"]])

	