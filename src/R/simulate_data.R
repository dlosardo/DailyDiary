#To simulate data for a model formulated in the state-space framework.
simulate_data <- function(matrices, ny, nt, nlv, pop_values, nstates, p, q){
  lv = matrix(0, nt, nstates)
  y = matrix(0, nt, ny)
  x = matrix(0, nt, nx)
  lv_all = matrix(0, nt*np, nstates)
  y_all_nt = matrix(0, nt*np, ny)
  for (i in 1:np){
    init = t(matrices$lv0) + rnorm(nstates)%*%matrices$chol_lv_covs0
    lv[1, ] = t(init)
    error = t(rnorm(1)%*%matrices$chol_measurement_covs[1, 1])
    cur_y = matrices$measurement_intercepts[1] +
      (matrices$lv_coef[1, ]*c(1, matrices$covariate[i, 1]))%*%lv[1, ] + error
    y[1, 1] = cur_y
    for (t in 2:nt){
      ifelse(t > nt, t_ <- 2, t_ <- t)
      disturbance = matrices$Rmat%*%matrix(rnorm(nlv)%*%matrices$chol_lv_covs, nlv, 1)
      error = t(rnorm(ny)%*%matrices$chol_measurement_covs[t_, t_]) #only works with one y
      prev_lv = as.matrix(lv[t - 1, ])
      cur_lv = matrices$lv_intercepts + matrices$lv_transition%*%prev_lv + disturbance
      lv[t, ] = t(cur_lv)
      cur_y = matrices$measurement_intercepts[t_] +
        (matrices$lv_coef[t_, ]*c(1, matrices$covariate[i, t]))%*%cur_lv + error
      y[t, 1:ny] = cur_y
    }
    y_all_nt[(1 + (i - 1) * nt):(i * nt), 1:ny] = y[(1:nt), 1:ny]
    lv_all[(1 + (i - 1) * nt):(i * nt), 1:nlv] = lv[(1:nt), 1:nlv]
  }
  #Transforming to wide format for complete data set
  y_all <- data.frame(values = y_all_nt, id = factor(rep(1:np, each = nt))
                      , time = factor(rep(1:nt, np)))
  y_all_wide <- dcast(y_all, id ~ time, value.var = "values")
  lv_all_wide <- data.frame(values = lv_all, id = factor(rep(1:np, each = nt))
                      , time = factor(rep(1:nt, np)))
  return(list(y_all_wide = y_all_wide, lv_all_wide = lv_all_wide))
}
