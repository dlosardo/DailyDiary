#To simulate data for a model formulated in the state-space framework.

# innov z residuals e
lv = matrix(0, ntt, nlv)
y = matrix(0, ntt, ny)
x = matrix(0, ntt, nx)
y_all_ntt = matrix(0, ntt*np, ny)
lv_all = matrix(0, nt*np, nlv)
y_all_nt = matrix(0, nt*np, ny)

for (i in 1:np){
  init = t(lv0) + rnorm(nlv)%*%chol_lv_covs0
  lv[1, 1:nlv] = t(init)
  error = t(rnorm(1)%*%chol_measurement_covs[1, 1])
  cur_y = measurement_intercepts[1] + lv_coef[1, ]%*%lv[1, 1:nlv] + error
  y[1, 1] = cur_y
  for (t in 2:ntt){
    disturbance = t(rnorm(nlv)%*%chol_lv_covs)
    error = t(rnorm(1)%*%chol_measurement_covs[t, t])
    cur_lv = as.matrix(lv[t - 1, 1:nlv])
    cur_lv = lv_intercepts + lv_transition%*%cur_lv + disturbance
    lv[t, 1:nlv] = t(cur_lv)
    cur_y = measurement_intercepts[t] + lv_coef[t, ]%*%cur_lv + error
    y[t, 1:ny] = cur_y
  }
  y_all_ntt[(1 + (i - 1) * ntt):(i * ntt), 1:ny] = y
  y_all_nt[(1 + (i - 1) * nt):(i * nt), 1:ny] = y[(ist:ntt), 1:ny]
  lv_all[(1 + (i - 1) * nt):(i * nt), 1:nlv] = lv[(ist:ntt), 1:nlv]
}
###yntonly structure - each person's data going from t=1 to t=nt stacked into rows, across the ny indicators
#Transforming to wide format for complete data set
y_all <- data.frame(values = y_all_nt, id = factor(rep(1:np, each = nt))
                    , time = factor(rep(1:nt, np)))
tmp <- dcast(y_all, id ~ time, value.var = "values")
lv_all <- data.frame(values = lv_all, id = factor(rep(1:np, each = nt))
                    , time = factor(rep(1:nt, np)))


