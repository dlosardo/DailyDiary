if(!AR1){

if(!INIT){
  npad = 50 # start up
  ist = npad + 1   
  ntt = nt + npad
}
if(INIT){
  ntt = nt
  ist = 1
}

#if (model == 1 | model == 2){
# y = c + Z*lvs + error, error ~ N(0, U)
# lvs_t = d + T*lvs_{t-1} + disturbance, disturbance ~ N(0, V)
# Z
lv_coef <- matrix(c(rep(1, nt), 1:nt), nt, nlv)
# V
lv_covs = lv_cov_matrix(c(0, 0, 0))
# T
lv_transition = diag(nlv)
# U
measurement_covs = diag(pop_values[[2]], ny*nt, ny*nt)
# c
lv_intercepts = matrix(rep(0, nlv), nlv, 1, byrow = TRUE)
# d
#d=matrix(c(3.4,2.5,4.4,5.2,3,4),ny,1,byrow=T)
measurement_intercepts = matrix(rep(0, ny*nt), ny*nt, 1, byrow = TRUE)
# states a t=1

lv0 = matrix(rep(pop_values[[3]], nlv), nlv, 1, byrow=T)
lv_covs0 = lv_cov_matrix(pop_values[[1]])

chol_measurement_covs <- chol(measurement_covs)
chol_lv_covs <- if(det(lv_covs) > 0) chol(lv_covs) else matrix(0, nlv, nlv)
chol_lv_covs0 <- chol(lv_covs0)
#for AR(1)
if(AR1){
nt=20 	# number of time points
nlv=1   	# number of states
ny=1	# number of observed 
nx=0    # number of fixed regressors
np=100	# number of subjects

npad=0 # start up
ist=npad+1   
ntt=nt+npad
INIT=FALSE
# Z
Z=matrix(c(
1,0),
ny,nlv,byrow=T)
# V
V=matrix(c(
1),nlv,nlv,byrow=T)
# T
T=matrix(c(
1),nlv,nlv,byrow=T)
# U
U=matrix(c(0),ny,ny,byrow=T)
# c
c=matrix(c(.8),nlv,1,byrow=T)
# d
d=matrix(c(0),ny,1,byrow=T)
# states a t=0

# cholesky of Q & R 
Vs = chol(V)
Us = ifelse(det(U)>0,chol(U),0)
}
	