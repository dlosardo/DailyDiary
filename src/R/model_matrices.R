if(!AR1){

#nt=theTs[ss] 	# number of time points
#nlvs=2   	# number of states (LVs)
#ny=6	# number of observed 
nx=0    # number of fixed regressors
#np=sampleSizes[ss]	# number of subjects

if(!INIT){
  npad = 50 # start up
  ist = npad + 1   
  ntt = nt + npad
}
if(INIT){
  ntt = nt
  ist = 1
}

if (model == 1 | model == 2){
# Z
factor_loadings <- matrix(c(rep(1, nt), 1:nt), nt, nlvs)
# V
V = matrix(c(
popValues[5],popValues[6],
popValues[6],popValues[7]
),nlvs,nlvs,byrow=T)
# T
T=matrix(c(
popValues[8],popValues[9],
popValues[10],popValues[11]),nlvs,nlvs,byrow=T)
# U
U=diag(c(popValues[12],popValues[13],popValues[14],
	popValues[15],popValues[16],popValues[17]))
# c
c=matrix(c(0,0),nlvs,1,byrow=T)
# d
#d=matrix(c(3.4,2.5,4.4,5.2,3,4),ny,1,byrow=T)
d=matrix(c(0,0,0,0,0,0),ny,1,byrow=T)
# states a t=1

#for AR(1)
if(AR1){
nt=20 	# number of time points
nlvs=1   	# number of states
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
ny,nlvs,byrow=T)
# V
V=matrix(c(
1),nlvs,nlvs,byrow=T)
# T
T=matrix(c(
1),nlvs,nlvs,byrow=T)
# U
U=matrix(c(0),ny,ny,byrow=T)
# c
c=matrix(c(.8),nlvs,1,byrow=T)
# d
d=matrix(c(0),ny,1,byrow=T)
# states a t=0
lv0 = matrix(rep(0, nlvs), nlvs, 1, byrow=T)
cov_lv0=matrix(c(1000,0,0,
			0,1000,0,
			0,0,1000),
			nlvs,nlvs,byrow=T)
# cholesky of Q & R 
Vs = chol(V)
Us = ifelse(det(U)>0,chol(U),0)
}
	