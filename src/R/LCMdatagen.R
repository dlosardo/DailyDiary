###########################
## LCM DATA GENERATION ####
###########################
# S factor loading matrix
#--------------------------
S=matrix(c(
1,0
),ny,nlvs,byrow=T)
# Q process noise/factor covariance matrix
#-------------------------------------------
P=matrix(c(
.3,.05,
.05,.1
),nlvs,nlvs,byrow=T)
# R measurement error covariance matrix
#--------------------------------------------
R=1.2
# c intercepts of latent variables
#----------------------------------
c=matrix(c(1,0.02),nlvs,1,byrow=T)
# cholesky of Q & R (have to be positive definite)
#---------------------------------------------------
Ps = chol(P)
Rs = chol(R)
#Place holders
#------------------------
y=matrix(0,nt,ny)
yall=matrix(0,nt*np,ny)
for (j in 1:np){ 
	a0 = c + matrix(rnorm(nlvs)%*%Ps,ncol=1)
	etmp=t(rnorm(ny)%*%Rs)
	S[1,2] = 0
	ytmp=S%*%a0+etmp
	y[1,1:ny]=ytmp
	for (i in 2:nt){
		etmp=t(rnorm(ny)%*%Rs)
		S[1,2] = i-1
		ytmp=S%*%a0+etmp
		y[i,1:ny]=ytmp
	} #end i loop
yall[ (1+(j-1)*nt):(j*nt),1:ny] = y	#all the manifest observations
} #end j loop