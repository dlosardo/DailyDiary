#######################
### MLM DATAGEN #######
#######################
# tau matrix (covariance matrix for random effects)
#-------------------------------------------
P=matrix(c(
1.2,.3,
.3,.4
),ne,ne,byrow=T)
# R measurement error covariance matrix
#--------------------------------------------
R=.1
# c fixed effects of coefficients
#----------------------------------
c=matrix(c(3,1.2),ne,1,byrow=T)
# cholesky of Q & R (have to be positive definite)
#---------------------------------------------------
Ps = chol(P)
Rs = chol(R)
#Place holders
#------------------------
y=matrix(0,nt,ny)
x=matrix(0,nt,ny)
yall=matrix(0,nt*np,ny+2)
# X 
#--------------------------
X=matrix(c(
1,0
),ny,ne,byrow=T)
#y = b0+b1*X + error
for (j in 1:np){ 
	#intercept and slope
	b0b1 = c + matrix(rnorm(ne)%*%Ps,ncol=1)
	for (i in 1:nt){
		#error
		etmp=t(rnorm(ny)%*%Rs)  
		#time varying covariate
		X[1,2]=runif(ny)
		ytmp=X%*%b0b1+etmp
		y[i,1:ny]=ytmp
		x[i,1:ny]=X[1,2]
		}
	yall[ (1+(j-1)*nt):(j*nt),1:ny] = y	#all the manifest observations
	yall[ (1+(j-1)*nt):(j*nt),2] = x
	yall[(1+(j-1)*nt):(j*nt),3] = j
	}