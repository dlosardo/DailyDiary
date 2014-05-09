###################
## AR DATAGEN #####
###################
yall = matrix(0,nt*np,1)
ts.sim = NULL

for(i in 1:np){
ts.sim <- arima.sim(list(order = c(1,0,0), ar = 0.7), n = nt, sd = 1)

yall[ (1+(i-1)*nt):(i*nt),1:ny] = t(ts.sim)
}