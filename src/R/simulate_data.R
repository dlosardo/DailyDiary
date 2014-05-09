#To simulate data for a model formulated in the state-space framework.

# innov z residuals e
lv = matrix(0, ntt, nlvs)
y = matrix(0, ntt, ny)
x = matrix(0, ntt, nx)
y_all_ntt = matrix(0, ntt*np, ny)
lv_all = matrix(0, nt*np, nlvs)
y_all_nt = matrix(0, nt*np, ny)

for (i in 1:np){
  init = t(a0) + rnorm(nlvs)%*%P0s
  lv[1, 1:nlvs] = t(init)
  cur_error = t(rnorm(ny)%*%Us)
  cur_y = Z%*%lv[1, 1:nlvs] + error + d
  y[1, 1:ny] = cur_y
  for (t in 2:ntt){
    disturbance = t(rnorm(nlvs)%*%Vs)
    error = t(rnorm(ny)%*%Us)
    cur_lv = as.matrix(lv[t - 1, 1:nlvs])
    cur_lv = T%*%cur_lv + disturbance + c
    lv[t, 1:nlvs] = t(cur_lv)
    cur_y = Z%*%cur_lv + error + d
    y[t, 1:ny] = cur_y
  }
  y_all_ntt[(1 + (i - 1) * ntt):(i * ntt), 1:ny] = y
  y_all_nt[(1 + (i - 1) * nt):(i * nt), 1:ny] = y[(ist:ntt), 1:ny]
  lv_all[(1 + (i - 1) * nt):(i * nt), 1:nlvs] = a[(ist:ntt), 1:nlvs]
}
###yntonly structure - each person's data going from t=1 to t=nt stacked into rows, across the ny indicators

#Transforming to wide format for complete data set
Wide1 = NULL
All1=NULL
for (i in 1:np){
  timet = NULL
  newt = NULL
  newt2 = NULL
  newt3 = NULL
  newall= NULL
  for (t in 1:nt){
    personi = yntonly[t+(nt*(i-1)),]
    truei = all[t+(nt*(i-1)),1]
    truei2= all[t+(nt*(i-1)),2]
    if (model==3){
      if (sdys){
        truei3 = all[t+(nt*(i-1)),3]
      }
    }
    timet = c(timet,personi)
    newt = c(newt,truei)
    newt2 = c(newt2,truei2)
    if (model==3){
      if (sdys){
        newt3=c(newt3,truei3)
        newt3=as.matrix(t(newt3))
      }
    }
    timet = as.matrix(t(timet))
    newt = as.matrix(t(newt))
    newt2 = as.matrix(t(newt2))
    newall=c(newt,newt2)
    if (model==3){
      if (sdys){
        newall=c(newall,newt3)
      }
    }
  }
  Wide1 = rbind(Wide1,timet)
  All1 = rbind(All1,newall)
}

