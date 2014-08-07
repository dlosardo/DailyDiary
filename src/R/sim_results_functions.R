#Mean
mean_ = function(x){
  mean(x,na.rm=TRUE)
}
#SD
sd_ = function(x){
  sd(x,na.rm=TRUE)
}
#Bias function
bias = function(x,truex){
  mean(x,na.rm=T) - truex
}
#Relative Bias function
rel_bias = function(x,truex){
  (mean(x,na.rm=T) - truex)/truex
}
#CI coverage
lcl = function(x,xse){
  x-(1.96*xse)
}
ucl = function(x,xse){
  x+(1.96*xse)
}
coverage = function(lcl,ucl,truex){
  if (ucl>truex && truex>lcl) 1
  else 0
}
#Power
power = function(x,xse){
  if (abs(x/xse) > 1.96) 1
  else 0 
}
#RMSE
rmse = function(x,truex){
  sqrt(mean_((x-truex)^2))
}

arsde = function(SEhat,SE){
  (SEhat-SE)/SE
}
cv = function(x,SE){
  SE/x
}
