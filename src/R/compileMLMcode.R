##########################################################################
###########     Compile MPLUS script for MLM #################
##########################################################################
  
cat(paste("title: MLM with time varying covariate;"), 
       file=inp, append=FALSE, sep="")
cat(paste("\n
data: file = \"",data,"\";

variable:  names = y x id;
missing=.;
within = x;
between = ;
cluster = id;
!centering = grandmean (x);

analysis: type = twolevel random;
model: 

%WITHIN%

s | y on x;

%BETWEEN%
y with s
;

savedata: results = results.dat;


"),file=inp,append=TRUE)