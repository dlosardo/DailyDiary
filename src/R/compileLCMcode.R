##########################################################################
###########     Compile MPLUS script for LCM #################
##########################################################################


var_names = paste(c("Y"), combine, sep = "")
ints = paste(c(labs), "@1", sep = "")
slps = paste(c(labs),"@",sep="")
slps = paste(c(slps),(combine-1),sep="")
ints1 = paste(c(labs),"@0",sep="")
  
cat(paste("title: LCM;"), 
       file=inp, append=FALSE, sep="")
cat(paste("\n
data: file = \"",data,"\";"),file=inp,append=TRUE)

cat(paste(c("\n
variable:  names = y1-y14 id;
usevariables = ",labs,";\n
missing=.;")),file=inp,append=TRUE)

cat(paste(c("\n
model: 
int by ",ints,";
slp by ",slps,";

int slp;
[int slp];
int with slp;

",labs," (1);
[",ints1,"];


savedata: results = results.dat;
")),file=inp,append=TRUE)
