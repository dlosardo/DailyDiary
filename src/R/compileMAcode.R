##########################################################################
###########     Compile MPLUS script for MA #################
##########################################################################
  
cat(paste("title: MA ;"), 
       file=inp, append=FALSE, sep="")
cat(paste("\n
data: file = \"",data,"\";

variable:  names = y1-y14 id;
missing=.;
usevariables = y1-y14 ;

model: 

!initial status
inity by y1@0;
inity*;
[inity*];
y1 on inity@1;

!exogenous LVs

ksi1 by y1@1;
ksi2 by y2@1;
ksi3 by y3@1;
ksi4 by y4@1;
ksi5 by y5@1;
ksi6 by y6@1;
ksi7 by y7@1;
ksi8 by y8@1;
ksi9 by y9@1;
ksi10 by y10@1;
ksi11 by y11@1;
ksi12 by y12@1;
ksi13 by y13@1;
ksi14 by y14@1;

!MA process;
y2 on ksi1 (1);
y3 on ksi2 (1);
y4 on ksi3 (1);
y5 on ksi4 (1);
y6 on ksi5 (1);
y7 on ksi6 (1);
y8 on ksi7 (1);
y9 on ksi8 (1);
y10 on ksi9 (1);
y11 on ksi10 (1);
y12 on ksi11 (1);
y13 on ksi12 (1);
y14 on ksi13 (1);

!no corrs;
ksi1 with ksi2-ksi14@0;
ksi2 with ksi3-ksi14@0;
ksi3 with ksi4-ksi14@0;
ksi4 with ksi5-ksi14@0;
ksi5 with ksi6-ksi14@0;
ksi6 with ksi7-ksi14@0;
ksi7 with ksi8-ksi14@0;
ksi8 with ksi9-ksi14@0;
ksi9 with ksi10-ksi14@0;
ksi10 with ksi11-ksi14@0;
ksi11 with ksi12-ksi14@0;
ksi12 with ksi13-ksi14@0;
ksi13 with ksi14@0;
ksi1-ksi14 with inity@0;


!equal resid vars over time;
ksi1-ksi14 (2);

[y1-y14@0];
y1-y14@0;


savedata: results = results.dat;
"),file=inp,append=TRUE)