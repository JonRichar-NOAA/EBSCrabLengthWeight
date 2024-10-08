library(stats4)
library(readxl)
library(tidyverse)
Female_EBSCB_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/Female_EBSCB_weights_temps_analysis.csv")

Female_EBSCB_weights_and_temps
FEBSCB=Female_EBSCB_weights_and_temps
FEBSCB%>% filter(CLUTCH_SIZE==4|CLUTCH_SIZE==6)->FEBSCB_mat

# Sample size
nrow(FEBSCB_mat)

#MODEL 1#
#Null

LW1=function(a,b,sd){
  ave=log(FEBSCB_mat$WIDTH)*b+a
	-sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
	}
param=list(a=-8,b=3,sd=0.1)
mLW1=mle(LW1,start=param)
summary(mLW1)

#MODEL 2: #
#a(SC)
LW2=function(a,aOS,b,sd){
  a=a+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,aOS=0,sd=0.1)
mLW2=mle(LW2,start=param)
summary(mLW2)

#MODEL 3: #
#b(SC)
LW3=function(a,bOS,b,sd){
  b=b+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,sd=0.1)
mLW3=mle(LW3,start=param)
summary(mLW3)

#MODEL 4: #
#a(SC)b(SC)
LW4=function(a,aOS,b,bOS,sd){
  a=a+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  b=b+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bOS=0,sd=0.1)
mLW4=mle(LW4,start=param)
summary(mLW4)

#MODEL 5: #
#a(T)
LW5=function(a,aT,b,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,sd=0.1)
mLW5=mle(LW5,start=param)
summary(mLW5)

#MODEL 6: #
#b(T)
LW6=function(a,bT,b,sd){
  b=b+bT*FEBSCB_mat$Temperature_MATFEM
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,bT=0,b=3,sd=0.1)
mLW6=mle(LW6,start=param)
summary(mLW6)

#MODEL 7: #
#a(T)b(T)
LW7=function(a,aT,b,bT,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM
  b=b+bT*FEBSCB_mat$Temperature_MATFEM
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bT=0,sd=0.1)
mLW7=mle(LW7,start=param)
summary(mLW7)

#MODEL 8: #
#a(Tr)b(Tr)
LW8=function(a,aTr,b,bTr,sd){
  a=a+aTr*(FEBSCB_mat$Temperature_MATFEM>3.5)
  b=b+bTr*(FEBSCB_mat$Temperature_MATFEM>3.5)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,b=3,bTr=0,sd=0.1)
mLW8=mle(LW8,start=param)
summary(mLW8)

#MODEL 9: #
#a(T,T2)b(T,T2)
LW9=function(a,aT,aT2,b,bT,bT2,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM+aT2^2*FEBSCB_mat$Temperature_MATFEM
  b=b+bT*FEBSCB_mat$Temperature_MATFEM+bT2^2*FEBSCB_mat$Temperature_MATFEM
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aT2=0,b=3,bT=0,bT2=0,sd=0.1)
mLW9=mle(LW9,start=param)
summary(mLW9)

#MODEL 10: #
#a(T,SC)b(T)
LW10=function(a,aT,aOS,b,bT,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  b=b+bT*FEBSCB_mat$Temperature_MATFEM
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,bT=0,sd=0.1)
mLW10=mle(LW10,start=param)
summary(mLW10)

#MODEL 11: #
#a(T)b(T,SC)
LW11=function(a,aT,b,bT,bOS,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM
  b=b+bT*FEBSCB_mat$Temperature_MATFEM+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bOS=0,bT=0,sd=0.1)
mLW11=mle(LW11,start=param)
summary(mLW11)

#MODEL 12: #
#a(T,SC)b(T,SC)
LW12=function(a,aT,aOS,b,bT,bOS,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  b=b+bT*FEBSCB_mat$Temperature_MATFEM+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,bOS=0,bT=0,sd=0.1)
mLW12=mle(LW12,start=param)
summary(mLW12)


#MODEL 13: #
#a(SC,Tr)b(Tr)
LW13=function(a,aOS,aTr,b,bTr,sd){
  a=a+aTr*(FEBSCB_mat$Temperature_MATFEM>3.5)+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  b=b+bTr*(FEBSCB_mat$Temperature_MATFEM>3.5)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,aTr=0,b=3,bTr=0,sd=0.1)
mLW13=mle(LW13,start=param)
summary(mLW13)


#MODEL 14: #
#a(Tr)b(SC,Tr)
LW14=function(a,aTr,b,bOS,bTr,sd){
  a=a+aTr*(FEBSCB_mat$Temperature_MATFEM>3.5)
  b=b+bTr*(FEBSCB_mat$Temperature_MATFEM>3.5)+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,b=3,bOS=0,bTr=0,sd=0.1)
mLW14=mle(LW14,start=param)
summary(mLW14)


#MODEL 15: #
#a(SC,Tr)b(SC,Tr)
LW15=function(a,aOS,aTr,b,bOS,bTr,sd){
  a=a+aTr*(FEBSCB_mat$Temperature_MATFEM>3.5)+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  b=b+bTr*(FEBSCB_mat$Temperature_MATFEM>3.5)+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,aTr=0,b=3,bOS=0,bTr=0,sd=0.1)
mLW15=mle(LW15,start=param)
summary(mLW15)

#MODEL 16: #
#a(SC,T)b
LW16=function(a,aT,aOS,b,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,sd=0.1)
mLW16=mle(LW16,start=param)
summary(mLW16)

#MODEL 17: #
#a,b(T,SC)
LW17=function(a,b,bT,bOS,sd){
  b=b+bT*FEBSCB_mat$Temperature_MATFEM+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,bT=0,sd=0.1)
mLW17=mle(LW17,start=param)
summary(mLW17)

#MODEL 18: #
#a(SC,T(SC))b
LW18=function(a,a2T,aOS,b,sd){
  a=a+a2T*FEBSCB_mat$Temperature_MATFEM*(FEBSCB_mat$SHELL_CONDITION==2)+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,a2T=0,aOS=0,b=3,sd=0.1)
mLW18=mle(LW18,start=param)
summary(mLW18)

#MODEL 19: #
#a(SC#T))b
LW19=function(a,aT,a2T,aOS,b,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM+a2T*FEBSCB_mat$Temperature_MATFEM*(FEBSCB_mat$SHELL_CONDITION==2)+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,sd=0.1)
mLW19=mle(LW19,start=param)
summary(mLW19)

#MODEL 20: #
#a,b(T(SC),SC)
LW20=function(a,b,b2T,bOS,sd){
  b=b+b2T*FEBSCB_mat$Temperature_MATFEM*(FEBSCB_mat$SHELL_CONDITION==2)+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,b2T=0,sd=0.1)
mLW20=mle(LW20,start=param)
summary(mLW20)

#MODEL 21: #
#a,b(T#SC)
LW21=function(a,b,bT,b2T,bOS,sd){
  b=b+bT*FEBSCB_mat$Temperature_MATFEM+b2T*FEBSCB_mat$Temperature_MATFEM*(FEBSCB_mat$SHELL_CONDITION==2)+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bT=0,bOS=0,b2T=0,sd=0.1)
mLW21=mle(LW21,start=param)
summary(mLW21)

#MODEL 22: #
#a(T#SC),b(T#SC)
LW22=function(a,aT,a2T,aOS,b,bT,b2T,bOS,sd){
  a=a+aT*FEBSCB_mat$Temperature_MATFEM+a2T*FEBSCB_mat$Temperature_MATFEM*(FEBSCB_mat$SHELL_CONDITION==2)+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  b=b+bT*FEBSCB_mat$Temperature_MATFEM+b2T*FEBSCB_mat$Temperature_MATFEM*(FEBSCB_mat$SHELL_CONDITION==2)+bOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,bT=0,bOS=0,b2T=0,sd=0.1)
mLW22=mle(LW22,start=param)
summary(mLW22)

#MODEL 23: #
#a(SC#Tr))b
LW23=function(a,aTr,a2Tr,aOS,b,sd){
  a=a+aTr*(FEBSCB_mat$Temperature_MATFEM>3.5)+a2Tr*(FEBSCB_mat$Temperature_MATFEM>3.5)*(FEBSCB_mat$SHELL_CONDITION==2)+aOS*(FEBSCB_mat$SHELL_CONDITION!=2)
  ave=log(FEBSCB_mat$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,a2Tr=0,aOS=0,b=3,sd=0.1)
mLW23=mle(LW23,start=param)
summary(mLW23)


##############################CLUTCH_SIZE (4 and 6)
#MODEL 24: #
#b(clutch)
names(FEBSCB_mat)
FEBSCB_mat%>% filter(CLUTCH_SIZE==4|CLUTCH_SIZE==6)->FEBSCB_clutch
nrow(FEBSCB_clutch)

LW24=function(a,bc,b,sd){
  b=b+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bc=0,sd=0.1)
mLW24=mle(LW24,start=param)
summary(mLW24)

#MODEL 25: #
#a(bc)b(bc)
LW25=function(a,ac,b,bc,sd){
  a=a+ac*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  b=b+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,ac=0,b=3,bc=0,sd=0.1)
mLW25=mle(LW25,start=param)
summary(mLW25)

#MODEL 26: #
#a(SC)b(clutch)
LW26=function(a,aOS,b,bc,sd){
  a=a+aOS*(FEBSCB_clutch$SHELL_CONDITION!=2)
  b=b+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bc=0,sd=0.1)
mLW26=mle(LW26,start=param)
summary(mLW26)

#MODEL 27: #
#a(SC#T)b(Mat#T)
LW27=function(a,aT,a2T,aOS,b,bc,bT,b2T,sd){
  a=a+aT*FEBSCB_clutch$Temperature_MATFEM+a2T*FEBSCB_clutch$Temperature_MATFEM*(FEBSCB_clutch$SHELL_CONDITION!=2)+aOS*(FEBSCB_clutch$SHELL_CONDITION!=2)
  b=b+bT*FEBSCB_clutch$Temperature_MATFEM+b2T*FEBSCB_clutch$Temperature_MATFEM*(FEBSCB_clutch$CLUTCH_SIZE!=4)+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T,aOS=0,b=3,bT=0,bc=0,b2T=0,sd=0.1)
mLW27=mle(LW27,start=param)
summary(mLW27)

#MODEL 28: #
#a(SC#T)b(Mat#SC)
LW28=function(a,aT,a2T,aOS,b,bc,bOS,b2OS,sd){
  a=a+aT*FEBSCB_clutch$Temperature_MATFEM+a2T*FEBSCB_clutch$Temperature_MATFEM*(FEBSCB_clutch$SHELL_CONDITION!=2)+aOS*(FEBSCB_clutch$SHELL_CONDITION!=2)
  b=b+bOS*(FEBSCB_clutch$SHELL_CONDITION!=2)+b2OS*(FEBSCB_clutch$SHELL_CONDITION!=2)*(FEBSCB_clutch$CLUTCH_SIZE!=4)+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,bOS=0,bc=0,b2OS=0,sd=0.1)
mLW28=mle(LW28,start=param)
summary(mLW28)

#MODEL 29: #
#a(SC)b(Mat#SC)
LW29=function(a,aOS,b,bc,bOS,b2OS,sd){
  a=a+aOS*(FEBSCB_clutch$SHELL_CONDITION!=2)
  b=b+bOS*(FEBSCB_clutch$SHELL_CONDITION!=2)+b2OS*(FEBSCB_clutch$SHELL_CONDITION!=2)*(FEBSCB_clutch$CLUTCH_SIZE!=4)+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bOS=0,bc=0,b2OS=0,sd=0.1)
mLW29=mle(LW29,start=param)
summary(mLW29)

#MODEL 30: #
#a(SC#T)b(Mat)
LW30=function(a,aT,a2T,aOS,b,bc,sd){
  a=a+aT*FEBSCB_clutch$Temperature_MATFEM+a2T*FEBSCB_clutch$Temperature_MATFEM*(FEBSCB_clutch$SHELL_CONDITION!=2)+aOS*(FEBSCB_clutch$SHELL_CONDITION!=2)
  b=b+bc*(FEBSCB_clutch$CLUTCH_SIZE!=4)
  ave=log(FEBSCB_clutch$WIDTH)*b+a
  -sum(dnorm(log(FEBSCB_clutch$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,bc=0,sd=0.1)
mLW30=mle(LW30,start=param)
summary(mLW30)

