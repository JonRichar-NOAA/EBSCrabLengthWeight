library(stats4)
library(readxl)
library(tidyverse)

Male_EBSCO_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/data/Male_EBSCO_weights_temps_analysis.csv")

Male_EBSCO_weights_and_temps
colnames(Male_EBSCO_weights_and_temps)
Male_EBSCO_weights_and_temps%>% filter(Maturity == "Immature"|Maturity=="Mature") -> MEBSCO

# Sample size
nrow(MEBSCO)

#MODEL 1#
#Null

LW1=function(a,b,sd){
  ave=log(MEBSCO$WIDTH)*b+a
	-sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
	}
param=list(a=-8,b=3,sd=0.1)
mLW1=mle(LW1,start=param)

summary(mLW1)

#MODEL 2: #
#a(SC)
LW2=function(a,aOS,b,sd){
  a=a+aOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,aOS=0,sd=0.1)
mLW2=mle(LW2,start=param)
summary(mLW2)

#MODEL 3: #
#b(SC)
LW3=function(a,bOS,b,sd){
  b=b+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,sd=0.1)
mLW3=mle(LW3,start=param)
summary(mLW3)

#MODEL 4: #
#a(SC)b(SC)
LW4=function(a,aOS,b,bOS,sd){
  a=a+aOS*(MEBSCO$SHELL_CONDITION!=2)
  b=b+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bOS=0,sd=0.1)
mLW4=mle(LW4,start=param)
summary(mLW4)

#MODEL 5: #
MEBSCO$Temperature
MEBSCO$WIDTH
MEBSCO$WEIGHT

#a(T)
LW5=function(a,aT,b,sd){
  a=a+aT*MEBSCO$Temperature
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,sd=0.1)
mLW5=mle(LW5,start=param)
summary(mLW5)

#MODEL 6: #
#b(T)
LW6=function(a,bT,b,sd){
  b=b+bT*MEBSCO$Temperature
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,bT=0,b=3,sd=0.1)
mLW6=mle(LW6,start=param)
summary(mLW6)

#MODEL 7: #
#a(T)b(T)
LW7=function(a,aT,b,bT,sd){
  a=a+aT*MEBSCO$Temperature
  b=b+bT*MEBSCO$Temperature
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bT=0,sd=0.1)
mLW7=mle(LW7,start=param)
summary(mLW7)

#MODEL 8: #
#a(Tr)b(Tr)
LW8=function(a,aTr,b,bTr,sd){
  a=a+aTr*(MEBSCO$Temperature>1.5)
  b=b+bTr*(MEBSCO$Temperature>1.5)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-6,aTr=0,b=3,bTr=0,sd=0.1)
mLW8=mle(LW8,start=param)
summary(mLW8)


#MODEL 9: #
#a(T,T2)b(T,T2)
LW9=function(a,aT,aT2,b,bT,bT2,sd){
  a=a+aT*MEBSCO$Temperature+aT2^2*MEBSCO$Temperature
  b=b+bT*MEBSCO$Temperature+bT2^2*MEBSCO$Temperature
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aT2=0,b=3,bT=0,bT2=0,sd=0.1)
mLW9=mle(LW9,start=param)
summary(mLW9)

#MODEL 10: #
#a(T,SC)b(T)
LW10=function(a,aT,aOS,b,bT,sd){
  a=a+aT*MEBSCO$Temperature+aOS*(MEBSCO$SHELL_CONDITION!=2)
  b=b+bT*MEBSCO$Temperature
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,bT=0,sd=0.1)
mLW10=mle(LW10,start=param)
summary(mLW10)

#MODEL 11: #
#a(T)b(T,SC)
LW11=function(a,aT,b,bT,bOS,sd){
  a=a+aT*MEBSCO$Temperature
  b=b+bT*MEBSCO$Temperature+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bOS=0,bT=0,sd=0.1)
mLW11=mle(LW11,start=param)
summary(mLW11)

#MODEL 12: #
#a(T,SC)b(T,SC)
LW12=function(a,aT,aOS,b,bT,bOS,sd){
  a=a+aT*MEBSCO$Temperature+aOS*(MEBSCO$SHELL_CONDITION!=2)
  b=b+bT*MEBSCO$Temperature+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,bOS=0,bT=0,sd=0.1)
mLW12=mle(LW12,start=param)
summary(mLW12)


#MODEL 13: #
#a(SC,Tr)b(Tr)
LW13=function(a,aOS,aTr,b,bTr,sd){
  a=a+aTr*(MEBSCO$Temperature>1.5)+aOS*(MEBSCO$SHELL_CONDITION!=2)
  b=b+bTr*(MEBSCO$Temperature>1.5)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,aTr=0,b=3,bTr=0,sd=0.1)
mLW13=mle(LW13,start=param)
summary(mLW13)


#MODEL 14: #
#a(Tr)b(SC,Tr)
LW14=function(a,aTr,b,bOS,bTr,sd){
  a=a+aTr*(MEBSCO$Temperature>1.5)
  b=b+bTr*(MEBSCO$Temperature>1.5)+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,b=3,bOS=0,bTr=0,sd=0.1)
mLW14=mle(LW14,start=param)
summary(mLW14)


#MODEL 15: #
#a(SC,Tr)b(SC,Tr)
LW15=function(a,aOS,aTr,b,bOS,bTr,sd){
  a=a+aTr*(MEBSCO$Temperature>1.5)+aOS*(MEBSCO$SHELL_CONDITION!=2)
  b=b+bTr*(MEBSCO$Temperature>1.5)+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,aTr=0,b=3,bOS=0,bTr=0,sd=0.1)
mLW15=mle(LW15,start=param)
summary(mLW15)

#MODEL 16: #
#a(SC,T)b
LW16=function(a,aT,aOS,b,sd){
  a=a+aT*MEBSCO$Temperature+aOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,sd=0.1)
mLW16=mle(LW16,start=param)
summary(mLW16)

#MODEL 17: #
#a,b(T,SC)
LW17=function(a,b,bT,bOS,sd){
  b=b+bT*MEBSCO$Temperature+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,bT=0,sd=0.1)
mLW17=mle(LW17,start=param)
summary(mLW17)

#MODEL 18: #
#a(SC,T(SC))b
LW18=function(a,a2T,aOS,b,sd){
  a=a+a2T*MEBSCO$Temperature*(MEBSCO$SHELL_CONDITION==2)+aOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,a2T=0,aOS=0,b=3,sd=0.1)
mLW18=mle(LW18,start=param)
summary(mLW18)

#MODEL 19: #
#a(SC#T))b
LW19=function(a,aT,a2T,aOS,b,sd){
  a=a+aT*MEBSCO$Temperature+a2T*MEBSCO$Temperature*(MEBSCO$SHELL_CONDITION==2)+aOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,sd=0.1)
mLW19=mle(LW19,start=param)
summary(mLW19)

#MODEL 20: #
#a,b(T(SC),SC)
LW20=function(a,b,b2T,bOS,sd){
  b=b+b2T*MEBSCO$Temperature*(MEBSCO$SHELL_CONDITION==2)+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,b2T=0,sd=0.1)
mLW20=mle(LW20,start=param)
summary(mLW20)

#MODEL 21: #
#a,b(T#SC)
LW21=function(a,b,bT,b2T,bOS,sd){
  b=b+bT*MEBSCO$Temperature+b2T*MEBSCO$Temperature*(MEBSCO$SHELL_CONDITION==2)+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bT=0,bOS=0,b2T=0,sd=0.1)
mLW21=mle(LW21,start=param)
summary(mLW21)

#MODEL 22: #
#a(T#SC),b(T#SC)
LW22=function(a,aT,a2T,aOS,b,bT,b2T,bOS,sd){
  a=a+aT*MEBSCO$Temperature+a2T*MEBSCO$Temperature*(MEBSCO$SHELL_CONDITION==2)+aOS*(MEBSCO$SHELL_CONDITION!=2)
  b=b+bT*MEBSCO$Temperature+b2T*MEBSCO$Temperature*(MEBSCO$SHELL_CONDITION==2)+bOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,bT=0,bOS=0,b2T=0,sd=0.1)
mLW22=mle(LW22,start=param)
summary(mLW22)

#MODEL 23: #
#a(SC#Tr))b
LW23=function(a,aTr,a2Tr,aOS,b,sd){
  a=a+aTr*(MEBSCO$Temperature>1.5)+a2Tr*(MEBSCO$Temperature>1.5)*(MEBSCO$SHELL_CONDITION==2)+aOS*(MEBSCO$SHELL_CONDITION!=2)
  ave=log(MEBSCO$WIDTH)*b+a
  -sum(dnorm(log(MEBSCO$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,a2Tr=0,aOS=0,b=3,sd=0.1)
mLW23=mle(LW23,start=param)
summary(mLW23)
