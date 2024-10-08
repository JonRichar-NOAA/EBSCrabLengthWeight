library(stats4)
library(readxl)
library(tidyverse)
Female_BBRKC_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/BBRKC/Female_BBRKC_weights_temps_analysis.csv")

Female_BBRKC_weights_and_temps
FBBRKC=Female_BBRKC_weights_and_temps
FBBRKC%>% filter(CLUTCH_SIZE>=1 & SHELL_CONDITION ==2)->FBBRKC_mat

# Sample size
nrow(FBBRKC_mat)

#MODEL 1#
#Null
LW1=function(a,b,sd){
  ave=log(FBBRKC_mat$LENGTH)*b+a
  -sum(dnorm(log(FBBRKC_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,sd=0.1)
mLW1=mle(LW1,start=param)
summary(mLW1)


#MODEL 2: #
#a(T)
LW2=function(a,aT,b,sd){
  a=a+aT*FBBRKC_mat$Temperature_female
  ave=log(FBBRKC_mat$LENGTH)*b+a
  -sum(dnorm(log(FBBRKC_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,sd=0.1)
mLW2=mle(LW2,start=param)
summary(mLW2)

#MODEL 3: #
#b(T)
LW3=function(a,bT,b,sd){
  b=b+bT*FBBRKC_mat$Temperature_female
  ave=log(FBBRKC_mat$LENGTH)*b+a
  -sum(dnorm(log(FBBRKC_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,bT=0,b=3,sd=0.1)
mLW3=mle(LW3,start=param)
summary(mLW3)

#MODEL 4: #
#a(T)b(T)
LW4=function(a,aT,b,bT,sd){
  a=a+aT*FBBRKC_mat$Temperature_female
  b=b+bT*FBBRKC_mat$Temperature_female
  ave=log(FBBRKC_mat$LENGTH)*b+a
  -sum(dnorm(log(FBBRKC_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bT=0,sd=0.1)
mLW4=mle(LW4,start=param)
summary(mLW4)

#MODEL 5: #
#a(Tr)b(Tr)
LW5=function(a,aTr,b,bTr,sd){
  a=a+aTr*(FBBRKC_mat$Temperature_female>3.5)
  b=b+bTr*(FBBRKC_mat$Temperature_female>3.5)
  ave=log(FBBRKC_mat$LENGTH)*b+a
  -sum(dnorm(log(FBBRKC_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,b=3,bTr=0,sd=0.1)
mLW5=mle(LW5,start=param)
summary(mLW5)

#MODEL 6: #
#a(T,T2)b(T,T2)
LW6=function(a,aT,aT2,b,bT,bT2,sd){
  a=a+aT*FBBRKC_mat$Temperature_female+aT2^2*FBBRKC_mat$Temperature_female
  b=b+bT*FBBRKC_mat$Temperature_female+bT2^2*FBBRKC_mat$Temperature_female
  ave=log(FBBRKC_mat$LENGTH)*b+a
  -sum(dnorm(log(FBBRKC_mat$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aT2=0,b=3,bT=0,bT2=0,sd=0.1)
mLW6=mle(LW6,start=param)
summary(mLW6)


