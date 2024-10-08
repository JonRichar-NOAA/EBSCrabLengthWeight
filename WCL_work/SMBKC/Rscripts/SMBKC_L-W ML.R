library(stats4)
library(readxl)
library(tidyverse)
library(MuMIn)
Male_SMBKC_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/SMBKC/data/Male_SMBKC_weights_temps_analysis.csv")
Male_SMBKC_weights_and_temps
MSMBKC=Male_SMBKC_weights_and_temps

nrow(MSMBKC)

names(MSMBKC)

min(MSMBKC$Temperature)
max(MSMBKC$Temperature)
########################################################
#MODEL 1#
#Nul
LW1=function(a,b,sd){
  ave=log(MSMBKC$LENGTH)*b+a
	-sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
	}
param=list(a=-8,b=3,sd=0.1)
mLW1=mle(LW1,start=param)
summary(mLW1)

############################# RERUN MODEL 1 IN LOG10 ############################################
#MODEL 1#
#Nul
LW1a=function(a,b,sd){
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,sd=0.1)
mLW1a=mle(LW1a,start=param)
summary(mLW1a)
#################################################################################################
#MODEL 2: #
#a(SC)
LW2=function(a,aOS,b,sd){
  a=a+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,aOS=0,sd=0.1)
mLW2=mle(LW2,start=param)
summary(mLW2)
############################################### RERUN MODEL 2 USING LOG10 #########################
#MODEL 2: #
#a(SC)
LW2a=function(a,aOS,b,sd){
  a=a+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,aOS=0,sd=0.1)
mLW2a=mle(LW2a,start=param)
summary(mLW2a)

###################################################################################################
#MODEL 3: #
#b(SC)
LW3=function(a,bOS,b,sd){
  b=b+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,sd=0.1)
mLW3=mle(LW3,start=param)
summary(mLW3)

############################################ RERUN MODEL 3 USING LOG10 ############################################
#MODEL 3: #
#b(SC)
LW3a=function(a,bOS,b,sd){
  b=b+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,sd=0.1)
mLW3a=mle(LW3a,start=param)
summary(mLW3a)
###################################################################################################################
#MODEL 4: #
#a(SC)b(SC)
LW4=function(a,aOS,b,bOS,sd){
  a=a+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bOS=0,sd=0.1)
mLW4=mle(LW4,start=param)
summary(mLW4)

################################################ RERUN MODEL 4 USING LOG10 ############################################################
#MODEL 4: #
#a(SC)b(SC)
LW4a=function(a,aOS,b,bOS,sd){
  a=a+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bOS=0,sd=0.1)
mLW4a=mle(LW4a,start=param)
summary(mLW4a)

########################################################################################################################################
############################################### Temperature models #####################################################################
############################################### NONE CURRENTLY RUN #####################################################################
########################################################################################################################################
#MODEL 5: #
#a(T)
LW5=function(a,aT,b,sd){
  a=a+aT*MSMBKC$Temperature
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,sd=0.1)
mLW5=mle(LW5,start=param)
summary(mLW5)

#MODEL 6: #
#b(T)
LW6=function(a,bT,b,sd){
  b=b+bT*MSMBKC$Temperature
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,bT=0,b=3,sd=0.1)
mLW6=mle(LW6,start=param)
summary(mLW6)

#MODEL 7: #
#a(T)b(T)
LW7=function(a,aT,b,bT,sd){
  a=a+aT*MSMBKC$Temperature
  b=b+bT*MSMBKC$Temperature
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bT=0,sd=0.1)
mLW7=mle(LW7,start=param)
summary(mLW7)

#MODEL 8: #
#a(Tr)b(Tr)
LW8=function(a,aTr,b,bTr,sd){
  a=a+aTr*(MSMBKC$Temperature>2.0)
  b=b+bTr*(MSMBKC$Temperature>2.0)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,b=3,bTr=0,sd=0.1)
mLW8=mle(LW8,start=param)
summary(mLW8)

#MODEL 9: #
#a(T,T2)b(T,T2)
LW9=function(a,aT,aT2,b,bT,bT2,sd){
  a=a+aT*MSMBKC$Temperature+aT2^2*MSMBKC$Temperature
  b=b+bT*MSMBKC$Temperature+bT2^2*MSMBKC$Temperature
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aT2=0,b=3,bT=0,bT2=0,sd=0.1)
mLW9=mle(LW9,start=param)
summary(mLW9)

#MODEL 10: #
#a(T,SC)b(T)
LW10=function(a,aT,aOS,b,bT,sd){
  a=a+aT*MSMBKC$Temperature+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bT*MSMBKC$Temperature
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,bT=0,sd=0.1)
mLW10=mle(LW10,start=param)
summary(mLW10)

#MODEL 11: #
#a(T)b(T,SC)
LW11=function(a,aT,b,bT,bOS,sd){
  a=a+aT*MSMBKC$Temperature
  b=b+bT*MSMBKC$Temperature+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,b=3,bOS=0,bT=0,sd=0.1)
mLW11=mle(LW11,start=param)
summary(mLW11)

#MODEL 12: #
#a(T,SC)b(T,SC)
LW12=function(a,aT,aOS,b,bT,bOS,sd){
  a=a+aT*MSMBKC$Temperature+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bT*MSMBKC$Temperature+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,bOS=0,bT=0,sd=0.1)
mLW12=mle(LW12,start=param)
summary(mLW12)
################################# RERUN MODEL 12 USING LOG10 #######################################
#MODEL 12: #
#a(T,SC)b(T,SC)
LW12a=function(a,aT,aOS,b,bT,bOS,sd){
  a=a+aT*MSMBKC$Temperature+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bT*MSMBKC$Temperature+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-5,aT=0,aOS=0,b=3,bOS=0,bT=0,sd=0.1)
mLW12a=mle(LW12a,start=param)
summary(mLW12a)
#######################################################################################

#MODEL 13: #
#a(SC,Tr)b(Tr)
LW13=function(a,aOS,aTr,b,bTr,sd){
  a=a+aTr*(MSMBKC$Temperature>3.5)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bTr*(MSMBKC$Temperature>3.5)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,aTr=0,b=3,bTr=0,sd=0.1)
mLW13=mle(LW13,start=param)
summary(mLW13)


#MODEL 14: #
#a(Tr)b(SC,Tr)
LW14=function(a,aTr,b,bOS,bTr,sd){
  a=a+aTr*(MSMBKC$Temperature>3.5)
  b=b+bTr*(MSMBKC$Temperature>3.5)+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,b=3,bOS=0,bTr=0,sd=0.1)
mLW14=mle(LW14,start=param)
summary(mLW14)


#MODEL 15: #
#a(SC,Tr)b(SC,Tr)
LW15=function(a,aOS,aTr,b,bOS,bTr,sd){
  a=a+aTr*(MSMBKC$Temperature>3.5)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bTr*(MSMBKC$Temperature>3.5)+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,aTr=0,b=3,bOS=0,bTr=0,sd=0.1)
mLW15=mle(LW15,start=param)
summary(mLW15)

#MODEL 16: #
#a(SC,T)b
LW16=function(a,aT,aOS,b,sd){
  a=a+aT*MSMBKC$Temperature+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,aOS=0,b=3,sd=0.1)
mLW16=mle(LW16,start=param)
summary(mLW16)

#MODEL 17: #
#a,b(T,SC)
LW17=function(a,b,bT,bOS,sd){
  b=b+bT*MSMBKC$Temperature+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,bT=0,sd=0.1)
mLW17=mle(LW17,start=param)
summary(mLW17)

#MODEL 18: #
#a(SC,T(SC))b
LW18=function(a,a2T,aOS,b,sd){
  a=a+a2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,a2T=0,aOS=0,b=3,sd=0.1)
mLW18=mle(LW18,start=param)
summary(mLW18)

#MODEL 19: #
#a(SC#T))b
LW19=function(a,aT,a2T,aOS,b,sd){
  a=a+aT*MSMBKC$Temperature+a2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,sd=0.1)
mLW19=mle(LW19,start=param)
summary(mLW19)

###################################### REDO MODEL 19 WITH LOG10 #########################
#MODEL 19: #
#a(SC#T))b
LW19a=function(a,aT,a2T,aOS,b,sd){
  a=a+aT*MSMBKC$Temperature+a2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,sd=0.1)
mLW19a=mle(LW19a,start=param)
summary(mLW19a)
##########################################################################################
#MODEL 20: #
#a,b(T(SC),SC)
LW20=function(a,b,b2T,bOS,sd){
  b=b+b2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,b2T=0,sd=0.1)
mLW20=mle(LW20,start=param)
summary(mLW20)

#MODEL 21: #
#a,b(T#SC)
LW21=function(a,b,bT,b2T,bOS,sd){
  b=b+bT*MSMBKC$Temperature+b2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bT=0,bOS=0,b2T=0,sd=0.1)
mLW21=mle(LW21,start=param)
summary(mLW21)

#MODEL 22: #
#a(T#SC),b(T#SC)
LW22=function(a,aT,a2T,aOS,b,bT,b2T,bOS,sd){
  a=a+aT*MSMBKC$Temperature+a2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bT*MSMBKC$Temperature+b2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aT=0,a2T=0,aOS=0,b=3,bT=0,bOS=0,b2T=0,sd=0.1)
mLW22=mle(LW22,start=param)
summary(mLW22)

###################################### RERUN MODEL 22 IN LOG10 ########################################################
#MODEL 22: #
#a(T#SC),b(T#SC)
LW22a=function(a,aT,a2T,aOS,b,bT,b2T,bOS,sd){
  a=a+aT*MSMBKC$Temperature+a2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  b=b+bT*MSMBKC$Temperature+b2T*MSMBKC$Temperature*(MSMBKC$SHELL_CONDITION==2)+bOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log10(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log10(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-6,aT=0,a2T=0,aOS=0,b=4,bT=0,bOS=0,b2T=0,sd=0.1)
mLW22a=mle(LW22a,start=param)
summary(mLW22a)

########################################################################################################################
#MODEL 23: #
#a(SC#Tr))b
LW23=function(a,aTr,a2Tr,aOS,b,sd){
  a=a+aTr*(MSMBKC$Temperature>3.5)+a2Tr*(MSMBKC$Temperature>3.5)*(MSMBKC$SHELL_CONDITION==2)+aOS*(MSMBKC$SHELL_CONDITION!=2)
  ave=log(MSMBKC$LENGTH)*b+a
  -sum(dnorm(log(MSMBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aTr=0,a2Tr=0,aOS=0,b=3,sd=0.1)
mLW23=mle(LW23,start=param)
summary(mLW23)
