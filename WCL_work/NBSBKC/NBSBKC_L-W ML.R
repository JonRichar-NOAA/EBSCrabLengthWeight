library(stats4)
library(readxl)
library(tidyverse)
Male_NBSBKC_weights_and_temps <- read_csv("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/NBS_Data/NBS_BK_Analysis_males.csv")
Male_NBSBKC_weights_and_temps
MNBSBKC=Male_NBSBKC_weights_and_temps
#view(MNBSBKC)
nrow(MNBSBKC)

MNBSBKC%>%
  filter(SHELL_CONDITION==2)->NS_MALE

MNBSBKC%>%
  filter(SHELL_CONDITION>2)->OS_MALE

nrow(NS_MALE)
nrow(OS_MALE)
########################################################
#MODEL 1#
#Nul
LW1=function(a,b,sd){
  ave=log(MNBSBKC$LENGTH)*b+a
	-sum(dnorm(log(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
	}
param=list(a=-8,b=3,sd=0.1)
mLW1=mle(LW1,start=param)
summary(mLW1)

############################# RERUN MODEL 1 IN LOG10 ############################################
#MODEL 1#
#Nul
LW1a=function(a,b,sd){
  ave=log10(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log10(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-5,b=4,sd=0.1)
mLW1a=mle(LW1a,start=param)
summary(mLW1a)
#################################################################################################
#MODEL 2: #
#a(SC)
LW2=function(a,aOS,b,sd){
  a=a+aOS*(MNBSBKC$SHELL_CONDITION!=2)
  ave=log(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,aOS=0,sd=0.1)
mLW2=mle(LW2,start=param)
summary(mLW2)

###############################################RERUN MODEL 2 USING LOG10##################################################
#MODEL 2: #
#a(SC)
LW2a=function(a,aOS,b,sd){
  a=a+aOS*(MNBSBKC$SHELL_CONDITION!=2)
  ave=log10(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log10(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-5,b=4,aOS=0,sd=0.1)
mLW2a=mle(LW2a,start=param)
summary(mLW2a)
############################################################################################
#MODEL 3: #
#b(SC)
LW3=function(a,bOS,b,sd){
  b=b+bOS*(MNBSBKC$SHELL_CONDITION!=2)
  ave=log(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,b=3,bOS=0,sd=0.1)
mLW3=mle(LW3,start=param)
summary(mLW3)

################################### RERUN MODEL 3 USING LOG10 #########################################################
#MODEL 3: #
#b(SC)
LW3a=function(a,bOS,b,sd){
  b=b+bOS*(MNBSBKC$SHELL_CONDITION!=2)
  ave=log10(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log10(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-5,b=4,bOS=0,sd=0.1)
mLW3a=mle(LW3a,start=param)
summary(mLW3a)

#########################################################################################
#MODEL 4: #
#a(SC)b(SC)
LW4=function(a,aOS,b,bOS,sd){
  a=a+aOS*(MNBSBKC$SHELL_CONDITION!=2)
  b=b+bOS*(MNBSBKC$SHELL_CONDITION!=2)
  ave=log(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-8,aOS=0,b=3,bOS=0,sd=0.1)
mLW4=mle(LW4,start=param)
summary(mLW4)

################################### RERUN MODEL 4 USING LOG10 ######################################################
#MODEL 4: #
#a(SC)b(SC)
LW4a=function(a,aOS,b,bOS,sd){
  a=a+aOS*(MNBSBKC$SHELL_CONDITION!=2)
  b=b+bOS*(MNBSBKC$SHELL_CONDITION!=2)
  ave=log10(MNBSBKC$LENGTH)*b+a
  -sum(dnorm(log10(MNBSBKC$WEIGHT), mean=ave,sd=sd,log=TRUE))
}
param=list(a=-5,aOS=0,b=4,bOS=0,sd=0.1)
mLW4a=mle(LW4a,start=param)
summary(mLW4a)
