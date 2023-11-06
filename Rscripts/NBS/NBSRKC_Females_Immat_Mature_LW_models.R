# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
getwd()
setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/NBS_Data")
df<-read.csv("NBS_RKC_DATA_FOR_SIZEWEIGHT_FEMALE.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log(WIDTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 #  filter(SEX == 2) -> female #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(female, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
#     geom_point(aes(colour = factor(SHELL_CONDITION)))


################################################################################

########################## Aggregate by New shell/old shell #####################################
ns_immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==0 & SHELL_CONDITION==2)

all_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0)





#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ Immature females ########################################################
######################################################################################################
plot(ns_immatfemales$WEIGHT~ns_immatfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_immatfemales, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_immatfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(ns_immatfemales$LENGTH)
log.weight <- log(ns_immatfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_immatfemales,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) immature females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.length,data=ns_imfemale)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_imfemale)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale

ns_imfemale$Cooks_D <- cooks.distance(fit1)
ns_immatfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_immatfemales_analysis)    #6 obs removed based on Cook's Distance
nrow(ns_immatfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) immature females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit.im<-lm(log.weight~log.length,data=ns_immatfemales_analysis)
summary(fit.im)
coef(fit.im)

cf2<-as.matrix(coef(fit.im))

exp(-7.721384)
# log(W) = -7.721384  + 3.098881  * log(L) on transformed scale       #updated for females
# W = exp(-7.721384) * L^(3.098881)  on original scale                #updated for females
# a = 0.0004432467                                                 #updated for females
# b = 3.098881                                                     #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit.im)
names(summary(fit.im))
###############################################################################################
v2<-(summary(fit.im)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.0004448441 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.im)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL  IMMATURE FEMALE MODEL ###############################
# a = 0.0004448441     #updated for females
# b = 3.098881           #updated for females



######################################################################################################
############################ All mature females ###############################################################
######################################################################################################

plot(all_matfemales$WEIGHT~all_matfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(all_matfemales, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Mature females")

############################# Add fields for analysis ########################################################
Year <- substring(all_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(all_matfemales$LENGTH)
log.weight <- log(all_matfemales$WEIGHT)
all_female<-as.data.frame(cbind(all_matfemales,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
all_female                    							  		 # inspect data
names(all_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(all_female, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Mature females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.length,data=all_female)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(all_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

all_female$Cooks_D <- cooks.distance(fit1)
ns_matfemales_analysis<-subset(all_female, Cooks_D < (4/(nrow(all_female))))
 
nrow(all_female)-nrow(ns_matfemales_analysis)    #11 obs removed based on Cook's Distance

nrow(ns_matfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.length,data=ns_matfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
exp(cf2[1,1])
# log(W) = -6.814391  + 2.902332  * log(L) on transformed scale           #Updated for NBS RKC females
# W = exp(-6.814391) * L^(2.902332)  on original scale                   #Updated for NBS RKC females
# a = 0.001097861                                                      #Updated for NBS RKC females
# b = 2.902332                                                          #Updated for NBS RKC females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit2)
names(summary(fit2))
###############################################################################################
v2<-(summary(fit2)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.001100768

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
# a = 0.001100768                                                  #Updated for NBS RKC females
# b = 2.902332                                                      #Updated for NBS RKC females


