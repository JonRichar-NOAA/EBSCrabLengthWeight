# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
getwd()
setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight")
df<-read.csv("DATA/BBRKC_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log10(WIDTH),
#          logweight = log10(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 #  filter(SEX == 2) -> female #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(female, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
#     geom_point(aes(colour = factor(SHELL_CONDITION)))


################################################################################

########################## Aggregate by New shell/old shell #####################################
ns_nonovigfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1 & SHELL_CONDITION==2)

all_ovigfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1)





#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ nonovigure females ########################################################
######################################################################################################
plot(ns_nonovigfemales$WEIGHT~ns_nonovigfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
#dev.new()
p<-ggplot(ns_nonovigfemales, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) nonovigure females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_nonovigfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(ns_nonovigfemales$LENGTH)
log.weight <- log(ns_nonovigfemales$WEIGHT)
log10.length<-log10(ns_nonovigfemales$LENGTH)
log10.weight <- log10(ns_nonovigfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_nonovigfemales,YEAR,log10.length,log10.weight,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformed data in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log10.length, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) nonovigerous females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.length,data=ns_imfemale)
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
ns_nonovigfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_nonovigfemales_analysis)    #13 obs removed based on Cook's Distance
nrow(ns_nonovigfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_nonovigfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) nonovigerous females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_nonovigfemales_analysis, aes(x = log10.length, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) nonovigerous females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit.im<-lm(log10.weight~log10.length,data=ns_nonovigfemales_analysis)
summary(fit.im)
coef(fit.im)

cf2<-as.matrix(coef(fit.im))

exp(cf2[1,1])
# log10(W) = -7.582706  + 3.07793 * log10(L) on transformed scale       #updated for females
# W = exp(-7.582706) * L^(3.07793)  on original scale                #updated for females
# a = 0.0005091815                                                 #updated for females
# b = 3.07793                                                     #updated for females
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
A                         #0.000474389 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.im)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL NONOVIGEROUS FEMALE MODEL ###############################
# a = 0.0005106292    #updated for females
# b = 3.07793           #updated for females



######################################################################################################
############################ All ovigerous females ###############################################################
######################################################################################################

plot(all_ovigfemales$WEIGHT~all_ovigfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(all_ovigfemales, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="ovigure females")

############################# Add fields for analysis ########################################################
Year <- substring(all_ovigfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length <-log(all_ovigfemales$LENGTH)
log.weight <- log(all_ovigfemales$WEIGHT)
log10.length <-log10(all_ovigfemales$LENGTH)
log10.weight <- log10(all_ovigfemales$WEIGHT)
all_female<-as.data.frame(cbind(all_ovigfemales,YEAR,log10.length,log10.weight))   		 # Bind new data objects and crab data in data frame  
all_female                    							  		 # inspect data
names(all_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(all_female, aes(x = log10.length, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="ovigure females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.length,data=all_female)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(all_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

all_female$Cooks_D <- cooks.distance(fit1)
ns_ovigfemales_analysis<-subset(all_female, Cooks_D < (4/(nrow(all_female))))
 
nrow(all_female)-nrow(ns_ovigfemales_analysis)    #60 obs removed based on Cook's Distance

nrow(ns_ovigfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_ovigfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_ovigfemales_analysis, aes(x = log10.length, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.length,data=ns_ovigfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])
# log10(W) = -5.37042 + 2.608199 * log10(L) on transformed scale           #Updated for females
# W = exp(-5.37042) * L^(2.608199)  on original scale                   #Updated for females
# a = 0.004652175                                                      #Updated for females
# b = 2.608199                                                          #Updated for females
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
A                         #0.004667638

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OVIGEROUS MODEL ###############################
# a = 0.004667638                                                  #Updated for females
# b = 2.608199                                                     #Updated for females


