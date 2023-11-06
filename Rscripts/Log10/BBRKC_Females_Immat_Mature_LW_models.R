# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
getwd()
setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA")
df<-read.csv("BBRKC_weightDB_analysis.csv")

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

log10.length<-log10(ns_immatfemales$LENGTH)
log10.weight <- log10(ns_immatfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_immatfemales,YEAR,log10.length,log10.weight,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log10.length, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) immature females-log transformed")


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
ns_immatfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_immatfemales_analysis)    #10 obs removed based on Cook's Distance
nrow(ns_immatfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = log10.length, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) immature females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit.im<-lm(log10.weight~log10.length,data=ns_immatfemales_analysis)
summary(fit.im)
coef(fit.im)

cf2<-as.matrix(coef(fit.im))

exp(-7.656292)
# log10(W) = -7.656292  + 3.094701  * log10(L) on transformed scale       #updated for females
# W = exp(-7.656292) * L^(3.094701)  on original scale                #updated for females
# a = 0.0004730583                                                 #updated for females
# b = 3.094701                                                     #updated for females
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
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELLIMMATURE FEMALE MODEL ###############################
# a = 0.000474389    #updated for females
# b = 3.094701           #updated for females



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

log10.length<-log10(all_matfemales$LENGTH)
log10.weight <- log10(all_matfemales$WEIGHT)
all_female<-as.data.frame(cbind(all_matfemales,YEAR,log10.length,log10.weight,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
all_female                    							  		 # inspect data
names(all_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(all_female, aes(x = log10.length, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Mature females-log transformed")


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
ns_matfemales_analysis<-subset(all_female, Cooks_D < (4/(nrow(all_female))))
 
nrow(all_female)-nrow(ns_matfemales_analysis)    #60 obs removed based on Cook's Distance

nrow(ns_matfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = log10.length, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.length,data=ns_matfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
exp(-5.38851)
# log10(W) = -5.38851  + 2.616050  * log10(L) on transformed scale           #Updated for females
# W = exp(-5.38851) * L^(2.616050 )  on original scale                   #Updated for females
# a = 0.004568776                                                      #Updated for females
# b = 2.616050                                                          #Updated for females
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
A                         #0.004538776 

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
# a = 0.004538776                                                  #Updated for females
# b = 2.616050                                                      #Updated for females


