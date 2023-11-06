# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for SMBKC females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA")

df<-read.csv("SMBKC_weightDB_analysis.csv")

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

sc2_immatfemales<-subset(df1,SEX==2 & SHELL_CONDITION==2 & CLUTCH_SIZE==0)

matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0)

hist(matfemales$CRUISE)
hist(matfemales$WEIGHT)
hist(sc2_immatfemales$WEIGHT)



########################## Aggregate by New shell/old shell #####################################
ns_immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==0 & SHELL_CONDITION==2)

all_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0)


all_matfemales


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

nrow(ns_imfemale)-nrow(ns_immatfemales_analysis)    #94 obs removed based on Cook's Distance
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

exp(cf2[1,1])
# log(W) = -8.402001  + 3.272900  * log(L) on transformed scale       #updated for females
# W = exp(-8.402001) * L^(3.272900)  on original scale                #updated for females
# a = 0.0002244178                                                 #updated for females
# b = 3.272900                                                    #updated for females
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
A                         #0.0002256236

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
# a = 0.0002256236    #updated for females
# b = 3.272900           #updated for females



          

########################################################################################################################
############################# All mature females together ###############################################################
########################################################################################################################

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

fit1a<-lm(log.weight~log.length,data=all_female)
summary(fit1a)
coef(fit1a)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1a)
plot(cooks.distance(fit1a), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(all_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

all_female$Cooks_D <- cooks.distance(fit1a)
all_matfemales_analysis<-subset(all_female, Cooks_D < (4/(nrow(all_female))))

nrow(all_female)-nrow(all_matfemales_analysis)    #5 obs removed based on Cook's Distance

nrow(all_matfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(all_matfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Mature females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(all_matfemales_analysis, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Mature females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2a<-lm(log.weight~log.length,data=all_matfemales_analysis)
summary(fit2a)
coef(fit2a)

cf2<-as.matrix(coef(fit2a))
exp(cf2[1,1])
# log(W) = -8.670425 + 3.341792 * log(L) on transformed scale           #Updated for females
# W = exp(-8.670425) * L^(3.341792)  on original scale                   #Updated for females
# a = 0.0001715862                                                     #Updated for females
# b = 3.341792                                                          #Updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit2a)
names(summary(fit2a))
###############################################################################################
v2<-(summary(fit2a)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.000172181

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2a)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
# a = 0.000172181                                                  #Updated for females
# b = 3.341792                                                      #Updated for females

########################################################################################################################################
############################ combine data sets and plot, using shell condition as grouping factor############################################################
names(all_matfemales_analysis)
names(ns_immatfemales_analysis)
ns_immatfemales_analysis$Mat <- "Immature"
all_matfemales_analysis$Mat <- "Mature"

analysis_males<-rbind(ns_immatfemales_analysis,all_matfemales_analysis)
names(analysis_males)


ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = Mat)) +
  geom_point(aes(colour = factor(Mat)))


q<-ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = Mat)) +
  geom_point(aes(colour = factor(Mat)))
q+ labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (Immature and mature)")

######################################################################################################################
########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = Mat)) +
  geom_point(aes(colour = factor(Mat)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed)")

bbrkc_Mat_points <- ggplot(analysis_males, aes(x = log.length, y = log.weight, group = Mat)) +
  geom_point(aes(colour = Mat))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = Mat,color = Mat,shape=Mat)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed)")


bbrkc_Mat_lines <-ggplot(analysis_males, aes(x = log.length, y = log.weight, group = Mat,color = Mat,shape=Mat)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed)")


############################# with points and lines#####################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = Mat,color = Mat,shape=Mat)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell log-transformed)")

######################  Alternative approach
ggscatter(analysis_males, x="log.length",y="log.weight",color="Mat", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(Mat))
  )
##################### Alternative 2 ####################################################
p<-ggplot(analysis_males, aes(x = log.length, y = log.weight), group = Mat,shape=Mat) +
  geom_point(aes(colour = Mat))
p+ labs(x="ln(length)",y="ln(weight)", title="SMBKC females(mature and immature)log transformed")+
  geom_abline(intercept = -8.402001,slope = 3.272900 ,color = "red")+
  geom_abline(intercept = -8.670425,slope = 3.341792 ,color = "blue") 

