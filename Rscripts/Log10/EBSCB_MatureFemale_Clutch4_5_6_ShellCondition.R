# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/4/2021
# Calculate width/weight regression models for EBSCB females by clutch-size AND compare across shell condition categories
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBSCB_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)

colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log10(WIDTH),
#          logweight = log10(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
# filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
#      geom_point(aes(colour = factor(YEAR)))




########################## Aggregate by New shell/old shell #####################################

noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1)
ns_noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1& SHELL_CONDITION==2)

ns_immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==0& SHELL_CONDITION==2)

ns_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==2)
os_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

ns_egg4females<-subset(df1,SEX==2 & CLUTCH_SIZE==4 & SHELL_CONDITION==2)
os_egg4females<-subset(df1,SEX==2 & CLUTCH_SIZE==4 & SHELL_CONDITION==3|SHELL_CONDITION==4)

ns_egg5females<-subset(df1,SEX==2 & CLUTCH_SIZE==5 & SHELL_CONDITION==2)
os_egg5females<-subset(df1,SEX==2 & CLUTCH_SIZE==5 & SHELL_CONDITION==3|SHELL_CONDITION==4)

ns_egg6females<-subset(df1,SEX==2 & CLUTCH_SIZE==6 & SHELL_CONDITION==2)
os_egg6females<-subset(df1,SEX==2 & CLUTCH_SIZE==6 & SHELL_CONDITION==3|SHELL_CONDITION==4)


hist(ns_egg4females$WEIGHT)
hist(os_egg4females$WEIGHT)
hist(ns_egg5females$WEIGHT)
hist(os_egg5females$WEIGHT)
hist(ns_egg6females$WEIGHT)
hist(os_egg6females$WEIGHT)


######################################################################################################
############################ Clutch 4 ###############################################################
######################################################################################################

######################################################################################################
############################ New shell  ###############################################################
######################################################################################################


plot(ns_egg4females$WEIGHT~ns_egg4females$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_egg4females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_egg4females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_egg4females$WIDTH)
log.weight <- log(ns_egg4females$WEIGHT)

log10.width<-log10(ns_egg4females$WIDTH)
log10.weight <- log10(ns_egg4females$WEIGHT)
ns_female<-as.data.frame(cbind(ns_egg4females,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=ns_female)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit1)
ns_egg4females_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))

nrow(ns_female)-nrow(ns_egg4females_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_egg4females_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_egg4females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) FEmales- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_egg4females_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.width,data=ns_egg4females_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

10^(cf2[1,1])
# log10(W) = -3.37940 + 2.90211  * log10(L) on transformed scale       #updated for females
# W = exp(-3.37940) * L^(2.90211)  on original scale                #updated for females
# a = 0.0004174411                                                #updated for females
# b = 2.90211                                                     #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit2)
names(summary(fit2))
###############################################################################################
v2<-(summary(fit2)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(10^(int)*10^(v2/2))
A                         #0.0004177377

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL CS 4 MODEL ###############################
# a = 0.0004177377   #updated for females
# b = 2.90211          #updated for females


###################################################################################################
############################ Old shell ############################################################
###################################################################################################



dev.new()
plot(os_egg4females$WEIGHT~os_egg4females$WIDTH)
p<-ggplot(os_egg4females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_egg4females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_egg4females$WIDTH)
log.weight <- log(os_egg4females$WEIGHT)

log10.width<-log10(os_egg4females$WIDTH)
log10.weight <- log10(os_egg4females$WEIGHT)
os_female<-as.data.frame(cbind(os_egg4females,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names

nrow(os_female)
############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_female, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log10.weight~log10.width,data=os_female)
summary(fit3)
coef(fit3)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_female$Cooks_D <- cooks.distance(fit3)
os_egg4females_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))

nrow(os_female)-nrow(os_egg4females_analysis)    #32 obs removed

nrow(os_egg4females_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_eggfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_egg4females_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log10.weight~log10.width,data=os_egg4females_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=os_egg4females_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])
10^(cf4[1,1])
# log10(W) = -3.200466 + 2.823430  * log10(L) on transformed scale       #updated for females
# W = exp(-3.200466) * L^(2.823430)  on original scale              #updated for females
# a = 0.0006301353                                                #updated for females
# b = 2.823430                                                      #updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(10^(int)*10^(v4/2))
A                         #0.000630523                               #updated for females

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*exp(v4/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.000630523
# b = 2.823430 

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_egg4females_analysis$SC <- "OS"
ns_egg4females_analysis$SC <- "NS"

analysis_matfemales<-rbind(ns_egg4females_analysis,os_egg4females_analysis)

#write.csv(analysis_matfemales,"EBS_CB_Analysis_matfemales.csv")

ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
  geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
  geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell)females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
  geom_point(aes(colour = factor(SC)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


ebscb_sc_points<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
  geom_point(aes(colour = factor(SC)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
  )


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_matfemales)
summary(mod1)									# p-value = 0.855 on interaction term. No evidence slopes are different

mod2<-aov(log10.weight~log10.width+SC,data = analysis_matfemales)
summary(mod2)									# p-value for SC = 1.24e-05. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 0.8548....removing interaction does not impact model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_matfemales)
summary(reg_mod)

###################################################################################################
############################ Clutch 5 #############################################################
###################################################################################################

######################################################################################################
############################ New shell  ###############################################################
######################################################################################################


plot(ns_egg5females$WEIGHT~ns_egg5females$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_egg5females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_egg5females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_egg5females$WIDTH)
log.weight <- log(ns_egg5females$WEIGHT)

log10.width<-log10(ns_egg5females$WIDTH)
log10.weight <- log10(ns_egg5females$WEIGHT)
ns_female<-as.data.frame(cbind(ns_egg5females,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit5<-lm(log10.weight~log10.width,data=ns_female)
summary(fit5)
coef(fit5)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit5)
plot(cooks.distance(fit5), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit5)
ns_egg5females_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))

nrow(ns_female)-nrow(ns_egg5females_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_egg5females_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_egg5females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) FEmales- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_egg5females_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit6<-lm(log10.weight~log10.width,data=ns_egg5females_analysis)
summary(fit6)
coef(fit6)

cf6<-as.matrix(coef(fit6))

10^(cf6[1,1])
# log10(W) = -3.37940 + 2.90211  * log10(L) on transformed scale       #updated for females
# W = exp(-3.37940) * L^(2.90211)  on original scale                #updated for females
# a = 0.0004174411                                                #updated for females
# b = 2.90211                                                     #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf6
names(fit6)
names(summary(fit6))
###############################################################################################
v6<-(summary(fit6)$sigma)**2  #Variance 
v6
int<-cf6[1,1]
A<-(10^(int)*10^(v6/2))
A                         #0.0004177377

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit6)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v6/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL CS 5 MODEL ###############################
# a = 0.0004177377   #updated for females
# b = 2.90211          #updated for females



##############################################################################################################
############################## Old Shell #####################################################################
##############################################################################################################
dev.new()
plot(os_egg5females$WEIGHT~os_egg5females$WIDTH)
p<-ggplot(os_egg5females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_egg5females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_egg5females$WIDTH)
log.weight <- log(os_egg5females$WEIGHT)

log10.width<-log10(os_egg5females$WIDTH)
log10.weight <- log10(os_egg5females$WEIGHT)
os_female<-as.data.frame(cbind(os_egg5females,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names

nrow(os_female)
############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_female, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed")

############################## Fit initial model ########################################################

fit7<-lm(log10.weight~log10.width,data=os_female)
summary(fit7)
coef(fit7)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit7)
plot(cooks.distance(fit7), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_female$Cooks_D <- cooks.distance(fit7)
os_egg5females_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))

nrow(os_female)-nrow(os_egg5females_analysis)    #32 obs removed

nrow(os_egg5females_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_eggfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_egg5females_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit8<-lm(log10.weight~log10.width,data=os_egg5females_analysis)
summary(fit8)
coef(fit8)
cf8<-as.matrix(coef(fit8))

plot(log10.weight~log10.width,data=os_egg5females_analysis)
abline(a=cf6[1,1],b=cf6[2,1])
abline(a=cf8[1,1],b=cf8[2,1])

10^(cf8[1,1])
# log10(W) = -3.200566 + 2.823430  * log10(L) on transformed scale       #updated for females
# W = exp(-3.200566) * L^(2.823430)  on original scale              #updated for females
# a = 0.0006301353                                                #updated for females
# b = 2.823430                                                      #updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v8<-(summary(fit8)$sigma)**2  #Variance 
v8
int<-cf8[1,1]
A<-(10^(int)*10^(v8/2))
A                         #0.000630523                               #updated for females

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit8)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*exp(v8/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.000630523
# b = 2.823430 

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_egg5females_analysis$SC <- "OS"
ns_egg5females_analysis$SC <- "NS"

analysis_matfemales<-rbind(ns_egg5females_analysis,os_egg5females_analysis)

#write.csv(analysis_matfemales,"EBS_CB_Analysis_matfemales.csv")

ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
  geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
  geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell)females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
  geom_point(aes(colour = factor(SC)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


ebscb_sc_points<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
  geom_point(aes(colour = factor(SC)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell, CS5, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell, CS5, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell, CS5) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
  )


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_matfemales)
summary(mod1)									# p-value = 0.0349 on interaction term. Evidence slopes are different

mod2<-aov(log10.weight~log10.width+SC,data = analysis_matfemales)
summary(mod2)									# p-value for SC = <2e-16. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 0.03494....removing interaction impacts model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_matfemales)
summary(reg_mod)

######################################################################################################
############################ Clutch 6 ###############################################################
######################################################################################################

######################################################################################################
############################ New shell  ###############################################################
######################################################################################################


plot(ns_egg6females$WEIGHT~ns_egg6females$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_egg6females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_egg6females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_egg6females$WIDTH)
log.weight <- log(ns_egg6females$WEIGHT)

log10.width<-log10(ns_egg6females$WIDTH)
log10.weight <- log10(ns_egg6females$WEIGHT)
ns_female<-as.data.frame(cbind(ns_egg6females,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit9<-lm(log10.weight~log10.width,data=ns_female)
summary(fit9)
coef(fit9)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit9)
plot(cooks.distance(fit9), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit9)
ns_egg6females_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))

nrow(ns_female)-nrow(ns_egg6females_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_egg6females_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_egg6females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_egg6females_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit10<-lm(log10.weight~log10.width,data=ns_egg6females_analysis)
summary(fit10)
coef(fit10)

cf10<-as.matrix(coef(fit10))

10^(cf10[1,1])

# log10(W) = -3.197247  + 2.817620  * log10(L) on transformed scale       #updated for females
# W = exp(-3.197247) * L^(2.817620)  on original scale                #updated for females
# a = 0.0006349701                                                 #updated for females
# b = 2.817620                                                      #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf10
names(fit10)
names(summary(fit10))
###############################################################################################
v10<-(summary(fit10)$sigma)**2  #Variance 
v10
int<-cf10[1,1]
A<-(10^(int)*10^(v10/2))
A                         #0.0004567222

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit10)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v10/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
# a = 0.000635287    #updated for females
# b = 2.817620        #updated for females

###################################################################################################
############################ Old shell ############################################################
###################################################################################################

dev.new()
plot(os_egg6females$WEIGHT~os_eggfemales$WIDTH)
p<-ggplot(os_egg6females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_egg6females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_egg6females$WIDTH)
log.weight <- log(os_egg6females$WEIGHT)

log10.width<-log10(os_egg6females$WIDTH)
log10.weight <- log10(os_egg6females$WEIGHT)
os_female<-as.data.frame(cbind(os_egg6females,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names

nrow(os_female)
############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_female, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed")

############################## Fit initial model ########################################################

fit11<-lm(log10.weight~log10.width,data=os_female)
summary(fit11)
coef(fit11)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit11)
plot(cooks.distance(fit11), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_female$Cooks_D <- cooks.distance(fit11)
os_egg6females_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))

nrow(os_female)-nrow(os_egg6females_analysis)    #32 obs removed

nrow(os_egg6females_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_egg6females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_egg6females_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit12<-lm(log10.weight~log10.width,data=os_egg6females_analysis)
summary(fit12)
coef(fit12)
cf12<-as.matrix(coef(fit12))

plot(log10.weight~log10.width,data=os_egg6females_analysis)

abline(a=cf10[1,1],b=cf10[2,1])
abline(a=cf12[1,1],b=cf12[2,1])

10^(cf12[1,1])
# log10(W) = -3.203268 + 2.827141 * log10(L) on transformed scale       #updated for females
# W = exp(-3.203268) * L^(2.827141)  on original scale              #updated for females
# a = 0.0006262273                                               #updated for females
# b = 2.827141                                                     #updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v12<-(summary(fit12)$sigma)**2  #Variance 
v12
int<-cf12[1,1]
A<-(10^(int)*10^(v12/2))
A                         #0.0006265894                              #updated for females

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit12)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v12/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.0006265894
# b = 2.827141 

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_egg6females_analysis$SC <- "OS"
ns_egg6females_analysis$SC <- "NS"

analysis_matfemales<-rbind(ns_egg6females_analysis,os_egg6females_analysis)

#write.csv(analysis_matfemales,"EBS_CB_Analysis_matfemales.csv")

ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
  geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
  geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell)females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
  geom_point(aes(colour = factor(SC)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


ebscb_sc_points<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
  geom_point(aes(colour = factor(SC)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
  )


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_matfemales)
summary(mod1)									# p-value = 0.855 on interaction term. No evidence slopes are different

mod2<-aov(log10.weight~log10.width+SC,data = analysis_matfemales)
summary(mod2)									# p-value for SC = 1.24e-05. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 0.8548....removing interaction does not impact model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_matfemales)
summary(reg_mod)