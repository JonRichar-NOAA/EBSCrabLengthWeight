# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/4/2021
# Calculate width/weight regression models for EBSCB females by clutch-size AND compare across clutch_sizes WITH shell condition categories
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
############################ New shell  clutch 4, clutch 5 and 6 ###############################################################
######################################################################################################

###################################################################################################
############################ Clutch 4  ############################################################
###################################################################################################

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
p<-ggplot(ns_female, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.width,data=ns_female)
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
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
#dev.new()
p<-ggplot(ns_egg4females_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.width,data=ns_egg4females_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])

# log10(W) = -8.494581 + 3.060180  * log10(L) on transformed scale       #updated for females
# W = exp(-8.494581 * L^(3.060180)  on original scale                #updated for females
# a = 0.000204574                                               #updated for females
# b = 3.060180                                                    #updated for females
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
A                         #0.0002049898

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL CS 4 MODEL ###############################
# a = 0.0002049898   #updated for females
# b = 3.060180         #updated for females

###################################################################################################
############################ Clutch 5  ############################################################
###################################################################################################

plot(ns_egg5females$WEIGHT~ns_egg5females$WIDTH)
############################## Plot base data in GG plot ############################################################
#dev.new()
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
ns_female<-as.data.frame(cbind(ns_egg5females,YEAR,log.width,log.weight,log10.width,log10.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
#dev.new()
p<-ggplot(ns_female, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit3<-lm(log.weight~log.width,data=ns_female)
summary(fit3)
coef(fi3)
############################## check diagnostics #################################################
#dev.new()
par(mfrow=c(2,2))

plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit3)
ns_egg5females_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))

nrow(ns_female)-nrow(ns_egg5females_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_egg5females_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
#dev.new()
p<-ggplot(ns_egg5females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
#dev.new()
p<-ggplot(ns_egg5females_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log.weight~log.width,data=ns_egg5females_analysis)
summary(fit4)
coef(fit4)

cf4<-as.matrix(coef(fit4))

exp(cf4[1,1])

# log10(W) = -7.781367 + 2.902110  * log10(L) on transformed scale       #updated for females
# W = exp(-7.781367) * L^(2.902110)  on original scale                #updated for females
# a = 0.0004174411                                               #updated for females
# b = 2.902110                                                     #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf4
names(fit4)
names(summary(fit4))
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0004181243 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v4/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL CS 5 MODEL ###############################
# a = 0.0004181243    #updated for females
# b = 2.902110         #updated for females

###################################################################################################
############################ Clutch 6  ############################################################
###################################################################################################

#dev.new()

plot(ns_egg6females$WEIGHT~ns_egg6females$WIDTH)
p<-ggplot(ns_egg6females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_egg6females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_egg6females$WIDTH)
log.weight <- log(ns_egg6females$WEIGHT)

log10.width<-log10(ns_egg6females$WIDTH)
log10.weight <- log10(ns_egg6females$WEIGHT)
ns_c6_female<-as.data.frame(cbind(ns_egg6females,YEAR,log.width,log.weight,log10.width,log10.weight))   		 # Bind new data objects and crab data in data frame  
ns_c6_female                    							  		 # inspect data
names(ns_c6_female)											 # Check column names

nrow(ns_c6_female)
############################ Plot log transformeddata in GGplot #############################################
#dev.new()
p<-ggplot(ns_c6_female, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell clutch 6 females-log transformed")

############################## Fit initial model ########################################################

fit5<-lm(log.weight~log.width,data=ns_c6_female)
summary(fit5)
coef(fit5)

############################## check diagnostics #################################################
#dev.new()
par(mfrow=c(2,2))

plot(fit5)
plot(cooks.distance(fit5), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_c6_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_c6_female$Cooks_D <- cooks.distance(fit5)
ns_egg6females_analysis<-subset(ns_c6_female, Cooks_D < (4/(nrow(ns_c6_female))))

nrow(ns_c6_female)-nrow(ns_egg6females_analysis)    #32 obs removed

nrow(ns_egg6females_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
#dev.new()
p<-ggplot(ns_egg6females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="new shell clutch 6 females- infuential points removed")


############################ log transformed ##################################################
#dev.new()
p<-ggplot(ns_egg6females_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="new shell clutch 6 females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit6<-lm(log.weight~log.width,data=ns_egg6females_analysis)
summary(fit6)
coef(fit6)
cf6<-as.matrix(coef(fit6))

plot(log.weight~log.width,data=ns_egg6females_analysis, main = "New shell, CS4, 5 and 6")

abline(a=cf2[1,1],b=cf2[2,1],col=1)
abline(a=cf4[1,1],b=cf4[2,1],col=2)
abline(a=cf6[1,1],b=cf6[2,1],col=4)

exp(cf6[1,1])
# log10(W) = -3.197247 + 2.817620   * log10(L) on transformed scale       #updated for females
# W = exp(-3.197247) * L^(2.817620 )  on original scale              #updated for females
# a = 0.0006349701                                              #updated for females
# b = 2.817620                                                       #updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v6<-(summary(fit6)$sigma)**2  #Variance 
v6
int<-cf6[1,1]
A<-(exp(int)*10^(v6/2))
A                         #0.0006366521                               #updated for females

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit6)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*exp(v6/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
A
cf6[2,1]

# a = 0.0006366521
# b = 2.81762 

############################ combine data sets and plot, using shell condition as grouping factor############################################################
ns_egg4females_analysis$ClutchSize <- "4"
ns_egg5females_analysis$ClutchSize <- "5"
ns_egg6females_analysis$ClutchSize <- "6"

analysis_matfemales<-rbind(ns_egg4females_analysis,ns_egg5females_analysis,ns_egg6females_analysis)

#write.csv(analysis_matfemales,"EBS_CB_Analysis_matfemales.csv")

ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(Clutch 4, 5 and 6)females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(Clutch 5 and 6) log-transformed")


ebscb_sc_points<-ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(Clutch 5 and 6) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize,color = ClutchSize,shape=ClutchSize)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (Clutch 5 and 6, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize,color = ClutchSize,shape=ClutchSize)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(Clutch 5 and 6, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize,color = ClutchSize,shape=ClutchSize)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(Clutch 5 and 6) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log.width",y="log.weight",color="ClutchSize", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(ClutchSize))
  )


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*ClutchSize,data = analysis_matfemales)
summary(mod1)									# p-value = 0.157 on interaction term. Evidence slopes are not different, 0.00206 with CS4

mod2<-aov(log.weight~log.width+ClutchSize,data = analysis_matfemales)
summary(mod2)									# p-value for CS = 1.81e-12. Intercepts are significantly different, 1.81e-12 with CS4

anova(mod1,mod2)									# p-value = 0.1567....removing interaction impacts model, 0.002059 with CS4

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_matfemales)
summary(reg_mod)

######################################################################################################
############################ Old shell clutch 4, 5 and 6 ###############################################################
######################################################################################################

###################################################################################################
############################ Clutch 4  ############################################################
###################################################################################################
dev.new()
plot(os_egg4females$WEIGHT~os_egg4females$WIDTH)
############################## Plot base data in GG plot ############################################################
#dev.new()
p<-ggplot(os_egg4females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell clutch 4 females")

############################# Add fields for analysis ########################################################
Year <- substring(os_egg4females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_egg4females$WIDTH)
log.weight <- log(os_egg4females$WEIGHT)

log10.width<-log10(os_egg4females$WIDTH)
log10.weight <- log10(os_egg4females$WEIGHT)
os_c4_female<-as.data.frame(cbind(os_egg4females,YEAR,log.width,log.weight,log10.width,log10.weight))   		 # Bind new data objects and crab data in data frame  
os_c4_female                   							  		 # inspect data
names(os_c4_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
#dev.new()
p<-ggplot(os_c4_female, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell clutch 4 females-log transformed")


############################## Fit initial model ########################################################

fit7<-lm(log.weight~log.width,data=os_c4_female)
summary(fit7)
coef(fit7)
############################## check diagnostics #################################################
#dev.new()
par(mfrow=c(2,2))

plot(fit7)
plot(cooks.distance(fit7), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_c4_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_c4_female$Cooks_D <- cooks.distance(fit7)
os_egg4females_analysis<-subset(os_c4_female, Cooks_D < (4/(nrow(os_c4_female))))

nrow(os_c4_female)-nrow(os_egg4females_analysis)    # obs removed based on Cook's Distance
nrow(os_egg4females_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_egg4females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell clutch 4 females- infuential points removed")


############################ log transformed ##################################################
#dev.new()
p<-ggplot(os_egg4females_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell clutch 4 females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit8<-lm(log.weight~log.width,data=os_egg4females_analysis)
summary(fit8)
coef(fit8)

cf8<-as.matrix(coef(fit8))

exp(cf8[1,1])

# log(W) = -7.370200  + 2.825224   * log10(L) on transformed scale       #updated for females
# W = exp(-7.370200) * L^(2.825224 )  on original scale                #updated for females
# a = 0.0006297423                                                #updated for females
# b = 2.825224                                                      #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf8
names(fit8)
names(summary(fit8))
###############################################################################################
v8<-(summary(fit8)$sigma)**2  #Variance 
v8
int<-cf8[1,1]
A<-(exp(int)*exp(v8/2))
A                         #0.0006305698 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit8)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v8/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
A
cf8[2,1]
# a = 0.0006305698    #updated for females
# b = 2.825224        #updated for females


###################################################################################################
############################ Clutch 5  ############################################################
###################################################################################################
dev.new()
plot(os_egg5females$WEIGHT~os_egg5females$WIDTH)

############################## Plot base data in GG plot ############################################################
#dev.new()
p<-ggplot(os_egg5females, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell clutch 5 females")

############################# Add fields for analysis ########################################################
Year <- substring(os_egg5females$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_egg5females$WIDTH)
log.weight <- log(os_egg5females$WEIGHT)

log10.width<-log10(os_egg5females$WIDTH)
log10.weight <- log10(os_egg5females$WEIGHT)
os_c5_female<-as.data.frame(cbind(os_egg5females,YEAR,log.width,log.weight,log10.width,log10.weight))   		 # Bind new data objects and crab data in data frame  
os_c5_female                   							  		 # inspect data
names(os_c5_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
#dev.new()
p<-ggplot(os_c5_female, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell clutch 5 females-log transformed")


############################## Fit initial model ########################################################

fit9<-lm(log.weight~log.width,data=os_c5_female)
summary(fit9)
coef(fit9)
############################## check diagnostics #################################################
#dev.new()
par(mfrow=c(2,2))

plot(fit9)
plot(cooks.distance(fit9), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_c5_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_c5_female$Cooks_D <- cooks.distance(fit9)
os_egg5females_analysis<-subset(os_c5_female, Cooks_D < (4/(nrow(os_c5_female))))

nrow(os_c5_female)-nrow(os_egg5females_analysis)    # obs removed based on Cook's Distance
nrow(os_egg5females_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_egg5females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell clutch 5 females- infuential points removed")


############################ log transformed ##################################################
#dev.new()
p<-ggplot(os_egg5females_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell clutch 5 females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit10<-lm(log.weight~log.width,data=os_egg5females_analysis)
summary(fit10)
coef(fit10)

cf10<-as.matrix(coef(fit10))

exp(cf10[1,1])
# log10(W) = -7.369576  + 2.823430  * log10(L) on transformed scale       #updated for females
# W = exp(-7.369576) * L^(2.823430)  on original scale                #updated for females
# a = 0.0006301353                                                 #updated for females
# b = 2.823430                                                     #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf10
names(fit10)
names(summary(fit10))
###############################################################################################
v10<-(summary(fit10)$sigma)**2  #Variance 
v10
int<-cf10[1,1]
A<-(exp(int)*exp(v10/2))
A                         #0.0006310285

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit10)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v10/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
cf10[2,1]
A
# a = 0.0006310285    #updated for females
# b = 2.82343        #updated for females

###################################################################################################
############################ Clutch 6 ############################################################
###################################################################################################
dev.new()
plot(os_egg6females$WEIGHT~os_egg6females$WIDTH)
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
os_c6_female<-as.data.frame(cbind(os_egg6females,YEAR,log.width,log.weight,log10.width,log10.weight))   		 # Bind new data objects and crab data in data frame  
os_c6_female                   							  		 # inspect data
names(os_c6_female)											 # Check column names

nrow(os_c6_female)
############################ Plot log transformeddata in GGplot #############################################
#dev.new()
p<-ggplot(os_c6_female, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) clutch 6 females-log transformed")

############################## Fit initial model ########################################################

fit11<-lm(log.weight~log.width,data=os_c6_female)
summary(fit11)
coef(fit11)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit11)
plot(cooks.distance(fit11), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_c6_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_c6_female$Cooks_D <- cooks.distance(fit11)
os_egg6females_analysis<-subset(os_c6_female, Cooks_D < (4/(nrow(os_c6_female))))

nrow(os_c6_female)-nrow(os_egg6females_analysis)    #32 obs removed

nrow(os_egg6females_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
par(mfrow=c(1,1))
#dev.new()
p<-ggplot(os_egg6females_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) clutch 6 females- infuential points removed")


############################ log transformed ##################################################
#dev.new()
p<-ggplot(os_egg6females_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) clutch 6 females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit12<-lm(log.weight~log.width,data=os_egg6females_analysis)
summary(fit12)
coef(fit12)
cf12<-as.matrix(coef(fit12))

plot(log.weight~log.width,data=os_egg6females_analysis, main = "SC3+SC4 CS 4 5 and 6")

abline(a=cf8[1,1],b=cf8[2,1],col=1)
abline(a=cf10[1,1],b=cf10[2,1],col=2)
abline(a=cf12[1,1],b=cf12[2,1],col=4)

exp(cf12[1,1])
# log10(W) = -7.375797 + 2.827141 * log10(L) on transformed scale       #updated for females
# W = exp(-7.375797) * L^(2.827141)  on original scale              #updated for females
# a = 0.0006262273                                               #updated for females
# b = 2.827141                                                     #updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v12<-(summary(fit12)$sigma)**2  #Variance 
v12
int<-cf12[1,1]
A<-(exp(int)*exp(v12/2))
A                         #0.0006270613                              #updated for females

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit12)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v12/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
cf12[2,1]
A
# a = 0.0006270613
# b = 2.827141 

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_egg4females_analysis$ClutchSize <- "4"
os_egg5females_analysis$ClutchSize <- "5"
os_egg6females_analysis$ClutchSize <- "6"

analysis_matfemales<-rbind(os_egg4females_analysis,os_egg5females_analysis,os_egg6females_analysis)

#write.csv(analysis_matfemales,"EBS_CB_Analysis_matfemales.csv")

ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))

q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell)females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB old shell clutch 5 and 6, log-transformed")


ebscb_sc_points<-ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize)) +
  geom_point(aes(colour = factor(ClutchSize)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB old shell clutch 5 and 6, log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize,color = ClutchSize,shape=ClutchSize)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB old shell clutch 5 and 6, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize,color = ClutchSize,shape=ClutchSize)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB old shell clutch 5 and 6, log-transformed")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = ClutchSize,color = ClutchSize,shape=ClutchSize)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB old shell clutch 5 and 6, log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log.width",y="log.weight",color="ClutchSize", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(ClutchSize))
  )


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*ClutchSize,data = analysis_matfemales)
summary(mod1)									# p-value = 0.855 on interaction term. No evidence slopes are different, 0.99235 with CS4 crab

mod2<-aov(log.weight~log.width+ClutchSize,data = analysis_matfemales)
summary(mod2)									# p-value for SC = 1.24e-05. Intercepts are significantly different, 0.0071 with CS4 crab

anova(mod1,mod2)									# p-value = 0.8548....removing interaction does not impact model, 0.9924 with CS4 crab

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_matfemales)
summary(reg_mod)