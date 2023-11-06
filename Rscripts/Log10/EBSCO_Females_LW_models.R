# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 01/05/2022
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBSCO_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)

colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log10(WIDTH),
#          logweight = log10(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 #  filter(SEX == 2) -> female #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(female, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
#     geom_point(aes(colour = factor(SHELL_CONDITION)))


################################################################################

sc1_immatfemales<-subset(df1,SEX==2 & SHELL_CONDITION==1 & CLUTCH_SIZE==0)
sc2_immatfemales<-subset(df1,SEX==2 & SHELL_CONDITION==2 & CLUTCH_SIZE==0)

sc1_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==1 & CLUTCH_SIZE>0)
sc2_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==2 & CLUTCH_SIZE>0)
sc3_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==3 & CLUTCH_SIZE>0)
sc4_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==4 & CLUTCH_SIZE>0)
sc5_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==5 & CLUTCH_SIZE>0)

sc1_barrenfemales<-subset(df1,SEX==2 & SHELL_CONDITION==1 & CLUTCH_SIZE==1)
sc2_barrenfemales<-subset(df1,SEX==2 & SHELL_CONDITION==2 & CLUTCH_SIZE==1)
sc3_barrenfemales<-subset(df1,SEX==2 & SHELL_CONDITION==3 & CLUTCH_SIZE==1)
sc4_barrenfemales<-subset(df1,SEX==2 & SHELL_CONDITION==4 & CLUTCH_SIZE==1)
sc5_barrenfemales<-subset(df1,SEX==2 & SHELL_CONDITION==5 & CLUTCH_SIZE==1)

barrenfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==1)

hist(sc1_matfemales$WEIGHT)
hist(sc2_matfemales$WEIGHT)
hist(sc3_matfemales$WEIGHT)
hist(sc4_matfemales$WEIGHT)
hist(sc5_matfemales$WEIGHT)

hist(sc1_barrenfemales$WEIGHT)
hist(sc2_barrenfemales$WEIGHT)
hist(sc3_barrenfemales$WEIGHT)
hist(sc4_barrenfemales$WEIGHT)
hist(sc5_barrenfemales$WEIGHT)

hist(barrenfemales$WEIGHT)

hist(log10(sc2_matfemales$WEIGHT))
hist(sc3_matfemales$WEIGHT)
hist(sc4_matfemales$WEIGHT)

plot(sc2_matfemales$WEIGHT~sc2_matfemales$WIDTH)
plot(sc3_matfemales$WEIGHT~sc3_matfemales$WIDTH)
plot(sc4_matfemales$WEIGHT~sc4_matfemales$WIDTH)

########################## Aggregate by New shell/old shell #####################################
ns_immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==0 & SHELL_CONDITION==2)
ns_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0 & SHELL_CONDITION==2)
os_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0 & SHELL_CONDITION==3|SHELL_CONDITION==4)
os_matfemales





#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ Immature females ########################################################
######################################################################################################
plot(ns_immatfemales$WEIGHT~ns_immatfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_immatfemales, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_immatfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_immatfemales$WIDTH)
log.weight <- log(ns_immatfemales$WEIGHT)

log10.width<-log10(ns_immatfemales$WIDTH)
log10.weight <- log10(ns_immatfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_immatfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformed data in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) immature females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=ns_imfemale)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_imfemale)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale$Cooks_D <- cooks.distance(fit1)
ns_immatfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_immatfemales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_immatfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) immature females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit.im<-lm(log10.weight~log10.width,data=ns_immatfemales_analysis)
summary(fit.im)
coef(fit.im)

cf2<-as.matrix(coef(fit.im))

exp(-7.164450)
# log10(W) = -7.164450  + 2.781262  * log10(L) on transformed scale       #updated for females
# W = exp(-7.164450) * L^(2.781262)  on original scale                #updated for females
# a = 0.0007736043                                                  #updated for females
# b = 2.781262                                                     #updated for females
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
A                         #0.0007758883

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
# a = 0.0007758883   #updated for females
# b = 2.781262          #updated for females



######################################################################################################
############################ New shell ###############################################################
######################################################################################################

plot(ns_matfemales$WEIGHT~ns_matfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_matfemales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_matfemales$WIDTH)
log.weight <- log(ns_matfemales$WEIGHT)

log10.width<-log10(ns_matfemales$WIDTH)
log10.weight <- log10(ns_matfemales$WEIGHT)
ns_female<-as.data.frame(cbind(ns_matfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) females-log transformed")


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
ns_matfemales_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))
 
nrow(ns_female)-nrow(ns_matfemales_analysis)    #235 obs removed based on Cook's Distance

nrow(ns_matfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.width,data=ns_matfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
exp(-7.14223)
# log10(W) = -7.14223  + 2.797970 * log10(L) on transformed scale           #Updated for females
# W = exp(-7.14223) * L^(2.797970)  on original scale                   #Updated for females
# a = 0.0007909862                                                      #Updated for females
# b = 2.797970                                                          #Updated for females
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
A                         #0.000792414 

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
# a = 0.000792414                                                   #Updated for females
# b = 2.797970                                                      #Updated for females
###################################################################################################
############################ Old shell ############################################################
###################################################################################################

dev.new()
plot(os_matfemales$WEIGHT~os_matfemales$WIDTH)
p<-ggplot(os_matfemales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_matfemales$WIDTH)
log.weight <- log(os_matfemales$WEIGHT)

log10.width<-log10(os_matfemales$WIDTH)
log10.weight <- log10(os_matfemales$WEIGHT)
os_female<-as.data.frame(cbind(os_matfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names


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
os_matfemales_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))
 
nrow(os_female)-nrow(os_matfemales_analysis)    #123 obs removed

nrow(os_matfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_matfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_matfemales_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) opilio females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log10.weight~log10.width,data=os_matfemales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=os_matfemales_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

cf4
exp(-7.379058)

# log10(W) = -7.379058 + 2.865039 * log10(L) on transformed scale                     #Updated for females
# W = exp(-7.379058) * L^(2.865039)  on original scale                            #Updated for females
# a = 0.0006241886                                                                #Updated for females
# b = 2.865039                                                                    #Updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0006251401 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v4/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.0006251401                                                                  #Updated for females
# b = 2.865039                                                                      #Updated for females

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_matfemales_analysis$SC <- "OS"
ns_matfemales_analysis$SC <- "NS"

analysis_females<-rbind(ns_matfemales_analysis,os_matfemales_analysis)

write.csv(analysis_females,"EBS_CO_Analysis_females.csv")

ggplot(analysis_females, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_females, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="Female opilio (New shell and old shell)")

######################Log transformed #################################################
dev.new()


ggplot(analysis_females, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female opilio (New shell and old shell, log-transformed)")

ebsco_sc_points<-ggplot(analysis_females, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CO (New shell and old shell, log-transformed)")

# lines ONLY
ggplot(analysis_females, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female opilio (New shell and old shell, log-transformed)")

ebsco_sc_lines<-ggplot(analysis_females, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CO(New shell and old shell, log-transformed)")

# with points and lines
ggplot(analysis_females, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female opilio (New shell and old shell log-transformed)")


################ Alternative approach with lines
ggscatter(analysis_females, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_females)			# p = 3.16e-05--significant interaction -- slopes different
summary(mod1)

mod2<-aov(log10.weight~log10.width+SC,data = analysis_females)			# p< 2e-16: interceptes are significantly different
summary(mod2)

anova(mod1,mod2)										# p = 3.164e-05--interaction term is significant to model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_females)
summary(reg_mod)


########################################################################################################################
############################ Assess SC2 males ONLY by cold/warm year periods - Period 1 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_females<-subset(ns_matfemales_analysis, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_females<-subset(ns_matfemales_analysis, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)

warm_ns_females<-subset(warm_females,SC=="NS")

cold_ns_females<-subset(cold_females,SC=="NS")

mean(warm_ns_females$WEIGHT)
sd(warm_ns_females$WEIGHT)

mean(cold_ns_females$WEIGHT)
sd(cold_ns_females$WEIGHT)

warm_ns_females$TEMP <- "WARM"
cold_ns_females$TEMP <- "COLD"
cold_ns_females$COLOR <- 1
warm_ns_females$COLOR <- 4

temp_ns_females<-rbind(warm_ns_females,cold_ns_females)

nrow(cold_ns_females)
nrow(warm_ns_females)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log10.weight~log10.width,data=cold_ns_females)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log10.weight~log10.width,data=warm_ns_females)
summary(fit.warm)
coef.warm<-as.matrix(coef(fit.warm))

###################################################################################################
######################## Apply bias-correction procedure ##########################################
###################################################################################################

###################################################################################################
######################### COLD YEARS ##############################################################
v.cold<-(summary(fit.cold)$sigma)**2  #Variance 
v.cold
int<-cf.cold[1,1]
A<-(10^(int)*10^(v.cold/2))
A                         #0.0003686092

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(10^(int)*10^(v.warm/2))
A                         #0.0005069227
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.warm/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL MODEL ###############################
# a = 0.0003691035 
# b = 3.157545
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0005074622  
# b = 3.09928

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*TEMP,data = temp_ns_females)							# p = 0.218 on interaction term =  slopes not different
summary(mod1)									

mod2<-aov(log10.weight~log10.width+TEMP,data = temp_ns_females)							# p  = 0.999 for TEMP: temperature-based regression lines do not have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.2182...removing interaction term significantly affects model								

reg_mod<-lm(log10.weight~TEMP/log10.width-1,data = temp_ns_females)		
summary(reg_mod)

mod10<-anova(lm(log10.weight~log10.width,data = temp_ns_females),lm(log10.weight~log10.width*TEMP,data = temp_ns_females))
summary(mod10)
#################################Plot in GG plot ########################################################################
dev.new()

ggscatter(temp_ns_females, x="log10.width",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_females, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Mature female Opilio (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell, log-transformed)")

temp_ns_females
################################ with points and lines - custom color ##################################################
sp<-ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)



########################################################################################################################
############################ Assess SC2 females ONLY by cold/warm year periods - Period 2 - Five most extreme years for warm and cold ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_females<-subset(ns_matfemales_analysis, YEAR==2003|YEAR==2005|YEAR==2016|YEAR==2018|YEAR==2019)
cold_females<-subset(ns_matfemales_analysis, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)

warm_ns_females<-subset(warm_females,SC=="NS")

cold_ns_females<-subset(cold_females,SC=="NS")

mean(warm_ns_females$WEIGHT)
sd(warm_ns_females$WEIGHT)

mean(cold_ns_females$WEIGHT)
sd(cold_ns_females$WEIGHT)


warm_ns_females$TEMP <- "WARM"
cold_ns_females$TEMP <- "COLD"
cold_ns_females$COLOR <- 1
warm_ns_females$COLOR <- 4

temp_ns_females<-rbind(warm_ns_females,cold_ns_females)

nrow(cold_ns_females)
nrow(warm_ns_females)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log10.weight~log10.width,data=cold_ns_females)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log10.weight~log10.width,data=warm_ns_females)
summary(fit.warm)
coef.warm<-as.matrix(coef(fit.warm))

###################################################################################################
######################## Apply bias-correction procedure ##########################################
###################################################################################################

###################################################################################################
######################### COLD YEARS ##############################################################
v.cold<-(summary(fit.cold)$sigma)**2  #Variance 
v.cold
int<-cf.cold[1,1]
A<-(10^(int)*10^(v.cold/2))
A                         #0.0003686092

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(10^(int)*10^(v.warm/2))
A                         #0.0005069227
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.warm/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL MODEL ###############################
# a = 0.0003691035 
# b = 3.157545
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0005074622  
# b = 3.09928

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*TEMP,data = temp_ns_females)							# p = 0.00861 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log10.weight~log10.width+TEMP,data = temp_ns_females)							# p = 2.34e-06 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)													                                	# p = 0.00861...removing interaction term significantly affects model								

reg_mod<-lm(log10.weight~TEMP/log10.width-1,data = temp_ns_females)		
summary(reg_mod)

mod10<-anova(lm(log10.weight~log10.width,data = temp_ns_females),lm(log10.weight~log10.width*TEMP,data = temp_ns_females))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_females, x="log10.width",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_females, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Mature female Opilio (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell, log-transformed)")

temp_ns_females
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_females, aes(x = log10.width, y = log10.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Log10(Length)",y="Log10(Weight)", title="Mature female Opilio (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)