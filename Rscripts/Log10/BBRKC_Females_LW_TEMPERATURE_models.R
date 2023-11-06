# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
getwd()
setwd("C:/Users/Jon.Richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
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
ns_nonovigfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1 & SHELL_CONDITION==2)

all_ovigfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1)

ns_ovigfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1& SHELL_CONDITION==2)





######################################################################################################
############################ nonovigerous females ########################################################
######################################################################################################
plot(ns_nonovigfemales$WEIGHT~ns_nonovigfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_nonovigfemales, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) nonovigurous females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_nonovigfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(ns_nonovigfemales$LENGTH)
log.weight <- log(ns_nonovigfemales$WEIGHT)
log10.length<-log10(ns_nonovigfemales$LENGTH)
log10.weight <- log10(ns_nonovigfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_nonovigfemales,YEAR,log10.length,log10.weight,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data

(ns_imfemale)											 # Check column names


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

nrow(ns_imfemale)-nrow(ns_nonovigfemales_analysis)    #10 obs removed based on Cook's Distance
nrow(ns_nonovigfemales_analysis)

######################################################################################################
############################ All ovigerous females ###############################################################
######################################################################################################

plot(all_ovigfemales$WEIGHT~all_ovigfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(all_ovigfemales, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Ovigurous females")

############################# Add fields for analysis ########################################################
Year <- substring(all_ovigfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(all_ovigfemales$LENGTH)
log.weight <- log(all_ovigfemales$WEIGHT)
log10.length<-log10(all_ovigfemales$LENGTH)
log10.weight <- log10(all_ovigfemales$WEIGHT)
all_female<-as.data.frame(cbind(all_ovigfemales,YEAR,log10.length,log10.weight,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
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


########################################################################################################################
############################ Assess SC2 females ONLY by cold/warm year periods ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

ns_ovigfemales_analysis$SC <- "NS"

warm_females<-subset(ns_ovigfemales_analysis, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_females<-subset(ns_ovigfemales_analysis, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)

warm_ns_females<-subset(warm_females,SC=="NS")

cold_ns_females<-subset(cold_females,SC=="NS")

warm_ns_females$TEMP <- "WARM"
cold_ns_females$TEMP <- "COLD"
cold_ns_females$COLOR <- 1
warm_ns_females$COLOR <- 4

temp_ns_females<-rbind(warm_ns_females,cold_ns_females)

nrow(cold_ns_females)
nrow(warm_ns_females)

###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log10.weight~log10.length,data=cold_ns_females)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log10.weight~log10.length,data=warm_ns_females)
summary(fit.warm)
coef(fit.warm)
cf.warm<-as.matrix(coef(fit.warm))

###################################################################################################
######################## Apply bias-correction procedure ##########################################
###################################################################################################

###################################################################################################
######################### COLD YEARS ##############################################################
v.cold<-(summary(fit.cold)$sigma)**2  #Variance 
v.cold
int<-cf.cold[1,1]
int
A.cold<-(10^(int)*10^(v.cold/2))
A.cold   # = 0.004078121                     

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.cold/2))
sdA

sdA_base<-10^(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-cf.warm[1,1]
int
A.warm<-(10^(int)*10^(v.warm/2))
A.warm                         # = 0.006293978
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]    #extract variance for intercept
sd<-sqrt(Av)               #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.warm/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL FEMALE MODEL ###############################
# a = 0.004078121 
# b = 2.636994
# SE.b = 0.02137
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.006293978
# b = 2.5427
# SE.b = 0.0283
########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.length*TEMP,data = temp_ns_females)							# p = 0.02429  on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log10.weight~log10.length+TEMP,data = temp_ns_females)							# p = 0.00869 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.02429...removing interaction term significantly affects model								

reg_mod<-lm(log10.weight~TEMP/log10.length-1,data = temp_ns_females)		
summary(reg_mod)

mod10<-anova(lm(log10.weight~log10.length,data = temp_ns_females),lm(log10.weight~log10.length*TEMP,data = temp_ns_females))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_females, x="log10.length",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_females, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Female BBRKC (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell, log-transformed)")

temp_ns_females
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)

########################################################################################################################
############################ Assess SC2 females ONLY by cold/warm year periods method 2 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2013, 2014, 2015, 2016, 2018, 2019

warm_ns_eggfemales<-subset(ns_eggfemales_analysis, YEAR==2001|YEAR==2014|YEAR==2016|YEAR==2018|YEAR==2019)
cold_ns_eggfemales<-subset(ns_eggfemales_analysis, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)


warm_ns_eggfemales$TEMP <- "WARM"
cold_ns_eggfemales$TEMP <- "COLD"
cold_ns_eggfemales$COLOR <- 1
warm_ns_eggfemales$COLOR <- 4

temp_ns_eggfemales<-rbind(warm_ns_eggfemales,cold_ns_eggfemales)

nrow(cold_ns_eggfemales)
nrow(warm_ns_eggfemales)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log10.weight~log10.length,data=cold_ns_eggfemales)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log10.weight~log10.length,data=warm_ns_eggfemales)
coef(fit.warm)
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
A.cold<-(10^(int)*10^(v.cold/2))
A.cold                        # 0.004005911   

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.cold/2))
sdA

sdA_base<-10^(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A.warm<-(10^(int)*10^(v.warm/2))
A.warm                         #0.007581225 
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
# a = 0.004005911
# b = 2.642179 
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.007581225 
# b = 2.505127

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.length*TEMP,data = temp_ns_eggfemales)							# p = 0.0505 on interaction term = significant interaction--suggests slopes differ
summary(mod1)									

mod2<-aov(log10.weight~log10.length+TEMP,data = temp_ns_eggfemales)							# p = 0.0222 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log10.weight~TEMP/log10.length-1,data = temp_ns_eggfemales)		
summary(reg_mod)

mod10<-anova(lm(log10.weight~log10.length,data = temp_ns_eggfemales),lm(log10.weight~log10.length*TEMP,data = temp_ns_eggfemales))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_eggfemales, x="log10.length",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_females, x="log10.length",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_females, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Female BBRKC (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell, log-transformed)")

temp_ns_females
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_females, aes(x = log10.length, y = log10.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)