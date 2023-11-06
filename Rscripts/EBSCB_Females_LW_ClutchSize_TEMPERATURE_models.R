# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/4/2021
# Calculate width/weight regression models for EBSCB females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBSCB_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)

colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log(WIDTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
# filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
#      geom_point(aes(colour = factor(YEAR)))




########################## Aggregate by New shell/old shell #####################################

noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1)
ns_noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1& SHELL_CONDITION==2)
ns_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==2)
os_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

######################################################################################################
############################ Nonegg-bearing females SC2 only ########################################################
######################################################################################################
plot(ns_noeggfemales$WEIGHT~ns_noeggfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_noeggfemale, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Non-egg bearing, SC2")

############################# Add fields for analysis ########################################################
Year <- substring(ns_noeggfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_noeggfemales$WIDTH)
log.weight <- log(ns_noeggfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_noeggfemales,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Non egg-bearing females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.width,data=ns_imfemale)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale$Cooks_D <- cooks.distance(fit1)
ns_noeggfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_noeggfemales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_noeggfemales_analysis)

######################################################################################################
############################ New shell Mature females###############################################################
######################################################################################################



plot(ns_eggfemales$WEIGHT~ns_eggfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_eggfemales, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_eggfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_eggfemales$WIDTH)
log.weight <- log(ns_eggfemales$WEIGHT)
ns_female<-as.data.frame(cbind(ns_eggfemales,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
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
ns_eggfemales_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))

nrow(ns_female)-nrow(ns_eggfemales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_eggfemales_analysis)

########################################################################################################################
########################################################################################################################
############################ Assess SC2 females ONLY by cold/warm year periods method 1 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_ns_eggfemales<-subset(ns_eggfemales_analysis, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_ns_eggfemales<-subset(ns_eggfemales_analysis, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)



warm_ns_eggfemales$TEMP <- "WARM"
cold_ns_eggfemales$TEMP <- "COLD"
cold_ns_eggfemales$COLOR <- 1
warm_ns_eggfemales$COLOR <- 4

temp_ns_eggfemales<-rbind(warm_ns_eggfemales,cold_ns_eggfemales)

nrow(cold_ns_eggfemales)
nrow(warm_ns_eggfemales)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.width,data=cold_ns_eggfemales)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.width,data=warm_ns_eggfemales)
summary(fit.warm)
coef(fit.warm)
coef.warm<-as.matrix(coef(fit.warm))

###################################################################################################
######################## Apply bias-correction procedure ##########################################
###################################################################################################

###################################################################################################
######################### COLD YEARS ##############################################################
v.cold<-(summary(fit.cold)$sigma)**2  #Variance 
v.cold
int<-cf.cold[1,1]
A<-(exp(int)*exp(v.cold/2))
A                         #0.0005052078  

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(exp(int)*exp(v.warm/2))
A                         #0.0003868361 
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
# a = 0.0002310341 
# b = 3.126483 
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0002521992  
# b = 3.105468

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*TEMP,data = temp_ns_eggfemales)							# p = 0.0104 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.width+TEMP,data = temp_ns_eggfemales)							# p = 0.446 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.width-1,data = temp_ns_eggfemales)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.width,data = temp_ns_eggfemales),lm(log.weight~log.width*TEMP,data = temp_ns_eggfemales))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_eggfemales, x="log.width",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_points<-ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_eggfemales, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Male EBS CB (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_lines <- ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

temp_ns_eggfemales
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_temp_points, ebscb_temp_lines +rremove("x.text"),
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
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_ns_eggfemales<-subset(analysis_eggfemales, YEAR==2003|YEAR==2005|YEAR==2016|YEAR==2018|YEAR==2019)
cold_ns_eggfemales<-subset(analysis_eggfemales, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)


warm_ns_eggfemales$TEMP <- "WARM"
cold_ns_eggfemales$TEMP <- "COLD"
cold_ns_eggfemales$COLOR <- 1
warm_ns_eggfemales$COLOR <- 4

temp_ns_eggfemales<-rbind(warm_ns_eggfemales,cold_ns_eggfemales)

nrow(cold_ns_eggfemales)
nrow(warm_ns_eggfemales)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.width,data=cold_ns_eggfemales)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.width,data=warm_ns_eggfemales)
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
A<-(exp(int)*exp(v.cold/2))
A                         #0.0002310341  

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(exp(int)*exp(v.warm/2))
A                         #0.0002521992 
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
# a = 0.0002310341 
# b = 3.126483 
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0002521992  
# b = 3.105468

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*TEMP,data = temp_ns_eggfemales)							# p = 0.0104 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.width+TEMP,data = temp_ns_eggfemales)							# p = 0.446 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.width-1,data = temp_ns_eggfemales)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.width,data = temp_ns_eggfemales),lm(log.weight~log.width*TEMP,data = temp_ns_eggfemales))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_eggfemales, x="log.width",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

ebscb_temp_points<-ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_eggfemales, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Female EBS CB (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

ebscb_temp_lines <- ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell, log-transformed)")

temp_ns_eggfemales
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_eggfemales, aes(x = log.width, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_temp_points, ebscb_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)





########################################################################################################################
########################################################################################################################
############################ Assess non eggbearing females ONLY by cold/warm year periods method 1 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_ns_noeggfemales<-subset(ns_noeggfemales_analysis, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_ns_noeggfemales<-subset(ns_noeggfemales_analysis, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)


warm_ns_noeggfemales$TEMP <- "WARM"
cold_ns_noeggfemales$TEMP <- "COLD"
cold_ns_noeggfemales$COLOR <- 1
warm_ns_noeggfemales$COLOR <- 4

temp_ns_noeggfemales<-rbind(warm_ns_noeggfemales,cold_ns_noeggfemales)

nrow(cold_ns_noeggfemales)
nrow(warm_ns_noeggfemales)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.width,data=cold_ns_noeggfemales)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.width,data=warm_ns_noeggfemales)
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
A<-(exp(int)*exp(v.cold/2))
A                         #0.0002310341  

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(exp(int)*exp(v.warm/2))
A                         #0.0002521992 
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
# a = 0.0002310341 
# b = 3.126483 
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0002521992  
# b = 3.105468

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*TEMP,data = temp_ns_noeggfemales)							# p = 0.0104 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.width+TEMP,data = temp_ns_noeggfemales)							# p = 0.446 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.width-1,data = temp_ns_noeggfemales)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.width,data = temp_ns_noeggfemales),lm(log.weight~log.width*TEMP,data = temp_ns_noeggfemales))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_noeggfemales, x="log.width",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_points<-ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_noeggfemales, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Male EBS CB (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_lines <- ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

temp_ns_noeggfemales
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_temp_points, ebscb_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)


########################################################################################################################
############################ Assess non eggbearing females ONLY by cold/warm year periods method 2 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_ns_noeggfemales<-subset(noeggfemales_analysis, YEAR==2003|YEAR==2005|YEAR==2016|YEAR==2018|YEAR==2019)
cold_ns_noeggfemales<-subset(noeggfemales_analysis, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)


warm_ns_noeggfemales$TEMP <- "WARM"
cold_ns_noeggfemales$TEMP <- "COLD"
cold_ns_noeggfemales$COLOR <- 1
warm_ns_noeggfemales$COLOR <- 4

temp_ns_noeggfemales<-rbind(warm_ns_noeggfemales,cold_ns_noeggfemales)

nrow(cold_ns_noeggfemales)
nrow(warm_ns_noeggfemales)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.width,data=cold_ns_noeggfemales)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.width,data=warm_ns_noeggfemales)
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
A<-(exp(int)*exp(v.cold/2))
A                         #0.0002310341  

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(exp(int)*exp(v.warm/2))
A                         #0.0002521992 
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
# a = 0.0002310341 
# b = 3.126483 
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0002521992  
# b = 3.105468

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*TEMP,data = temp_ns_noeggfemales)							# p = 0.0104 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.width+TEMP,data = temp_ns_noeggfemales)							# p = 0.446 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.width-1,data = temp_ns_noeggfemales)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.width,data = temp_ns_noeggfemales),lm(log.weight~log.width*TEMP,data = temp_ns_noeggfemales))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_noeggfemales, x="log.width",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

ebscb_temp_points<-ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_noeggfemales, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Female EBS CB (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

ebscb_temp_lines <- ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell, log-transformed)")

temp_ns_noeggfemales
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_noeggfemales, aes(x = log.width, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Female EBS CB (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_temp_points, ebscb_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)

