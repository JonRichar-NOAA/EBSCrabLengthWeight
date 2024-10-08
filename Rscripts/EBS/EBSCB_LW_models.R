# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate width/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBSCB_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
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


################################################################################

sc1_males<-subset(df1,SEX==1 & SHELL_CONDITION==1)
sc2_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)
sc3_males<-subset(df1,SEX==1 & SHELL_CONDITION==3)
sc4_males<-subset(df1,SEX==1 & SHELL_CONDITION==4)
sc5_males<-subset(df1,SEX==1 & SHELL_CONDITION==5)

hist(sc1_males$WEIGHT)
hist(sc2_males$WEIGHT)
hist(sc3_males$WEIGHT)
hist(sc4_males$WEIGHT)
hist(sc5_males$WEIGHT)

hist(log(sc2_males$WEIGHT))
hist(sc3_males$WEIGHT)
hist(sc4_males$WEIGHT)

plot(sc2_males$WEIGHT~sc2_males$WIDTH)
plot(sc3_males$WEIGHT~sc3_males$WIDTH)
plot(sc4_males$WEIGHT~sc4_males$WIDTH)

########################## Aggregate by New shell/old shell #####################################
ns_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)
os_males<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)
os_males





#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ New shell ###############################################################
######################################################################################################

plot(ns_males$WEIGHT~ns_males$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_males, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males")

############################# Add fields for analysis ########################################################
Year <- substring(ns_males$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_males$WIDTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_male                    							  		 # inspect data
names(ns_male)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_male, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.width,data=ns_male)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(ns_male)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_male$Cooks_D <- cooks.distance(fit1)
ns_males_analysis<-subset(ns_male, Cooks_D < (4/(nrow(ns_male))))
 
nrow(ns_male)-nrow(ns_males_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_males_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.width,data=ns_males_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

# log(W) = -8.204571  + 3.014254 * log(L) on transformed scale
# W = exp(-8.204571) * L^(3.014254)  on original scale
# a = 0.000273401
# b = 3.014254
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
A                         #0.0002743986

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
# a = 0.0002743986
# b = 3.014254
cf2[2,1]

dev.new()
plot(WEIGHT~WIDTH,data=ns_males_analysis, main = "New shell male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_males_analysis$WIDTH

g<-function(x) {0.0002743986 *x^(3.014254)} #bias corrected
f<-function(x) {0.000273401 *x^(3.014254)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x,g(x),col=2,lwd=2)#bias corrected
#lines(x,f(x),col=4,lwd=2)# not corrected
lines(x,h(x),col=4,lwd=2)# baseline
legend(40,1550, c("Baseline L-W model", "SC based L-W model"), col=c(4,2),lwd=c(2,2))

###################################################################################################
############################ Old shell ############################################################
###################################################################################################

dev.new()
plot(os_males$WEIGHT~os_males$WIDTH)
p<-ggplot(os_males, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) males")

############################# Add fields for analysis ########################################################
Year <- substring(os_males$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_males$WIDTH)
log.weight <- log(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_male                    							  		 # inspect data
names(os_male)											 # Check column names

nrow(os_male)
############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_male, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log.weight~log.width,data=os_male)
summary(fit3)
coef(fit3)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(os_male)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_male$Cooks_D <- cooks.distance(fit3)
os_males_analysis<-subset(os_male, Cooks_D < (4/(nrow(os_male))))
 
nrow(os_male)-nrow(os_males_analysis)    #32 obs removed

nrow(os_males_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_males_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_males_analysis, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log.weight~log.width,data=os_males_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log.weight~log.width,data=os_males_analysis)

abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])
exp(-8.478206)
# log(W) = -8.478206 + 3.091966 * log(L) on transformed scale
# W = exp(-8.478206) * L^(3.091966)  on original scale
# a = 0.0002079514
# b = 3.091966

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0002083584

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v4/2))
sdA                       #1.046375

sdA_base<-exp(sd)
sdA_base                  #1.044331
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.0002083584
# b = 3.091966

plot(WEIGHT~WIDTH,data=os_males_analysis, main = "Old shell male Bairdi")


x<-os_males_analysis$WIDTH
g<-function(x) {0.0002083584 *x^(3.091966)} #bias corrected
f<-function(x) {0.0002083584 *x^(3.091966)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x,g(x),col=2,lwd=2)#bias corrected
lines(x,f(x),col=4,lwd=2)# not corrected
lines(x,h(x),col=3,lwd=2)# baseline
legend(40,1800, c("Baseline L-W model", "SC based L-W model"), col=c(3,2),lwd=c(2,2))

############################ Plot above as two panel figure with both new shell and old shell males ##############################
dev.new()
par(mfrow =c(2,1))

# New shell
plot(WEIGHT~WIDTH,data=ns_males_analysis, main = "New shell male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_males_analysis$WIDTH

g<-function(x) {0.0002743986 *x^(3.014254)} #bias corrected
f<-function(x) {0.000273401 *x^(3.014254)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x,g(x),col=2,lwd=2)#bias corrected
#lines(x,f(x),col=4,lwd=2)# not corrected
lines(x,h(x),col=4,lwd=2)# baseline
legend(15,1550, c("Baseline L-W model", "SC based L-W model"), col=c(4,2),lwd=c(2,2))

#Old shell

plot(WEIGHT~WIDTH,data=os_males_analysis, main = "Old shell male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_males_analysis$WIDTH
g<-function(x) {0.0002083584 *x^(3.091966)} #bias corrected
#f<-function(x) {0.0002083584 *x^(3.091966)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x,g(x),col=2,lwd=2)#bias corrected
#lines(x,f(x),col=4,lwd=2)# not corrected
lines(x,h(x),col=4,lwd=2)# baseline
legend(38,1800, c("Baseline L-W model", "SC based L-W model"), col=c(4,2),lwd=c(2,2))

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_males_analysis$SC <- "OS"
ns_males_analysis$SC <- "NS"

analysis_males<-rbind(ns_males_analysis,os_males_analysis)

write.csv(analysis_males,"EBS_CB_Analysis_males.csv")

ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell) males")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_males, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell) log-transformed")


ebscb_sc_points<-ggplot(analysis_males, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_males, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_males, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_males, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_males, x="log.width",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*SC,data = analysis_males)
summary(mod1)									# p-value = 3.33e-11 on interaction term. Evidence slopes are different

mod2<-aov(log.weight~log.width+SC,data = analysis_males)
summary(mod2)									# p-value for SC = <2e-16. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 3.33e-11....removing interaction impacts model

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_males)
summary(reg_mod)




########################################################################################################################
########################################################################################################################
############################ Assess SC2 males ONLY by cold/warm year periods method 1 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_males<-subset(analysis_males, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_males<-subset(analysis_males, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)

warm_ns_males<-subset(warm_males,SC=="NS")

cold_ns_males<-subset(cold_males,SC=="NS")

warm_ns_males$TEMP <- "WARM"
cold_ns_males$TEMP <- "COLD"
cold_ns_males$COLOR <- 1
warm_ns_males$COLOR <- 4

temp_ns_males<-rbind(warm_ns_males,cold_ns_males)

nrow(cold_ns_males)
nrow(warm_ns_males)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.width,data=cold_ns_males)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.width,data=warm_ns_males)
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
A                         #0.0002649991  
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

mod1<-aov(log.weight~log.width*TEMP,data = temp_ns_males)							# p = 0.0105 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.width+TEMP,data = temp_ns_males)							# p = 3.57e-08 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.width-1,data = temp_ns_males)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.width,data = temp_ns_males),lm(log.weight~log.width*TEMP,data = temp_ns_males))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_males, x="log.width",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_points<-ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_males, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Male EBS CB (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_lines <- ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

temp_ns_males
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
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
############################ Assess SC2 males ONLY by cold/warm year periods method 2 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_males<-subset(analysis_males, YEAR==2003|YEAR==2005|YEAR==2016|YEAR==2018|YEAR==2019)
cold_males<-subset(analysis_males, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)

warm_ns_males<-subset(warm_males,SC=="NS")

cold_ns_males<-subset(cold_males,SC=="NS")

warm_ns_males$TEMP <- "WARM"
cold_ns_males$TEMP <- "COLD"
cold_ns_males$COLOR <- 1
warm_ns_males$COLOR <- 4

temp_ns_males<-rbind(warm_ns_males,cold_ns_males)

nrow(cold_ns_males)
nrow(warm_ns_males)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.width,data=cold_ns_males)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.width,data=warm_ns_males)
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

mod1<-aov(log.weight~log.width*TEMP,data = temp_ns_males)							# p = 0.0104 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.width+TEMP,data = temp_ns_males)							# p = 0.446 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.width-1,data = temp_ns_males)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.width,data = temp_ns_males),lm(log.weight~log.width*TEMP,data = temp_ns_males))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_males, x="log.width",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_points<-ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_males, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Male EBS CB (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

ebscb_temp_lines <- ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

temp_ns_males
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_males, aes(x = log.width, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CB (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_temp_points, ebscb_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)
##################################################################################################################################################


##########################################################################################################################################################
############################### Rerun shell condition models while limit data to same size ranges ##########################################################
###########################################################################################################################################################


#######################################################################################################################################################



x.lim=c(0,200)
?hist()
summary(os_males_analysis$WIDTH)


summary(ns_males_analysis$WIDTH)


dev.new()
par(mfrow=c(2,1))
hist(ns_males_analysis$WIDTH,breaks=30,main="New shell size frequency",xaxt='n',xlim=x.lim)
abline(v=mean(ns_males_analysis$WIDTH),col=4)
axis(side=1,at=seq(0,200,10),label=seq(0,200,10))
hist(os_males_analysis$WIDTH,breaks=20,main="Old shell size frequency",xaxt='n',xlim=x.lim)
abline(v=mean(os_males_analysis$WIDTH),col=4)
axis(side=1,at=seq(0,200,10),label=seq(0,200,10))



ns_males_analysis_sub<-subset(ns_males_analysis,WIDTH>40.0)


############################ Fit followup model ##########################################
fit5<-lm(log.weight~log.width,data=ns_males_analysis_sub)
summary(fit5)
coef(fit5)

cf5<-as.matrix(coef(fit5))
cf5
			# log(W) = -8.36005  + 3.04885 * log(L) on transformed scale
    			# W = exp(-8.36005)*L^(3.04885 )  on original scale
			# a = 0.0002340326
			# b = 3.04885

p<-ggplot(ns_males_analysis_sub, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = cf5[1,1],slope = cf5[2,1],color = "red")



####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################
analysis_males_sub<-as.data.frame(rbind(ns_males_analysis_sub,os_males_analysis))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell)")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males_sub, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_points_sub <- ggplot(analysis_males_sub, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log transformed and truncated at 40mmm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell, log-transformed)")


ebscb_sc_lines_sub <-ggplot(analysis_males_sub, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell,log transformed and truncated at 40mm)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub, x="log.width",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_sc_points_sub, ebscb_sc_lines_sub,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*SC,data = analysis_males_sub)
summary(mod1)									# p= 0.000205 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.width+SC,data = analysis_males_sub)
summary(mod2)									# p = 2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p= 0.000205...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_males_sub)		
summary(reg_mod)


########################################################################################################################
############################# Truncate further at width = 106 mm(based on old shell first quartile) ######################################################
ns_males_analysis_sub2<-subset(ns_males_analysis,WIDTH>106)
os_males_analysis_sub<-subset(os_males_analysis,WIDTH>106)

ncol(os_males_analysis_sub)
ncol(ns_males_analysis_sub2)
############################ Fit followup model ##########################################
fit6<-lm(log.weight~log.width,data=ns_males_analysis_sub2)
summary(fit6)
coef(fit6)

cf6<-as.matrix(coef(fit6))
			# log(W) = -9.03317  + 3.19235  * log(L) on transformed scale
    			# W = exp(-9.03317*L^(3.19235  )  on original scale
			# a =  0.0001193834
			# b = 3.19235

p<-ggplot(ns_males_analysis_sub2, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = cf6[1,1],slope = cf6[2,1],color = "red")
############################ Old shell males ############################################
fit7<-lm(log.weight~log.width,data=os_males_analysis_sub)
summary(fit7)
coef(fit7)

cf7<-as.matrix(coef(fit7))
			# log(W) = -8.35343  + 3.06634  * log(L) on transformed scale
    			# W = exp(-8.35343 *L^(3.06634)  on original scale
			# a =  0.0002355871
			# b = 3.18150
####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################

analysis_males_sub2<-as.data.frame(rbind(ns_males_analysis_sub2,os_males_analysis_sub))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub2, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell) males")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males_sub2, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_points_sub2 <- ggplot(analysis_males_sub2, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log-transformed and truncated at 106 mm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub2, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log-transformed)")


ebscb_sc_lines_sub2 <-ggplot(analysis_males_sub2, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell, log-transformed and truncated at 106 mm)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub2, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub2, x="log.width",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################
ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_sc_points_sub2, ebscb_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

ggarrange(ebscb_sc_points_sub,ebscb_sc_lines_sub,ebscb_sc_points_sub2, ebscb_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*SC,data = analysis_males_sub2)
summary(mod1)									# p=  on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.width+SC,data = analysis_males_sub2)
summary(mod2)									# p =  for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p= ...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_males_sub2)		
summary(reg_mod)


########################################################################################################################
############################## USE ANCOVA TO COMPARE NEW SHELL AND NEW SHELL TRUNCATED SERIES ##########################
########################################################################################################################
ns_males_analysis$Series<- "Base"
ns_males_analysis_sub$Series<- "Sub1"
ns_males_analysis_sub2$Series<- "Sub2"

analysis_males_sub<-rbind(ns_males_analysis,ns_males_analysis_sub)
analysis_males_sub2<-rbind(ns_males_analysis_sub,ns_males_analysis_sub2)
analysis_males_sub3<-rbind(ns_males_analysis,ns_males_analysis_sub2)

analysis_males_sub4<-rbind(os_males_analysis_sub,ns_males_analysis_sub2[,1:22])			# Series for 110 mm old shell and new shell

###########################################################################################################################################
############################### Run ANCOVA analyses to determine if 106 mm cutoff NS and OS are statistically different ############################

mod1<-aov(log.weight~log.width*SC,data = analysis_males_sub4)
summary(mod1)									# p = 8.94e-05 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.width+SC,data = analysis_males_sub4)
summary(mod2)									# p = <2e-16 for series, regression lines have differing intercepts

anova(mod1,mod2)									# p = 8.941e-05...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_males_sub4)		
summary(reg_mod)

##############################################################################################################################################
############################### Run ANCOVA analyses to determine if base and series 1 are statistically different ############################

mod1<-aov(log.weight~log.width*Series,data = analysis_males_sub)
summary(mod1)									# p = 6.44e-10 on interaction term= No significant interaction--suggests same slopes

mod2<-aov(log.weight~log.width+Series,data = analysis_males_sub)
summary(mod2)									# p = 0.00923 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 6.438e-10...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~Series/log.width-1,data=analysis_males_sub)		
summary(reg_mod)


###########################################################################################################################################
############################### Run ANCOVA analyses to determine if series 1 and 2 are statistically different ############################

mod1<-aov(log.weight~log.width*Series,data = analysis_males_sub2)
summary(mod1)									# p = 7.34e-06 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.width+Series,data = analysis_males_sub2)
summary(mod2)									# p = 3.3e-05 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 7.34e-06 ...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~Series/log.width-1,data=analysis_males_sub2)		
summary(reg_mod)


###########################################################################################################################################
############################### Run ANCOVA analyses to determine if baseline and series 2 are statistically different ############################

mod1<-aov(log.weight~log.width*Series,data = analysis_males_sub3)
summary(mod1)									# p = 5.60e-08 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.width+Series,data = analysis_males_sub3)
summary(mod2)									# p = 5.49e-12  for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 5.602e-08...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~Series/log.width-1,data=analysis_males_sub2)		
summary(reg_mod)



