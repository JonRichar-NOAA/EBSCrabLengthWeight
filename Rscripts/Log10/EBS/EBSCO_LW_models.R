# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBSCO_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)
######################### Tidyverse approach ###################################
df1 %>%
  mutate(logwidth = log(WIDTH),
          logweight = log(WEIGHT),
          Year = substring(CRUISE, 1,4), 
          YEAR = as.factor(Year)) %>%
 filter(SEX == 1) -> male #Only SC2 as to not bias for weight of epibionts 
#male
ggplot(male, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
     geom_point(aes(colour = factor(SHELL_CONDITION)))


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
log10.width<-log10(ns_males$WIDTH)
log10.weight <- log10(ns_males$WEIGHT)
log.width<-log(ns_males$WIDTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_male                    							  		 # inspect data
names(ns_male)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_male, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=ns_male)
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
 
nrow(ns_male)-nrow(ns_males_analysis)    #235 obs removed based on Cook's Distance

nrow(ns_males_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.width,data=ns_males_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
10^(cf2[1,1])
# log(W) = -3.625331 + 3.11951 * log(L) on transformed scale
# W = 10^(-3.625331) * L^(3.11951)  on original scale
# a = 0.0002369565 
# b = 3.11951
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
A                         #0.0002373173

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v2/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
# a = 0.0002373173
# b = 3.119509
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
log10.width<-log10(os_males$WIDTH)
log10.weight <- log10(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_male                    							  		 # inspect data
names(os_male)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_male, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log10.weight~log10.width,data=os_male)
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
 
nrow(os_male)-nrow(os_males_analysis)    #123 obs removed

nrow(os_males_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_males_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_males_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) opilio males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log10.weight~log10.width,data=os_males_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=os_males_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

cf4
10^cf4[1,1]

# log(W) = -3.464922 + 3.051748 * log(L) on transformed scale
# W = 10^(-3.464922) * L^(3.051748)  on original scale
# a = 0.0003428294 
# b = 3.051748

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(10^(int)*10^(v4/2))
A                         #0.0003431106 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v4/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.0003431106 
# b = 3.051748

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_males_analysis$SC <- "OS"
ns_males_analysis$SC <- "NS"

analysis_males<-rbind(ns_males_analysis,os_males_analysis)

write.csv(analysis_males,"EBS_CO_Analysis_males_log10.csv")

ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="Male opilio (New shell and old shell)")

######################Log transformed #################################################
dev.new()


ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

ebsco_sc_points<-ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

# lines ONLY
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

ebsco_sc_lines<-ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")

# with points and lines
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="EBS male opilio (New shell and old shell log-transformed)")


################ Alternative approach with lines
ggscatter(analysis_males, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_males)			# p = 1.94e-15--significant interaction -- slopes different
summary(mod1)

mod2<-aov(log10.weight~log10.width+SC,data = analysis_males)			# p < 2e-16: interceptes are significantly different
summary(mod2)

anova(mod1,mod2)										# p = 1.94e-15--interaction term is significant to model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_males)
summary(reg_mod)



#########################################################################################################################################################
############################### Size weight models by temperature regime ################################################################################
#########################################################################################################################################################

analysis_males<-rbind(ns_males_analysis,os_males_analysis)

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

mean(warm_ns_males$WEIGHT)
sd(warm_ns_males$WEIGHT)

mean(cold_ns_males$WEIGHT)
sd(cold_ns_males$WEIGHT)


warm_ns_males$TEMP <- "WARM"
cold_ns_males$TEMP <- "COLD"
cold_ns_males$COLOR <- 1
warm_ns_males$COLOR <- 4

temp_ns_males<-rbind(warm_ns_males,cold_ns_males)

nrow(cold_ns_males)
nrow(warm_ns_males)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log10.weight~log10.width,data=cold_ns_males)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log10.weight~log10.width,data=warm_ns_males)
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
A                         # 0.000230609  

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
A<-(10^(int)*10^(v.warm/2))
A                         # 0.0002516442 
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.warm/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL MODEL ###############################
# a = 0.000230609 
# b = 3.126483 
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0002516442  
# b = 3.105468

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*TEMP,data = temp_ns_males)							# p = 0.0104 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log10.weight~log10.width+TEMP,data = temp_ns_males)							# p = 0.446 for TEMP: no evidence temperature-based regression lines have different intercepts
summary(mod2)									

anova(mod1,mod2)														# p = 0.01043...removing interaction term significantly affects model								

reg_mod<-lm(log10.weight~TEMP/log10.width-1,data = temp_ns_males)		
summary(reg_mod)

mod10<-anova(lm(log10.weight~log10.width,data = temp_ns_males),lm(log10.weight~log10.width*TEMP,data = temp_ns_males))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_males, x="log10.width",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

ebsco_temp_points<-ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_males, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Male EBS CO (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

ebsco_temp_lines <- ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell, log-transformed)")

temp_ns_males
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebsco_sc_points,ebsco_sc_lines,ebsco_temp_points, ebsco_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)


########################################################################################################################
############################ Assess SC2 males ONLY by cold/warm year periods method 2- Five most extreme years for warm and cold ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_males<-subset(analysis_males, YEAR==2001|YEAR==2014|YEAR==2016|YEAR==2018|YEAR==2019)
cold_males<-subset(analysis_males, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)

warm_ns_males<-subset(warm_males,SC=="NS")

cold_ns_males<-subset(cold_males,SC=="NS")

mean(warm_ns_males$WEIGHT)
sd(warm_ns_males$WEIGHT)

mean(cold_ns_males$WEIGHT)
sd(cold_ns_males$WEIGHT)

warm_ns_males$TEMP <- "WARM"
cold_ns_males$TEMP <- "COLD"
cold_ns_males$COLOR <- 1
warm_ns_males$COLOR <- 4

temp_ns_males<-rbind(warm_ns_males,cold_ns_males)

nrow(cold_ns_males)
nrow(warm_ns_males)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log10.weight~log10.width,data=cold_ns_males)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log10.weight~log10.width,data=warm_ns_males)
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
A<-(10^(int)*10^(v.cold/2))
A                         #0.0002321831   

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
A<-(10^(int)*10^(v.warm/2))
A                         #0.0002821382  
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(10^(sd)*10^(v.warm/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL MODEL ###############################
# a = 0.0002321831 
# b = 3.123121
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0002821382 
# b = 3.08395

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*TEMP,data = temp_ns_males)							# p = 0.017  on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log10.weight~log10.width+TEMP,data = temp_ns_males)							# p = 6.02e-10  for TEMP: temperature-based regression lines have different intercepts
summary(mod2)									

anova(mod1,mod2)														# p = 0.01702...removing interaction term significantly affects model								

reg_mod<-lm(log10.weight~TEMP/log10.width-1,data = temp_ns_males)		
summary(reg_mod)

mod10<-anova(lm(log10.weight~log10.width,data = temp_ns_males),lm(log10.weight~log10.width*TEMP,data = temp_ns_males))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_males, x="log10.width",y="log10.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

ebsco_temp_points<-ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_males, aes(x = WIDTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Male EBS CO (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

ebsco_temp_lines <- ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell, log-transformed)")

temp_ns_males
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_males, aes(x = log10.width, y = log10.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male EBS CO (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebsco_sc_points,ebsco_sc_lines,ebsco_temp_points, ebsco_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)

##########################################################################################################################################################
############################### Rerun shell condition models while limit data to same size ranges ##########################################################
###########################################################################################################################################################


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

############################ Truncate new shells to match old shell size range ###################

ns_males_analysis_sub<-subset(ns_males_analysis,WIDTH>=36.0)


############################ Fit followup model ##########################################
fit5<-lm(log10.weight~log10.width,data=ns_males_analysis_sub)
summary(fit5)
coef(fit5)

cf5<-as.matrix(coef(fit5))
cf5

10^cf5[1,1]
			# log(W) = -3.654106 + 3.134742 * log(L) on transformed scale
    			# W = 10^(-3.654106)*L^(3.134742)  on original scale
			# a = 0.0002217655
			# b = 3.134742

p<-ggplot(ns_males_analysis_sub, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = cf5[1,1],slope = cf5[2,1],color = "red")

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v5<-(summary(fit5)$sigma)**2  #Variance 
v5
int<-cf5[1,1]
A<-(10^(int)*10^(v5/2))
A                         #0.0002221006 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit5)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v5/2))
sdA

sdA_base<-10^(sd)
sdA_base
####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################
analysis_males_sub<-as.data.frame(rbind(ns_males_analysis_sub,os_males_analysis))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell)")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males_sub, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

ebsco_sc_points_sub <- ggplot(analysis_males_sub, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log transformed and truncated)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")


ebsco_sc_lines_sub <-ggplot(analysis_males_sub, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell,log transformed and truncated)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebsco_sc_points, ebsco_sc_lines, ebsco_sc_points_sub, ebsco_sc_lines_sub,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_males_sub)
summary(mod1)									# p < 2e-16 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log10.weight~log10.width+SC,data = analysis_males_sub)
summary(mod2)									# p < 2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p < 2.2e-16...removing interaction term sinificantly affects model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_males_sub)		
summary(reg_mod)


########################################################################################################################
############################### Truncate further at width = 76 mm(based on old shell first quartile) ######################################################
ns_males_analysis_sub2<-subset(ns_males_analysis,WIDTH>=76)
os_males_analysis_sub<-subset(os_males_analysis,WIDTH>=76)

############################ Fit followup model - new shell ##########################################
fit6<-lm(log10.weight~log10.width,data=ns_males_analysis_sub2)
summary(fit6)
coef(fit6)

cf6<-as.matrix(coef(fit6))
10^cf6[1,1]
			# log(W) = -3.729034  + 3.173150 * log(L) on transformed scale
    			# W= 10^(-3.729034*L^(3.173150)  on original scale
			# a =  0.0001866235
			# b = 3.173150 
################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v6<-(summary(fit6)$sigma)**2  #Variance 
v6
int<-cf6[1,1]
A<-(10^(int)*10^(v6/2))
A                         #0.0001869239 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit6)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v6/2))
sdA

sdA_base<-10^(sd)
sdA_base
############################ Fit followup model - old shell ##########################################
fit7<-lm(log10.weight~log10.width,data=os_males_analysis_sub)
summary(fit7)
coef(fit7)

cf7<-as.matrix(coef(fit7))
10^cf7[1,1]
			# log(W) = -7.99453 + 3.055352  * log(L) on transformed scale
    			# W = 10^(-7.99453*L^(3.055352)  on original scale
			# a =  0.0003373026
			# b = 3.055352 

p<-ggplot(ns_males_analysis_sub2, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = cf6[1,1],slope = cf6[2,1],color = "red")

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v7<-(summary(fit7)$sigma)**2  #Variance 
v7
int<-cf7[1,1]
A<-(10^(int)*10^(v7/2))
A                         #0.0002221006 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit7)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v7/2))
sdA

sdA_base<-10^(sd)
sdA_base
####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################

analysis_males_sub2<-as.data.frame(rbind(ns_males_analysis_sub2,os_males_analysis_sub))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub2, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell) males")

########################### Log transformed ##########################################################################
dev.new()

############################# po only ############################################################################
ggplot(analysis_males_sub2, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

ebsco_sc_points_sub2 <- ggplot(analysis_males_sub2, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, truncated at 76 mm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub2, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")


ebsco_sc_lines_sub2 <-ggplot(analysis_males_sub2, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, truncated at 76 mm)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub2, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub2, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################
ggarrange(ebsco_sc_points,ebsco_sc_lines,ebsco_sc_points_sub2, ebsco_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

dev.new()

ggarrange(ebsco_sc_points_sub,ebsco_sc_lines_sub,ebsco_sc_points_sub2, ebsco_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_males_sub2)
summary(mod1)									# p = 8.64e-10 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log10.weight~log10.width+SC,data = analysis_males_sub2)
summary(mod2)									# p < 2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p = 8.638e-10...removing interaction term sinificantly affects model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_males_sub2)		
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

########################################################################################################################
############################### Run ANCOVA analyses to determine if base and series 1 are statistically different ############################

mod1<-aov(log10.weight~log10.width*Series,data = analysis_males_sub)
summary(mod1)									# p = 0.00701 on interaction term=significant interaction--suggests different slopes

mod2<-aov(log10.weight~log10.width+Series,data = analysis_males_sub)
summary(mod2)									# p = 0.298 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.007013...removing interaction term does not sinificantly affect model

reg_mod<-lm(log10.weight~Series/log10.width-1,data=analysis_males_sub)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if series 1 and 2 are statistically different ############################

mod1<-aov(log10.weight~log10.width*Series,data = analysis_males_sub2)
summary(mod1)									# p = 0.0111 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log10.weight~log10.width+Series,data = analysis_males_sub2)
summary(mod2)									# p = 0.773 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.7623...removing interaction term does not sinificantly affect model

reg_mod<-lm(log10.weight~Series/log10.width-1,data=analysis_males_sub2)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if series 1 and 2 are statistically different ############################

mod1<-aov(log10.weight~log10.width*Series,data = analysis_males_sub3)
summary(mod1)									# p = 0.00352l on interaction term= significant interaction--suggests different slopes

mod2<-aov(log10.weight~log10.width+Series,data = analysis_males_sub3)
summary(mod2)									# p = 0.198  for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.003517...removing interaction term sinificantly affects model

reg_mod<-lm(log10.weight~Series/log10.width-1,data=analysis_males_sub2)		
summary(reg_mod)



