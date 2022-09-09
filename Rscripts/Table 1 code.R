# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA")
##setwd("C:/Users/Jon.Richar/Work/GitRepos/Length_weight/DATA")
df<-read.csv("EBSCB_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
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


########################## Aggregate by New shell/old shell #####################################
ns_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)

Year <- substring(ns_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(ns_males$WIDTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
                    							  		 # inspect data
names(ns_male)
	
os_males<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

Year <- substring(os_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(os_males$WIDTH)
log.weight <- log(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
                    							  		 # inspect data
names(os_male)	
#######################################################################################
distinct(ns_male, YEAR)
nrow(subset(ns_male, YEAR==2000))
nrow(subset(ns_male, YEAR==2001))
nrow(subset(ns_male, YEAR==2006))
nrow(subset(ns_male, YEAR==2007))
nrow(subset(ns_male, YEAR==2008))
nrow(subset(ns_male, YEAR==2009))
nrow(subset(ns_male, YEAR==2010))
nrow(subset(ns_male, YEAR==2011))
nrow(subset(ns_male, YEAR==2012))
nrow(subset(ns_male, YEAR==2013))
nrow(subset(ns_male, YEAR==2014))
nrow(subset(ns_male, YEAR==2016))
nrow(subset(ns_male, YEAR==2017))
nrow(subset(ns_male, YEAR==2018))
nrow(subset(ns_male, YEAR==2019))


distinct(os_male, YEAR)
nrow(subset(os_male, YEAR==2000))
nrow(subset(os_male, YEAR==2001))
nrow(subset(os_male, YEAR==2006))
nrow(subset(os_male, YEAR==2007))
nrow(subset(os_male, YEAR==2008))
nrow(subset(os_male, YEAR==2009))
nrow(subset(os_male, YEAR==2010))
nrow(subset(os_male, YEAR==2011))
nrow(subset(os_male, YEAR==2012))
nrow(subset(os_male, YEAR==2013))
nrow(subset(os_male, YEAR==2014))
nrow(subset(os_male, YEAR==2016))
nrow(subset(os_male, YEAR==2017))
nrow(subset(os_male, YEAR==2018))
nrow(subset(os_male, YEAR==2019))






# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)

setwd("C:/Users/Jon.Richar/Work/Projects/Length_weight/DATA")

######################### BBRKC ####################################
df<-read.csv("BBRKC_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(loglength = log(LENGTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 # filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
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

plot(sc2_males$WEIGHT~sc2_males$LENGTH)
plot(sc3_males$WEIGHT~sc3_males$LENGTH)
plot(sc4_males$WEIGHT~sc4_males$LENGTH)

########################## Aggregate by New shell/old shell #####################################
ns_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)

Year <- substring(ns_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.length<-log(ns_males$LENGTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
       
os_males<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

Year <- substring(os_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.length<-log(os_males$LENGTH)
log.weight <- log(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
      
os_males

distinct(ns_male, YEAR)
unique(ns_male$YEAR)
nrow(subset(ns_male, YEAR==2000))
nrow(subset(ns_male, YEAR==2001))
nrow(subset(ns_male, YEAR==2006))
nrow(subset(ns_male, YEAR==2007))
nrow(subset(ns_male, YEAR==2008))
nrow(subset(ns_male, YEAR==2009))
nrow(subset(ns_male, YEAR==2010))
nrow(subset(ns_male, YEAR==2011))
nrow(subset(ns_male, YEAR==2012))
nrow(subset(ns_male, YEAR==2013))
nrow(subset(ns_male, YEAR==2015))
nrow(subset(ns_male, YEAR==2017))
nrow(subset(ns_male, YEAR==2019))


distinct(os_male, YEAR)
unique(ns_male$YEAR)
nrow(subset(os_male, YEAR==2000))
nrow(subset(os_male, YEAR==2001))
nrow(subset(os_male, YEAR==2006))
nrow(subset(os_male, YEAR==2007))
nrow(subset(os_male, YEAR==2008))
nrow(subset(os_male, YEAR==2009))
nrow(subset(os_male, YEAR==2010))
nrow(subset(os_male, YEAR==2011))
nrow(subset(os_male, YEAR==2012))
nrow(subset(os_male, YEAR==2013))
nrow(subset(os_male, YEAR==2015))
nrow(subset(os_male, YEAR==2017))
nrow(subset(os_male, YEAR==2019))


################## St Matthew Blue King Crab ####################################################

df<-read.csv("SMBKC_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(loglength = log(LENGTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
# filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
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

plot(sc2_males$WEIGHT~sc2_males$LENGTH)
plot(sc3_males$WEIGHT~sc3_males$LENGTH)
plot(sc4_males$WEIGHT~sc4_males$LENGTH)

########################## Aggregate by New shell/old shell #####################################
ns_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)

Year <- substring(ns_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.length<-log(ns_males$LENGTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  

os_males<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

Year <- substring(os_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.length<-log(os_males$LENGTH)
log.weight <- log(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  

os_males

distinct(ns_male, YEAR)
unique(ns_male$YEAR)
nrow(subset(ns_male, YEAR==2000))
nrow(subset(ns_male, YEAR==2001))
nrow(subset(ns_male, YEAR==2006))
nrow(subset(ns_male, YEAR==2007))
nrow(subset(ns_male, YEAR==2008))
nrow(subset(ns_male, YEAR==2009))
nrow(subset(ns_male, YEAR==2010))
nrow(subset(ns_male, YEAR==2011))
nrow(subset(ns_male, YEAR==2012))
nrow(subset(ns_male, YEAR==2013))
nrow(subset(ns_male, YEAR==2015))
nrow(subset(ns_male, YEAR==2017))
nrow(subset(ns_male, YEAR==2019))


distinct(os_male, YEAR)
unique(ns_male$YEAR)
nrow(subset(os_male, YEAR==2000))
nrow(subset(os_male, YEAR==2001))
nrow(subset(os_male, YEAR==2006))
nrow(subset(os_male, YEAR==2007))
nrow(subset(os_male, YEAR==2008))
nrow(subset(os_male, YEAR==2009))
nrow(subset(os_male, YEAR==2010))
nrow(subset(os_male, YEAR==2011))
nrow(subset(os_male, YEAR==2012))
nrow(subset(os_male, YEAR==2013))
nrow(subset(os_male, YEAR==2015))
nrow(subset(os_male, YEAR==2017))
nrow(subset(os_male, YEAR==2019))

######################## Snow crab ############################################

# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)

setwd("C:/Users/Jon.Richar/Work/Projects/Length_weight/DATA")
df<-read.csv("EBSCO_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$LENGTH)
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
########################## Aggregate by New shell/old shell #####################################
ns_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)

Year <- substring(ns_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(ns_males$WIDTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
       
os_males<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

Year <- substring(os_males$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(os_males$WIDTH)
log.weight <- log(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
      
os_males

distinct(ns_male, YEAR)
unique(ns_male$YEAR)
nrow(subset(ns_male, YEAR==1975))
nrow(subset(ns_male, YEAR==2000))
nrow(subset(ns_male, YEAR==2001))
nrow(subset(ns_male, YEAR==2006))
nrow(subset(ns_male, YEAR==2007))
nrow(subset(ns_male, YEAR==2008))
nrow(subset(ns_male, YEAR==2009))
nrow(subset(ns_male, YEAR==2010))
nrow(subset(ns_male, YEAR==2011))
nrow(subset(ns_male, YEAR==2012))
nrow(subset(ns_male, YEAR==2013))
nrow(subset(ns_male, YEAR==2015))
nrow(subset(ns_male, YEAR==2017))
nrow(subset(ns_male, YEAR==2018))
nrow(subset(ns_male, YEAR==2019))


distinct(os_male, YEAR)
unique(os_male$YEAR)
nrow(subset(os_male, YEAR==1975))
nrow(subset(os_male, YEAR==2000))
nrow(subset(os_male, YEAR==2001))
nrow(subset(os_male, YEAR==2006))
nrow(subset(os_male, YEAR==2007))
nrow(subset(os_male, YEAR==2008))
nrow(subset(os_male, YEAR==2009))
nrow(subset(os_male, YEAR==2010))
nrow(subset(os_male, YEAR==2011))
nrow(subset(os_male, YEAR==2012))
nrow(subset(os_male, YEAR==2013))
nrow(subset(os_male, YEAR==2015))
nrow(subset(os_male, YEAR==2017))
nrow(subset(os_male, YEAR==2018))
nrow(subset(os_male, YEAR==2019))






