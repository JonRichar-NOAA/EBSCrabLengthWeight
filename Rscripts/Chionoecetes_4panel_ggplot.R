# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/Projects/Length_weight/DATA")
df.cb<-read.csv("EBS_CB_Analysis_males.csv")
df.co<-read.csv("EBS_CO_Analysis_males.csv")

######################Log transformed #################################################
dev.new()

cb1<-ggplot(df.cb, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male bairdi (New shell and old shell, log-transformed)")

# lines ONLY
cb2<-ggplot(df.cb, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male bairdi (New shell and old shell, log-transformed)")

##################### Opilio ###################################################
# points only
co1<-ggplot(df.co, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

# lines ONLY
co2<-ggplot(df.co, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(cb1,cb2,co1, co2 +rremove("x.text"),
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

