# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df.cb<-read.csv("EBS_CB_Analysis_males_log10.csv")
df.co<-read.csv("EBS_CO_Analysis_males_log10.csv")

names(df.cb)
######################Log transformed #################################################
dev.new()

cb1<-ggplot(df.cb, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Log10(Width)",y="Log10(Weight)", title="Male bairdi (New shell and old shell, log-transformed)")

# lines ONLY
cb2<-ggplot(df.cb, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Log10(Width)",y="Log10(Weight)", title="Male bairdi (New shell and old shell, log-transformed)")

##################### Opilio ###################################################
# points only
co1<-ggplot(df.co, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Log10(Width)",y="Log10(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

# lines ONLY
co2<-ggplot(df.co, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Log10(Width)",y="Log10(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

########################################################################################################################
############################### 4 panel ################################################################################
dev.new()
ggarrange(cb1,cb2,co1, co2 +rremove("x.text"),
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

