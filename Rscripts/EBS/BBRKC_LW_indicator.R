# notes ----
# Calculate length:weight regressions and pre-recruit residuals for BBRKC 
# Erin Fedewa
# last updated: 2020/4/14

# load ----
library(tidyverse)

# data ----

df1 <- read_csv("./Data/ebscrab_rkc_bb_specimen_table_with_weights.csv")
head(df1)
nrow(df1)

# data mgmt ----

range(df1$CRUISE)

#look at the distribution
hist(df1$WEIGHT)
hist(log(df1$WEIGHT))# log transformed - better

#BBRKC Male L:W regression ----

#Plots
df1 %>%
  mutate(loglength = log(LENGTH),
          logweight = log(WEIGHT),
          Year = substring(CRUISE, 1,4), 
          YEAR = as.factor(Year)) %>%
  filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 

ggplot(male, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
      geom_point(aes(colour = factor(YEAR)))

ggplot(male, aes(x = loglength, y = logweight, group = YEAR)) +
  geom_point(aes(colour = factor(YEAR)))

  #Just pre-recruits
male %>%
  filter(LENGTH %in% (110:134)) %>% 
  ggplot(aes(x = loglength, y = logweight, group = YEAR)) +
  geom_point(aes(colour = factor(YEAR))) +
  geom_line(aes(y = predict(lm(logweight~loglength)))) +
  facet_wrap(~YEAR) #Very low sample size for some years!
  
#Linear model (natural log of power function W = a * L^b) ----
  #See http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf

fit1 <- lm(logweight~loglength, data=male) 
  plot(fit1)
  summary(fit1)
  coef(fit1)
  
  #Addressing outliers 
plot(cooks.distance(fit1), pch="*", cex=2, main="Influential Obs by Cook's distance") 
     abline(h = 4/(nrow(male)), col="red")  # add cutoff line (critical Cooks D > 4/n)
     
  male$cd <- cooks.distance(fit1)
  
  keepers<-subset(male, cd < (4/1857)) 
  nrow(male) - nrow(keepers) #52 observations removed 
  
ggplot(keepers, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
    geom_point(aes(colour = factor(YEAR)))

#Re-fit model with outliers removed 
fit2 <- lm(logweight~loglength, data=keepers) 
  plot(fit2)
  summary(fit2)
  coef(fit2)
  
  #SC2 Male RKC best-fit equations:
    # log(W) = -7.815319  + 3.139957 * log(L) on transformed scale
    # W = exp(-7.815319)*L^(3.139957)  on original scale 
        # a = 0.0004035061, b = 3.139957 
  
#L:W residual calculations for males ----
  
#All SC2 males
keepers %>% 
    mutate(resid = residuals(lm(logweight~loglength))) %>%
    group_by(YEAR) %>%
    summarise(Avg_resid = mean(resid)) %>%
    ggplot(aes(YEAR, Avg_resid)) +
    geom_bar(stat = "identity")
  
  
#Only pre-recruits
keepers %>% 
    mutate(resid = residuals(lm(logweight~loglength))) %>%
    filter(LENGTH %in% (110:134)) %>%
    group_by(YEAR) %>%
    summarise(Avg_resid = mean(resid)) %>%
    ggplot(aes(YEAR, Avg_resid)) +
      geom_bar(stat = "identity")
  
#Second method - 
  keepers %>%
    filter(LENGTH %in% (110:134)) %>%
    group_by(YEAR) %>%
    summarise(loglength = log(median(LENGTH)),
              logweight = log(median(WEIGHT))) %>%
    mutate(fitted_weight = predict(fit2, newdata = .),
           diff = logweight - fitted_weight) %>%
    ggplot(aes(YEAR, diff)) +
      geom_bar(stat = "identity")
  
  
#Condition factor K for male SC2 RKC ----
  df1 %>%
    filter(SEX == 1, SHELL_CONDITION == 2) %>%
    mutate(K=WEIGHT/(LENGTH^3)*100000, 
           Year = substring(CRUISE, 1,4), 
           YEAR = as.factor(Year)) %>%
    group_by(YEAR) %>%
    summarise(Avg_cond = mean(K)) %>%
    ggplot(aes(YEAR, Avg_cond)) +
      geom_point(size=4) #Seem to follow same trends as residuals 
  
#Thoughts: Diff in L:W likely driven by molt timing/moisture content in muscle- are residuals 
  #consistent with thermal regime in that cold years molt is delayed and crab weight less? 
  
  #Also pre-recruit size class presumably includes both imm/mature crabs- are trends in L:W due to differences in gonad development? 
