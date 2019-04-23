library(survival)
library(survminer)
library(dplyr)
setwd("/Users/yichenghu/Desktop/ADA_ID2")
dat<-read.csv("Myeloma.csv")
glimpse(dat)
hist(dat$calc)
median(dat$calc)
dat$cal[dat$calc>=median(dat$calc)]=1
dat$cal[dat$calc<=median(dat$calc)]=0
#ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = dat$dur, event = dat$dead)
surv_object
surv_fit<- survfit(surv_object ~ cal, data = dat)
summary(surv_fit)
ggsurvplot(surv_fit, data = dat, pval = TRUE)
