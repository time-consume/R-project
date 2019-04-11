setwd("/Users/boris/Downloads/SASUniversityEdition/myfolders/AGI")
library(data.table)
library(ggplot2)
library(sqldf)
library(dplyr)
library(fastDummies)
library(ggfortify)
library(autoplotly)
library(knitr)
dat <- read.csv(file = "dat.csv", header = T)
dat <- as.data.table(dat)
dat$start_date <- as.Date(dat$start_date, '%m/%d/%Y')
dat$end_date <- as.Date(dat$end_date, '%m/%d/%Y')
dat$usage_date <- as.Date(dat$usage_date, '%m/%d/%Y')

dat_retained <- dat[which(dat$TRIAL_RETEN_SUCCESS_STAT != "DID NOT RETAIN"),]
dat_retained[which(dat_retained$end_date == "3000-12-31")]$end_date <- as.Date('2018-01-01')
dat2 <- dat_retained[!duplicated(dat_retained$SUBBLOCK_ID)]

Revenue <- dat_retained[, .(sub_length = end_date - start_date), by = SUBBLOCK_ID]
Revenue <- Revenue[,.(sub_length = sum(sub_length)), by = SUBBLOCK_ID]
Revenue$sub_length_factor <- Revenue$sub_length/365
Revenue$revenue_factor <- ceiling(Revenue$sub_length_factor)
Revenue$revenue <- Revenue$revenue_factor*19.99
#Merge
device <- dat2[,.(SUBBLOCK_ID, device, TRIAL_RETEN_SUCCESS_STAT)]
setkey(Revenue, SUBBLOCK_ID)
Result <- Revenue[device]
#write.csv(Result, file = "Result.csv")
#T-test
t.test(Result$revenue ~ Result$device, conf.level = 0.95, alternative = "greater")

#T-test
t.test(Result$sub_length ~ Result$device, conf.level = 0.95, alternative = "greater")

#Use Trial
Result_trial <- Result[which(Result$TRIAL_RETEN_SUCCESS_STAT != "")]
t.test(Result_trial$revenue ~ Result_trial$device, conf.level = 0.95, alternative = "greater")

#Didn't use Trial
Result_notrial <- Result[which(Result$TRIAL_RETEN_SUCCESS_STAT != "RETAINED")]
t.test(Result_notrial$revenue ~ Result_notrial$device, conf.level = 0.95, alternative = "greater")

#summary(as.numeric(Result_notrial$revenue))
#sd(as.numeric(Result_notrial$revenue))
#sd(as.numeric(Result_notrial$sub_length))

####################
dat_retained$start_year<-as.data.table(format(dat_retained$start_date,'%Y'))
dat_retained$end_year<-as.data.table(format(dat_retained$end_date,'%Y'))
dat_retained$sub_length <- dat_retained$end_date-dat_retained$start_date
sub0 <- dat_retained[which(as.numeric(dat_retained$sub_length) <= 365),]
sub1 <- dat_retained[which(as.numeric(dat_retained$sub_length) > 365),]

#sub0
sub0_dummy <- fastDummies::dummy_cols(sub0, select_columns = "start_year")
sub0_dum <- sub0_dummy[,.(SUBBLOCK_ID, start_year_2013, start_year_2014, start_year_2015, start_year_2016, start_year_2017)]
sub0_dum <- sub0_dum[,.(start_year_2013 = sum(start_year_2013), start_year_2014 = sum(start_year_2014),
                        start_year_2015 = sum(start_year_2015), start_year_2016 = sum(start_year_2016),
                        start_year_2017 = sum(start_year_2017)), by = SUBBLOCK_ID]
sub0_dum[which(sub0_dum$start_year_2013 ==2),]$start_year_2013 <-1
sub0_dum[which(sub0_dum$start_year_2014 ==2),]$start_year_2014 <-1
sub0_dum[which(sub0_dum$start_year_2014 ==3),]$start_year_2014 <-1
sub0_dum[which(sub0_dum$start_year_2015 ==2),]$start_year_2015 <-1
sub0_dum[which(sub0_dum$start_year_2015 ==3),]$start_year_2015 <-1
sub0_dum[which(sub0_dum$start_year_2016 ==2),]$start_year_2016 <-1
sub0_dum[which(sub0_dum$start_year_2016 ==3),]$start_year_2016 <-1
sub0_dum[which(sub0_dum$start_year_2017 ==2),]$start_year_2017 <-1
sub0_dum[which(sub0_dum$start_year_2017 ==3),]$start_year_2017 <-1


for(i in 1:dim(sub0_dum)[1]){
  if(sub0_dum$start_year_2013[i] == 1 & sub0_dum$start_year_2014[i] == 1 & sub0_dum$start_year_2015[i] == 1 &
     sub0_dum$start_year_2016[i] == 1 & sub0_dum$start_year_2017[i] == 1){
    sub0_dum$duration[i] <- 5
  }else if((sub0_dum$start_year_2013[i] == 1 & sub0_dum$start_year_2014[i] == 1 & sub0_dum$start_year_2015[i] == 1 &
           sub0_dum$start_year_2016[i] == 1) | (sub0_dum$start_year_2014[i] == 1 & sub0_dum$start_year_2015[i] == 1 & sub0_dum$start_year_2016[i] == 1 &
                                             sub0_dum$start_year_2017[i] == 1)){
    sub0_dum$duration[i] <- 4
  }else if((sub0_dum$start_year_2013[i] == 1 & sub0_dum$start_year_2014[i] == 1 & sub0_dum$start_year_2015[i] == 1) |
           (sub0_dum$start_year_2014[i] == 1 & sub0_dum$start_year_2015[i] == 1 & sub0_dum$start_year_2016[i] == 1)|
           (sub0_dum$start_year_2015[i] == 1 & sub0_dum$start_year_2016[i] == 1 & sub0_dum$start_year_2017[i] == 1)){
    sub0_dum$duration[i] <- 3
  }else if((sub0_dum$start_year_2013[i] == 1 & sub0_dum$start_year_2014[i] == 1)|
           (sub0_dum$start_year_2014[i] == 1 & sub0_dum$start_year_2015[i] == 1)|
           (sub0_dum$start_year_2015[i] == 1 & sub0_dum$start_year_2016[i] == 1)|
           (sub0_dum$start_year_2016[i] == 1 & sub0_dum$start_year_2017[i] == 1)){
    sub0_dum$duration[i] <- 2
  }else{
    sub0_dum$duration[i] <- 1
  }
}

#write.csv(sub0_dum, file = "sub0_dum.csv")

sub0_dum <- read.csv("sub0_dum.csv", header = T)
sub0_dum <- as.data.table(sub0_dum)

sub0_dum_device <- sub0_dummy[,.(SUBBLOCK_ID, device, TRIAL_RETEN_SUCCESS_STAT)]
setkey(sub0_dum, SUBBLOCK_ID)
sub0_result <- sub0_dum[device]
sub0_desktop <- sub0_result[which(sub0_result$device == "DESKTOP")]
sub0_mobile <- sub0_result[which(sub0_result$device == "MOBILE")]

#sub1#
sub1$duration <- sub1[,.(duration = as.numeric(ceiling(sub1$sub_length/365)))]
sub1_desktop <- sub1[which(sub1$device == "DESKTOP")]
sub1_mobile <- sub1[which(sub1$device == "MOBILE")]

#Retention desktop#
retention_desktop <- matrix(NA, nrow = 4, ncol = 2)
retention_desktop <- as.data.table(retention_desktop)
colnames(retention_desktop) <- c("n", "Desktop Retention Rate")

for(i in 1:4){
  retention_desktop$n[i] <- i+1
  retention_desktop$`desktop retention rate`[i] <- (as.numeric(count(sub1_desktop[which(sub1_desktop$duration >= i+1)]))+as.numeric(count(sub0_desktop[which(sub0_desktop$duration >= i+1)])))/
    (as.numeric(count(sub1_desktop[which(sub1_desktop$duration >= i)]))+as.numeric(count(sub0_desktop[which(sub0_desktop$duration >= i)])))
}

#Retention mobile#
retention_mobile <- matrix(NA, nrow = 4, ncol = 2)
retention_mobile <- as.data.table(retention_mobile)
colnames(retention_mobile) <- c("n", "Mobile Retention Rate")

for(i in 1:4){
  retention_mobile$n[i] <- i+1
  retention_mobile$`mobile retention rate`[i] <- (as.numeric(count(sub1_mobile[which(sub1_mobile$duration >= i+1)]))+as.numeric(count(sub0_mobile[which(sub0_mobile$duration >= i+1)])))/
    (as.numeric(count(sub1_mobile[which(sub1_mobile$duration >= i)]))+as.numeric(count(sub0_mobile[which(sub0_mobile$duration >= i)])))
}

setkey(retention_desktop, n)
retention <- retention_desktop[retention_mobile]

#Survival analysis
index <- dat_retained[which(dat_retained$end_date != "2018-01-01")]
index <- index[!duplicated(index$SUBBLOCK_ID)]
survival <- Revenue[, .(SUBBLOCK_ID, sub_length)]
survival$device <- Result$device
survival$TRIAL_STAT <- Result$TRIAL_RETEN_SUCCESS_STAT
survival <- survival[which(survival$TRIAL_STAT != "ACTIVE")]
survival[which(survival$TRIAL_STAT == "RETAINED")]$TRIAL_STAT <- "USE TRIAL"
survival[which(survival$TRIAL_STAT == ""),]$TRIAL_STAT <- "NOT USE TRIAL"
survival$dead <- 1
survival <- merge(x=survival, y=index, by = "SUBBLOCK_ID", all.x = TRUE)
survival$sub_length.x <- as.numeric(survival$sub_length.x)
#sqldf("select survival.SUBBLOCK_ID, sub_length, device, TRIAL_STAT, dead from survival where survival.SUBBLOCK_ID <> index.SUBBLOCK_ID")
surv_object <- Surv(time = survival$sub_length.x, event = survival$dead)
write.csv(survival, file = "survival.csv")

#Survival Device
fit_device <- survfit(surv_object ~ device.x, data = survival)
ggsurvplot(fit_device, data = survival, pval = TRUE,conf.int = TRUE)
autoplot(fit_device) + xlab("Subscription length (day)") + ylab("Survival Probability")

#Survival TRIAL_RETEN_SUCCESS_STAT

fit_reten <- survfit(surv_object ~ TRIAL_STAT, data = survival)
#ggsurvplot(fit_reten, data = survival, pval = TRUE)
autoplot(fit_reten) + xlab("Subscription length (day)") + ylab("Survival Probability")

 #fit_reten_device <- survfit(surv_object ~ strata(TRIAL_STAT)+strata(device.x), data = survival)
#ggsurvplot(fit_reten_device, conf.int = TRUE) 






