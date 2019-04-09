#read files#
setwd("/Users/yichenghu/Documents/AGI")
send<-read.csv("first_send.csv",header = T)
sub<-read.csv("subscriptions.csv",header = T)
#summarize data#
summary(send)
summary(sub)
#merge two file by subscription id#
merge<-merge(send,sub,by='SUBSCRIPTION_ID')
#check no duplicate customer#
nrow(merge)==length(unique(merge$SUBBLOCK_ID))
#eliminate quited customers#
sub<-sub[sub$TRIAL_RETEN_SUCCESS_STAT!='DID NOT RETAIN',]
merge<-merge[merge$TRIAL_RETEN_SUCCESS_STAT!='DID NOT RETAIN',]
#check information#
summary(sub);summary(merge)
#merge by subblock id#
merge_new<-merge(merge,sub,by='SUBBLOCK_ID')
#seperate desktop and mobile#
merge_desktop<-merge_new[merge_new$device=='DESKTOP',]
merge_mobile<-merge_new[merge_new$device=='MOBILE',]  
#calculate customer numbers in each category#
length(unique(merge_desktop$SUBBLOCK_ID))
length(unique(merge_mobile$SUBBLOCK_ID))
#calculate subscription numbers#
nrow(merge_desktop)
nrow(merge_mobile)
#customer value=subscription fee*subscription period/customer numbers#
clv_desktop<-nrow(merge_desktop)*19.99/length(unique(merge_desktop$SUBBLOCK_ID))
clv_mobile<-nrow(merge_mobile)*19.99/length(unique(merge_mobile$SUBBLOCK_ID))
clv_desktop
clv_mobile



