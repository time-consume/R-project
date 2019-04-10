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
#survival rate#
table<-as.data.frame(table(merge_new$SUBBLOCK_ID))
table
dim(table)
table_2<-as.data.frame(table(table[,2]))
table_2
#Seperate by the date join in#
merge_2013<-subset(merge_new,grepl('2013',merge_new$SUBBLOCK_START_YRMON.x[]))
merge_2014<-subset(merge_new,grepl('2014',merge_new$SUBBLOCK_START_YRMON.x[]))
merge_2015<-subset(merge_new,grepl('2015',merge_new$SUBBLOCK_START_YRMON.x[]))
merge_2016<-subset(merge_new,grepl('2016',merge_new$SUBBLOCK_START_YRMON.x[]))
merge_2017<-subset(merge_new,grepl('2017',merge_new$SUBBLOCK_START_YRMON.x[]))
#count record every year 2013#
table_2013<-as.data.frame(table(merge_2013$SUBBLOCK_ID))
table_2013<-as.data.frame(table(table_2013[,2]))
table_2013
sum(table_2013[,2])
#count record every year 2014#
table_2014<-as.data.frame(table(merge_2014$SUBBLOCK_ID))
table_2014<-as.data.frame(table(table_2014[,2]))
print(2014)
table_2014
sum(table_2014[,2])
#count record every year 2015#
table_2015<-as.data.frame(table(merge_2015$SUBBLOCK_ID))
table_2015<-as.data.frame(table(table_2015[,2]))
print(2015)
table_2015
sum(table_2015[,2])
#count record every year 2016#
table_2016<-as.data.frame(table(merge_2016$SUBBLOCK_ID))
table_2016<-as.data.frame(table(table_2016[,2]))
print(2016)
table_2016
sum(table_2016[,2])
#count record every year 2017#
table_2017<-as.data.frame(table(merge_2017$SUBBLOCK_ID))
table_2017<-as.data.frame(table(table_2017[,2]))
print(2017)
table_2017
sum(table_2017[,2])

#Try vs no try#
merge_try<-merge_new[merge_new$TRIAL_RETEN_SUCCESS_STAT.x=='RETAINED',]
merge_notry<-merge_new[merge_new$TRIAL_RETEN_SUCCESS_STAT.x!='RETAINED',]
clv_try<-nrow(merge_try)*19.99/length(unique(merge_try$SUBBLOCK_ID))
clv_notry<-nrow(merge_notry)*19.99/length(unique(merge_notry$SUBBLOCK_ID))
clv_try
clv_notry

#Seperate merge_new into mobile and desktop#
merge_new_desktop<-merge_new[merge_new$device=='DESKTOP',]
merge_new_mobile<-merge_new[merge_new$device=='MOBILE',]

#This is desktop#
#Seperate by the date join in#
merge_desktop_2013<-subset(merge_new_desktop,grepl('2013',merge_new_desktop$SUBBLOCK_START_YRMON.x[]))
merge_desktop_2014<-subset(merge_new_desktop,grepl('2014',merge_new_desktop$SUBBLOCK_START_YRMON.x[]))
merge_desktop_2015<-subset(merge_new_desktop,grepl('2015',merge_new_desktop$SUBBLOCK_START_YRMON.x[]))
merge_desktop_2016<-subset(merge_new_desktop,grepl('2016',merge_new_desktop$SUBBLOCK_START_YRMON.x[]))
merge_desktop_2017<-subset(merge_new_desktop,grepl('2017',merge_new_desktop$SUBBLOCK_START_YRMON.x[]))
#count record every year 2013#
table_desktop_2013<-as.data.frame(table(merge_desktop_2013$SUBBLOCK_ID))
table_desktop_2013<-as.data.frame(table(table_desktop_2013[,2]))
print(2013)
table_desktop_2013
sum(table_desktop_2013[,2])
#count record every year 2014#
table_desktop_2014<-as.data.frame(table(merge_desktop_2014$SUBBLOCK_ID))
table_desktop_2014<-as.data.frame(table(table_desktop_2014[,2]))
print(2014)
table_desktop_2014
sum(table_desktop_2014[,2])
#count record every year 2015#
table_desktop_2015<-as.data.frame(table(merge_desktop_2015$SUBBLOCK_ID))
table_desktop_2015<-as.data.frame(table(table_desktop_2015[,2]))
print(2015)
table_desktop_2015
sum(table_desktop_2015[,2])
#count record every year 2016#
table_desktop_2016<-as.data.frame(table(merge_desktop_2016$SUBBLOCK_ID))
table_desktop_2016<-as.data.frame(table(table_desktop_2016[,2]))
print(2016)
table_desktop_2016
sum(table_desktop_2016[,2])
#count record every year 2017#
table_desktop_2017<-as.data.frame(table(merge_desktop_2017$SUBBLOCK_ID))
table_desktop_2017<-as.data.frame(table(table_desktop_2017[,2]))
print(2017)
table_desktop_2017
sum(table_desktop_2017[,2])


#This is mobile#
#Seperate by the date join in#
merge_mobile_2013<-subset(merge_new_mobile,grepl('2013',merge_new_mobile$SUBBLOCK_START_YRMON.x[]))
merge_mobile_2014<-subset(merge_new_mobile,grepl('2014',merge_new_mobile$SUBBLOCK_START_YRMON.x[]))
merge_mobile_2015<-subset(merge_new_mobile,grepl('2015',merge_new_mobile$SUBBLOCK_START_YRMON.x[]))
merge_mobile_2016<-subset(merge_new_mobile,grepl('2016',merge_new_mobile$SUBBLOCK_START_YRMON.x[]))
merge_mobile_2017<-subset(merge_new_mobile,grepl('2017',merge_new_mobile$SUBBLOCK_START_YRMON.x[]))
#count record every year 2013#
table_mobile_2013<-as.data.frame(table(merge_mobile_2013$SUBBLOCK_ID))
table_mobile_2013<-as.data.frame(table(table_mobile_2013[,2]))
print(2013)
table_mobile_2013
sum(table_mobile_2013[,2])
#count record every year 2014#
table_mobile_2014<-as.data.frame(table(merge_mobile_2014$SUBBLOCK_ID))
table_mobile_2014<-as.data.frame(table(table_mobile_2014[,2]))
print(2014)
table_mobile_2014
sum(table_mobile_2014[,2])
#count record every year 2015#
table_mobile_2015<-as.data.frame(table(merge_mobile_2015$SUBBLOCK_ID))
table_mobile_2015<-as.data.frame(table(table_mobile_2015[,2]))
print(2015)
table_mobile_2015
sum(table_mobile_2015[,2])
#count record every year 2016#
table_mobile_2016<-as.data.frame(table(merge_mobile_2016$SUBBLOCK_ID))
table_mobile_2016<-as.data.frame(table(table_mobile_2016[,2]))
print(2016)
table_mobile_2016
sum(table_mobile_2016[,2])
#count record every year 2017#
table_mobile_2017<-as.data.frame(table(merge_mobile_2017$SUBBLOCK_ID))
table_mobile_2017<-as.data.frame(table(table_mobile_2016[,2]))
print(2017)
table_mobile_2017
sum(table_mobile_2017[,2])
