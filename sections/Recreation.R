#Recreation calculations
#Alene Onion
#March 2019

#pulling class B waters to identify possible impairments to the drinking water designated use
REC<-intensive[grepl("B",intensive$simpleWC)|grepl("A",intensive$simpleWC),]

#calculate max hypo DO value
#insert thermoclines
thermoclines<-read.csv("sections/data/thermoclines.csv", stringsAsFactors=FALSE)
REC<-merge(REC,thermoclines,by=c('LAKE_ID'),all.x = TRUE)
#find max value
hypoDO<-REC[REC$Characteristic.Name=="DISSOLVED OXYGEN (DO)",]
hypoDO<-hypoDO[hypoDO$Depth>hypoDO$thermocline,]
library(dplyr)
hypoDO<- hypoDO %>%
  group_by(SAMPLE_ID) %>%
  summarize(Result.Value = max(Result.Value)) %>%
  ungroup()
hypoDO$Characteristic.Name <-"HYPO_DO"
samples<-unique(REC[c('SAMPLE_ID','LAKE_ID','SAMPLE_NAME','LOCATION_ID','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','Depth','WATER','Waterbody_Classification','INFO_TYPE','DATA_PROVIDER','basin','simpleWC','simpleT')])
hypoDO<-merge(hypoDO,samples,by=c('SAMPLE_ID'),all.x=TRUE)
#remove duplicate values (when the same ph or DO is read at multiple depths)
library(dplyr)
hypoDO<-hypoDO %>%
  group_by(SAMPLE_ID,LAKE_ID,SAMPLE_NAME,LOCATION_ID,SAMPLE_DATE,TIME,Characteristic.Name,Result.Value,START_DEPTH,END_DEPTH,WATER,Waterbody_Classification,INFO_TYPE,DATA_PROVIDER,basin,simpleWC,simpleT) %>%
  summarize(Depth = max(Depth)) %>%
  ungroup()
#merge with REC
REC$thermocline<-NULL
REC<-merge(REC,hypoDO,all=TRUE)
rm(list=c('hypoDO','samples','thermoclines'))

#add thresholds
short<-unique(thresholds[c('Designated_Use','Characteristic.Name','threshold','Characteristic.Name2','threshold2')])
REC<-merge(REC,short,by=c('Characteristic.Name'),all.x=TRUE)
rm(short)
REC<-REC[REC$Designated_Use=="recreation"|REC$Characteristic.Name=="TRUE COLOR",]
REC<-REC[!is.na(REC$Result.Value),]

#Identify possibly impaired
REC$PIrecreation<-NA
REC$PIrecreation<-ifelse(REC$Characteristic.Name=="PHOSPHORUS" & REC$Result.Sample.Fraction=="T",
                            ifelse(REC$INFO_TYPE=="OW" & REC$Result.Value>REC$threshold,1,REC$PIrecreation),
                            REC$PIrecreation)
REC$PIrecreation<-ifelse(REC$Characteristic.Name=="HYPO_DO",
                         ifelse(REC$Result.Value<REC$threshold,1,REC$PIrecreation),
                         REC$PIrecreation)

#identify secchi disk possibly impaired
RECdo<-REC[REC$Characteristic.Name=="TRUE COLOR",]
RECdo<-unique(RECdo[c('LAKE_ID','SAMPLE_DATE','Result.Value')])
names(RECdo)[names(RECdo)=="Result.Value"]<-"color"
REC<-merge(RECdo,REC,all.x = TRUE)
REC$PIrecreation<-ifelse(REC$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH" & REC$color<REC$threshold2,
                         ifelse(REC$Result.Value<REC$threshold,1,REC$PIrecreation),
                         REC$PIrecreation)
REC<-REC[!is.na(REC$PIrecreation),]
REC<-unique(REC[c('LAKE_ID','WATER','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE',
                  'SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Unit','START_DEPTH',
                  'END_DEPTH','Waterbody_Classification','DATA_PROVIDER','basin','PIrecreation')])
trend<-merge(trend,REC,all=TRUE)
trend<-trend[!is.na(trend$Characteristic.Name),]
rm(list=c('REC','RECdo'))