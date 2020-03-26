#Calculating trophic status
#ALene Onion
#March 2019

#add thresholds
short<-unique(thresholds[c('Designated_Use','Characteristic.Name','threshold')])
trophic<-merge(intensive,short,by=c('Characteristic.Name'),all.x=TRUE)
rm(short)
trophic<-trophic[trophic$Designated_Use=="eutrophic"|trophic$Designated_Use=="mesotrophic"|trophic$Designated_Use=="oligotrophic",]
trophic<-trophic[trophic$INFO_TYPE=="OW"|trophic$INFO_TYPE=="SD",]
trophic<-trophic[trophic$Result.Sample.Fraction=="T"|is.na(trophic$Result.Sample.Fraction),]
trophic<-trophic[!is.na(trophic$Result.Value),]


#detect possible eutrophic samples
eutrophic<-trophic[trophic$Designated_Use=="eutrophic",]
eutrophic$eutrophic<-NA
eutrophic$eutrophic<-ifelse(eutrophic$Result.Value>eutrophic$threshold,1,eutrophic$eutrophic)
eutrophic$eutrophic<-ifelse(eutrophic$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",NA,eutrophic$eutrophic)
eutrophic$eutrophic<-ifelse(eutrophic$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",
                            ifelse(eutrophic$Result.Value<eutrophic$threshold,1,eutrophic$eutrophic),
                            eutrophic$eutrophic)
eutrophic<-eutrophic[!is.na(eutrophic$eutrophic),]
eutrophic<-unique(eutrophic[c('LAKE_ID','WATER','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE',
                              'SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Unit','START_DEPTH',
                              'END_DEPTH','Waterbody_Classification','DATA_PROVIDER','basin','eutrophic')])
trophics<-eutrophic
trophics<-trophics[!is.na(trophics$Characteristic.Name),]
rm(list=c('eutrophic'))

#detect possible mesotrophic samples
mesotrophic<-trophic[trophic$Designated_Use=="mesotrophic",]
mesotrophic$mesotrophic<-NA
mesotrophic$mesotrophic<-ifelse(mesotrophic$Result.Value>mesotrophic$threshold,1,mesotrophic$mesotrophic)
mesotrophic$mesotrophic<-ifelse(mesotrophic$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",NA,mesotrophic$mesotrophic)
mesotrophic$mesotrophic<-ifelse(mesotrophic$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",
                                ifelse(mesotrophic$Result.Value<mesotrophic$threshold,1,mesotrophic$mesotrophic),
                                mesotrophic$mesotrophic)
mesotrophic<-mesotrophic[!is.na(mesotrophic$mesotrophic),]
mesotrophic<-unique(mesotrophic[c('LAKE_ID','WATER','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE',
                                  'SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Unit','START_DEPTH',
                                  'END_DEPTH','Waterbody_Classification','DATA_PROVIDER','basin','mesotrophic')])
trophics<-merge(trophics,mesotrophic,all=TRUE)
trophics<-trophics[!is.na(trophics$Characteristic.Name),]
rm(list=c('mesotrophic'))

#detect possible oligotrophic samples
oligotrophic<-trophic[trophic$Designated_Use=="oligotrophic",]
oligotrophic$oligotrophic<-NA
oligotrophic$oligotrophic<-ifelse(oligotrophic$Result.Value<oligotrophic$threshold,1,oligotrophic$oligotrophic)
oligotrophic$oligotrophic<-ifelse(oligotrophic$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",NA,oligotrophic$oligotrophic)
oligotrophic$oligotrophic<-ifelse(oligotrophic$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",
                                  ifelse(oligotrophic$Result.Value>oligotrophic$threshold,1,oligotrophic$oligotrophic),
                                  oligotrophic$oligotrophic)
oligotrophic<-oligotrophic[!is.na(oligotrophic$oligotrophic),]
oligotrophic<-unique(oligotrophic[c('LAKE_ID','WATER','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE',
                                    'SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Unit','START_DEPTH',
                                    'END_DEPTH','Waterbody_Classification','DATA_PROVIDER','basin','oligotrophic')])
trophics<-merge(trophics,oligotrophic,all=TRUE)
trophics<-trophics[!is.na(trophics$Characteristic.Name),]
rm(list=c('oligotrophic','trophic'))

#remove mesotrophic if eutrophic is triggered
trophics$mesotrophic<-ifelse(is.na(trophics$eutrophic),trophics$mesotrophic,NA)

#merge with trend
trend<-merge(trend,trophics,all=TRUE)
rm(trophics)
