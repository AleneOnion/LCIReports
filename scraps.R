#and to return to these copies for rinse and repeat:
profiles<-bprofiles
sample<-bsample
location<-blocation
results<-bresults
lake<-blake
habs<-bhabs
rm(list = c('data','thresholds','results2018','DP','trend','junk','info','notresults','trend','thresholds',
            'trophic','p','profile','profiles','temp','i','lakes','nlakes','nparams','params','display','trendlakes','DO',
            'errors','trendsimple'))

#creating pivot table of samples present per lake/sample date
junk<-data
junk<-unique(junk[c('LAKE_ID','SAMPLE_DATE','INFO_TYPE','DATA_PROVIDER')])
junk<-junk[!is.na(junk$LAKE_ID),]
junk<-junk[junk$DATA_PROVIDER=="LCI",]
junk<-junk[!is.na(junk$LAKE_ID),]
junk$presence<-1
junk$INFO_TYPE<-factor(junk$INFO_TYPE,levels=c('DP','OW','BS','SB','SD'))
library(tidyr)
junk<-junk %>%
  spread(INFO_TYPE,presence,fill=0)
write.csv(junk,file="jesse.simpler.csv",row.names=FALSE)


#checking if sample ids match in profiles and sample table
profiles<-bprofiles
sample<-bsample
sample<-unique(sample[c("SAMPLE_ID","SAMPLE_NAME","INFO_TYPE")])
sample<-sample[sample$INFO_TYPE=="DP",]
sample$SAMPLE_NAME<-gsub('_..$','',sample$SAMPLE_NAME)
names(sample)[names(sample)=="SAMPLE_ID"]<-"SAMPLE_SAMPLE_ID"
sample<-sample[!is.na(sample$SAMPLE_SAMPLE_ID),]

profiles<-unique(profiles[c("Sample_ID","DP_Key","Sample.4..SAMPLE_DATE")])
profiles$SAMPLE_NAME<-gsub('_DP$','',profiles$DP_Key)
names(profiles)[names(profiles)=="Sample_ID"]<-"PROFILES_SAMPLE_ID"
names(profiles)[names(profiles)=="Sample.4..SAMPLE_DATE"]<-"PROFILES_SAMPLE_DATE"

combined<-merge(profiles,sample,by=c('SAMPLE_NAME'),all.x = TRUE)
combined<-combined[!is.na(combined$PROFILES_SAMPLE_ID),]
combined$match<-ifelse(combined$PROFILES_SAMPLE_ID==combined$SAMPLE_SAMPLE_ID,"match","no")
combined<-combined[combined$match=="no",]
combined<-combined[!is.na(combined$SAMPLE_NAME),]
write.csv(combined,file="mismatched.sample.ids.csv",row.names = FALSE)
tail(combined)

#checking if sample ids match in secchi and sample table
results<-bresults
sample<-bsample
sample<-unique(sample[c("SAMPLE_ID","SAMPLE_NAME","INFO_TYPE")])
sample<-sample[sample$INFO_TYPE=="SD",]
sample$SAMPLE_NAME<-gsub('_..$','',sample$SAMPLE_NAME)
names(sample)[names(sample)=="SAMPLE_ID"]<-"SAMPLE_SAMPLE_ID"
sample<-sample[!is.na(sample$SAMPLE_SAMPLE_ID),]

results<-unique(results[c("SAMPLE_ID","Sample.5..SAMPLE_DATE","SAMPLE_NAME")])
results<-results[grep("_SD$",results$SAMPLE_NAME),]
names(results)[names(results)=="SAMPLE_NAME"]<-"SD_SAMPLE_NAME"
results$SAMPLE_NAME<-gsub('_SD$','',results$SD_SAMPLE_NAME)
names(results)[names(results)=="Sample.5..SAMPLE_DATE"]<-"SD_SAMPLE_DATE"
names(results)[names(results)=="SAMPLE_ID"]<-"SD_SAMPLE_ID"

combined<-merge(results,sample,by=c('SAMPLE_NAME'),all.x = TRUE)
combined<-combined[!is.na(combined$SD_SAMPLE_ID),]
combined$match<-ifelse(combined$SD_SAMPLE_ID==combined$SAMPLE_SAMPLE_ID,"match","no")
combined<-combined[combined$match=="no",]
combined<-combined[!is.na(combined$SAMPLE_NAME),]
write.csv(combined,file="mismatched.SD.ids.csv",row.names = FALSE)
tail(combined)

#figuring out which files in the thumb drive we don't have
results2018<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/LCI.2018.raw.concatinated.old.csv", stringsAsFactors=FALSE)
thumb<-read.csv("L:/DOW/SMAS/LabData/ChemData/2018/ALS thumb drives/LMAS/LCI/thumbs.csv", stringsAsFactors=FALSE)
results2018<-unique(results2018[c('sample_delivery_group','lab_name_code')])
names(thumb)[names(thumb)=="thumbs"]<-"sample_delivery_group"
thumb$thumb<-"X"
results2018$lab_name_code<-NULL
results2018$have<-"X"
together<-merge(thumb,results2018,by=c('sample_delivery_group'),all=TRUE)
together<-together[is.na(together$have),]
#these files need to be separated into test, batch, sample text files before tranferring
#R1805953
#R1808598
#R1808882
#R1809020
library(rmarkdown)
library(knitr)
library(kableExtra)
library(xlsx)
revisions<-read.xlsx("markdown/inputs/PEERS.xlsx" , 2, header=TRUE)
participants<-read.xlsx("markdown/inputs/PEERS.xlsx", 3, header=TRUE)
descriptions<-read.xlsx("markdown/inputs/PEERS.xlsx", 4, header=TRUE)
parameters<-read.xlsx("markdown/inputs/PEERS.xlsx", 5, header=TRUE)
#now pull the unique values to add to old data table
results2018<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/LCI.raw.concatenated.csv", stringsAsFactors=FALSE)
results2018<-merge(together,results2018,by=c('sample_delivery_group'),all.x = TRUE)
results2018$thumb<-NULL
results2018$have<-NULL
results2018<-results2018[!is.na(results2018$sys_loc_code),]
write.csv(results2018,"new.raw.data.csv",row.names=FALSE)

#for stephanie
results<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Test.Results.csv", stringsAsFactors=FALSE)
results<-unique(results[c('SAMPLE_NAME','Characteristic.Name','LAB_NAME_CODE')])
steph<-read.csv("C:/Rscripts/LCIReports/stephanie.csv")
steph<-unique(steph[c('SAMPLE_NAME','Characteristic.Name','labnamecode')])
names(steph)[names(steph)=="labnamecode"]<-"LAB_NAME_CODE"
#names(steph)[names(steph)=="Lab.Sample.Name"]<-"Lab_Sample_Name"
steph$steph<-"X"
results$results<-"X"
excluded<-merge(steph,results,by=c('SAMPLE_NAME','Characteristic.Name','LAB_NAME_CODE'),all.x = TRUE)
excluded<-excluded[is.na(excluded$results),]
write.csv(excluded,file="excluded.for.stephanie.csv",row.names=FALSE)

#checking if sites are in sample table 
newdata<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/new.raw.data.csv",stringsAsFactors=FALSE)
newdata<-unique(newdata[c('sys_sample_code','lab_name_code')])
newdata$sample<-substring(newdata$sys_sample_code,1,5)
newdata$DATA_PROVIDER<-NA
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18BLK","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18SRB","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18LIS","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18CMG","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18LHB","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18LCB","LCI",newdata$DATA_PROVIDER)
newdata<-newdata[newdata$DATA_PROVIDER=="LCI",]
newdata<-newdata[!is.na(newdata$sys_sample_code),]
newdata$sys_sample_code<-gsub('MSD$','',newdata$sys_sample_code)
newdata$sys_sample_code<-gsub('MS$','',newdata$sys_sample_code)
newdata$sys_sample_code<-gsub('18......DUP','junk',newdata$sys_sample_code)
newdata<-newdata[newdata$sys_sample_code!="junk",]
names(newdata)[names(newdata)=="sys_sample_code"]<-"SAMPLE_NAME"
newdata$newdata<-"X"
newdata$sample<-NULL
newdata$DATA_PROVIDER<-NULL
newdata$lab_name_code<-NULL
head(newdata)
sample<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Sample.csv", stringsAsFactors=FALSE)
sample<-unique(sample[c('SAMPLE_NAME','SAMPLE_ID')])
head(sample)
sample<-merge(newdata,sample,by=c('SAMPLE_NAME'),all.x = TRUE)
head(sample)
sample<-sample[is.na(sample$SAMPLE_ID),]
sample<-unique(sample[c('SAMPLE_NAME','SAMPLE_ID')])
sample
length(sample$SAMPLE_NAME)
newdata<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/new.raw.data.csv",stringsAsFactors=FALSE)
newdata<-unique(newdata[c('sys_sample_code','sample_date')])
names(newdata)[names(newdata)=="sys_sample_code"]<-"SAMPLE_NAME"
sample<-merge(sample,newdata,by=c('SAMPLE_NAME'),all.x = TRUE)
sample
length(sample$SAMPLE_NAME)

#pulling syssamplecode duplicates
junk$sample<-substring(junk$sys_sample_code,1,5)
junk$DATA_PROVIDER<-NA
junk$DATA_PROVIDER<-ifelse(junk$sample=="18BLK","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18SRB","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18LIS","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18CMG","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18LHB","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18LCB","LCI",junk$DATA_PROVIDER)
junk<-junk[junk$DATA_PROVIDER=="LCI",]
junk<-junk[!is.na(junk$sys_sample_code),]
length(junk$sys_sample_code)
junk2<-unique(junk$sys_sample_code)
length(junk2)
write.csv(junk,file="junk.csv",row.names = FALSE)
18LIS053	R1807212	7/31/2018 13:45	18LIS	LCI
18LHB299	R1807670	8/13/2018 13:42	18LHB	LCI


