###########################################################################################################
#reviewing and ranking 2018 data
###########################################################################################################
#loading data
bresults<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Test.Results.csv", stringsAsFactors=FALSE)
bsample<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Sample.csv", stringsAsFactors=FALSE)
blake<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Lake.Master.csv", stringsAsFactors=FALSE)
blocation<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Location.csv", stringsAsFactors=FALSE)
bhabs<-read.csv("L:/DOW/StreamDatabase/Lakes/data/HABstatus.csv", stringsAsFactors=FALSE)

#read in 2018 data
intensive<-read.csv("sections/data/data.backup.csv", stringsAsFactors=FALSE)
intensive<-intensive[!is.na(intensive$Characteristic.Name),]
intensive$SAMPLE_DATE<-as.Date(intensive$SAMPLE_DATE,format="%Y-%m-%d")

#now had historic data for needs verification and minor impacts waterbody inventory sites
#add needs verification list from waterbody inventory
waterinv<-read.csv("sections/data/Waterbody.Inventory.Input.csv", stringsAsFactors=FALSE)
waterinv<-waterinv[!is.na(waterinv$BASIN_CODE),]
pwl<-unique(blake[c('LakeID','PWLID')])
pwl$basin<-substring(pwl$LakeID,1,2)
pwl$basin<-paste("_",pwl$basin,sep="")
waterinv<-merge(waterinv,pwl,by=c('PWLID'),all = TRUE)
waterinv<-unique(waterinv[c('PWLID','LakeID')])
names(waterinv)[names(waterinv)=="LakeID"]<-"LAKE_ID"
rm(pwl)
#read complete data set
data1<-read.csv("sections/data/data.backup.all.csv", stringsAsFactors=FALSE)
data1<-data1[!is.na(data1$Characteristic.Name),]
data1$SAMPLE_DATE<-as.Date(data1$SAMPLE_DATE,format="%Y-%m-%d")

library(dplyr)
data1<-merge(waterinv,data1,by=c('LAKE_ID','PWLID'),all = TRUE)
data1 <- data1 %>% 
  filter(Characteristic.Name %in% c("AMMONIA", "NITROGEN, NITRATE (AS N)","SODIUM","IRON","MANGANESE","MAGNESIUM","NITROGEN, NITRATE-NITRITE","PH","DEPTH, SECCHI DISK DEPTH","NITROGEN, KJELDAHL, TOTAL","SILICA","CHLORIDE (AS CL)","ALKALINITY, TOTAL (AS CACO3)","TOTAL ORGANIC CARBON","CHLOROPHYLL A","PHOSPHORUS","SULFATE (AS SO4)","TRUE COLOR","CALCIUM","SULFATE","CHLORIDE","SPECIFIC CONDUCTANCE","DEPTH, BOTTOM","SULFATE","DISSOLVED ORGANIC CARBON","NITROGEN","ARSENIC","NITRITE","TEMPERATURE, WATER","DEPTH","TEMPERATURE, AIR","DISSOLVED OXYGEN","CONDUCTIVITY"))
data1<-data1[!is.na(data1$Characteristic.Name),]

#remove samples collected before 2010
intensive<-merge(intensive,data1,all = TRUE)
library(dplyr)
intensive<-distinct(intensive)
intensive$LAKE_ID[intensive$LAKE_ID==""]<-NA
intensive<-intensive[!is.na(intensive$LAKE_ID),]
intensive$SAMPLE_DATE<-as.Date(intensive$SAMPLE_DATE,format="%m/%d/%Y")
intensive<-intensive %>% 
  mutate(year=substring(SAMPLE_DATE,1,4))
intensive<-intensive[intensive$year>2009,]
intensive$LAKE_ID[intensive$LAKE_ID==""]<-NA
intensive<-intensive[!is.na(intensive$LAKE_ID),]
intensive<-distinct(intensive)

rm(list=c('data1','bresults','bsample','bhabs','blake','blocation','waterinv'))

############################################################################################################
#Fixing info type for Phosphorus only
############################################################################################################

#write function to capture last characters in a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#define info type for cslap samples and LCI PHOSPHORUS samples (onLY phosphorus)
intensive$end<-substrRight(intensive$SAMPLE_NAME,3)
intensive$end<-substr(intensive$end,1,1)
intensive$end2<-substrRight(intensive$SAMPLE_NAME,2)
intensive$end2<-substr(intensive$end2,1,1)
#specify info type for cslap samples
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",
                            ifelse(intensive$end=="-" & intensive$end2=="0","OW",intensive$INFO_TYPE),
                            intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",
                            ifelse(intensive$end=="-" & intensive$end2=="1","BS",intensive$INFO_TYPE),
                            intensive$INFO_TYPE)
#specify info type for LCI samples
intensive$end2<-substrRight(intensive$SAMPLE_NAME,1)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="1","OW",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="3","OW",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="5","OW",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="7","OW",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="9","OW",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="0","BS",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="2","BS",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="4","BS",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="6","BS",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",ifelse(intensive$end!="-" & intensive$end2=="8","BS",intensive$INFO_TYPE),intensive$INFO_TYPE)
intensive$end<-NULL
intensive$end2<-NULL


############################################################################################################
#ADDING THRESHOLDS
############################################################################################################

#these thresholds will define thresholds for future plots
thresholds<-read.csv("sections/data/thresholds2.csv", stringsAsFactors=FALSE)
thresholds<-thresholds[thresholds$Characteristic.Name!=0,]
thresholds$notes<-NULL
library(dplyr)
thresholds$Designated_Use<-NULL
thresholds$units<-NULL
thresholds<-distinct(thresholds)
thresholds$simpleT[thresholds$simpleT==""]<-NA
thresholds$limnion[thresholds$limnion==""]<-NA
thresholds$simpleF[thresholds$simpleF==""]<-NA
thresholds$simpleF<-as.character(thresholds$simpleF)
thresholds$simpleF[thresholds$simpleF=="TRUE"]<-"T"





#simplify waterbody classification
intensive<-intensive %>% 
  select(LAKE_ID,WATER,LOCATION_ID,Waterbody_Classification,PWS,PWLID,Beaches,Y_Coordinate,X_Coordinate,Type,SAMPLE_ID,SAMPLE_NAME,DATA_PROVIDER,SAMPLE_DATE,year,TIME,Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,INFO_TYPE)
intensive$simpleWC<-NA
intensive$simpleWC<-ifelse(grepl("C",intensive$Waterbody_Classification),"C",intensive$simpleWC)
intensive$simpleWC<-ifelse(grepl("B",intensive$Waterbody_Classification),"B",intensive$simpleWC)
intensive$simpleWC<-ifelse(grepl("A",intensive$Waterbody_Classification),"A",intensive$simpleWC)
intensive<-intensive[!is.na(intensive$simpleWC),]
intensive$simpleT<-NA
intensive$simpleT<-ifelse(grepl("T",intensive$Waterbody_Classification),"T",intensive$simpleT)
intensive$simpleT<-ifelse(grepl("TS",intensive$Waterbody_Classification),"TS",intensive$simpleT)
intensive$limnion<-NA
intensive$limnion<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",intensive$INFO_TYPE,NA)
intensive$simpleF<-NA
intensive$simpleF<-ifelse(intensive$Characteristic.Name=="PHOSPHORUS",intensive$Result.Sample.Fraction,NA)
intensive<-distinct(intensive)  


#add criteria
backupintensive<-intensive
backupthresholds<-thresholds
intensive<-merge(intensive,thresholds,by=c('Characteristic.Name','simpleWC','simpleT','limnion','simpleF'),all.x=TRUE)
intensive<-distinct(intensive)
rm(thresholds)

#calculate violations
intensive$violation<-NA
intensive$violation<-ifelse(intensive$direction=="greater",
                            ifelse(intensive$Result.Value>intensive$threshold,1,0),
                            ifelse(intensive$direction=="less",
                                   ifelse(intensive$Result.Value<intensive$threshold,1,0),
                                   NA))
#remove parametres without standards
intensive<-intensive[!is.na(intensive$violation),]
intensive<-distinct(intensive)

#count violations
library(dplyr)
intensive<-intensive %>% 
  group_by(LAKE_ID,Characteristic.Name,direction) %>% 
  mutate(violationcount=sum(violation)) %>% 
  mutate(samplenumber=length(violation))%>% 
  mutate(yrnumber=n_distinct(year))%>% 
  ungroup()
assess<-intensive %>% 
  filter(violation==1) %>% 
  group_by(LAKE_ID,Characteristic.Name,direction) %>% 
  mutate(yrviol=n_distinct(year)) %>% 
  ungroup()
assess<-unique(assess[c('LAKE_ID','Characteristic.Name','yrviol','direction')])
intensive<-merge(intensive,assess,by=c('LAKE_ID','Characteristic.Name','direction'),all.x = TRUE)
rm(assess)

#can we assess
intensive$assess<-NA
intensive$assess<-ifelse(intensive$yrnumber>1,
                         ifelse(intensive$samplenumber>5,'yes','no'),
                                'no')
intensive$assessment<-ifelse(intensive$assess=="yes",
                             ifelse(intensive$violationcount>1,
                                    ifelse(intensive$yrviol>0,"impaired","other"),
                                    "other"),
                             "other")

      



#################################################################################################################
#################################################################################################################
#################################################################################################################
#################################################################################################################

length(unique(intensive$LAKE_ID))

length(unique(intensive[intensive$assess=="yes",]$LAKE_ID))

length(unique(intensive[intensive$assess=="yes"&intensive$assessment=="impaired",]$LAKE_ID))

#how many impaired are class a
length(unique(intensive[intensive$assessment=="impaired"&intensive$simpleWC=="A",]$LAKE_ID))
length(unique(intensive[intensive$assessment=="impaired"&intensive$simpleWC=="B",]$LAKE_ID))
length(unique(intensive[intensive$assessment=="impaired"&intensive$simpleWC=="C",]$LAKE_ID))

#how many impaired from each data provider
length(unique(intensive[intensive$assessment=="impaired"&intensive$simpleWC=="A",]$LAKE_ID))
length(unique(intensive[intensive$assessment=="impaired"&intensive$simpleWC=="B",]$LAKE_ID))
length(unique(intensive[intensive$assessment=="impaired"&intensive$simpleWC=="C",]$LAKE_ID))


#pull out impaired and look at the breakdown
impaired<-intensive[intensive$assessment=="impaired",]
length(unique(impaired[impaired$Characteristic.Name=="PH",]$LAKE_ID))
length(unique(impaired[impaired$Characteristic.Name=="PHOSPHORUS",]$LAKE_ID))
length(unique(impaired[impaired$Characteristic.Name=="MANGANESE",]$LAKE_ID))
length(unique(impaired[impaired$Characteristic.Name=="IRON",]$LAKE_ID))
length(unique(impaired[impaired$Characteristic.Name=="DISSOLVED OXYGEN (DO)",]$LAKE_ID))

#create list of assessment
assessment<-intensive[intensive$assess=="yes",]
assessment<-assessment %>% 
  group_by(LAKE_ID,Characteristic.Name,direction) %>% 
  mutate(minyr=min(year)) %>% 
  mutate(maxyr=max(year)) %>% 
  ungroup()
assessment<-unique(assessment[c('LAKE_ID','WATER','PWLID','Characteristic.Name','direction','assessment','minyr','maxyr')])
assessment$assessment<-paste(assessment$assessment," (",assessment$minyr,"-",assessment$maxyr,")",sep="")
assessment$Characteristic.Name<-ifelse(assessment$Characteristic.Name=="PH",
                                       ifelse(assessment$direction=="less","PH low","PH high"),
                                       assessment$Characteristic.Name)
assessment<-unique(assessment[c('LAKE_ID','WATER','PWLID','Characteristic.Name','assessment')])
library(tidyr)
assessment<-assessment %>% 
  spread(Characteristic.Name,assessment)
#pull lake acreage
blake<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Lake.Master.csv", stringsAsFactors=FALSE)
blake<-unique(blake[c('LakeID','ACRES','Waterbody_Classification')])
blake$LAKE_ID<-blake$LakeID
blake$LakeID<-NULL
assessment<-merge(assessment,blake,by=c('LAKE_ID'),all.x=TRUE)

write.csv(assessment,file="testing.the.new.calm.assessments.csv",row.names = FALSE)
