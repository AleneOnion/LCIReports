#PWS calculations
#Alene Onion
#March 2019

#pulling class A waters to identify possible impairments to the drinking water designated use
PWS<-intensive[grepl("A",intensive$simpleWC),]

#adding UV254:DOC
#pull unique values for non-relavent columns to add back in later
samples<-unique(PWS[c('SAMPLE_ID','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification','DATA_PROVIDER','basin','simpleWC','simpleT')])
UV254<-PWS[PWS$Characteristic.Name=='UV 254',]
names(UV254)[names(UV254)=="Result.Value"]<-"UV254"
UV254<-unique(UV254[c('SAMPLE_ID','UV254')])
DOC<-PWS[PWS$Characteristic.Name=='DISSOLVED ORGANIC CARBON',]
names(DOC)[names(DOC)=="Result.Value"]<-"DOC"
DOC<-unique(DOC[c('SAMPLE_ID','DOC')])
UVDOC<-merge(UV254,DOC,all=TRUE)
UVDOC<-UVDOC[!is.na(UVDOC$SAMPLE_ID),]
UVDOC<-UVDOC[!is.na(UVDOC$UV254),]
UVDOC<-UVDOC[!is.na(UVDOC$DOC),]
rm(list=c('UV254','DOC'))
UVDOC$Characteristic.Name<-"UVDOC"
UVDOC$Result.Value<-(UVDOC$UV254)/UVDOC$DOC
#now add back in non-relavent columns
UVDOC<-merge(UVDOC,samples,all.x = TRUE)
UVDOC<-unique(UVDOC[c('SAMPLE_ID','Characteristic.Name','Result.Value','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification','DATA_PROVIDER','basin','simpleWC','simpleT')])
#create trend file
trend<-merge(PWS,UVDOC,all=TRUE)
rm(list=c('UVDOC','samples'))

#separate out chlorophyll which has to merge by class as well as characteristic name
trend.chl<-trend[trend$Characteristic.Name=="CHLOROPHYLL A",]
#remove depth profile values
trend.chl<-trend.chl[trend.chl$INFO_TYPE=="OW",]
trend<-trend[trend$Characteristic.Name!="CHLOROPHYLL A",]

#add thresholds
trend<-merge(trend,thresholds,by=c('Characteristic.Name'),all.x=TRUE)
trend<-trend[!is.na(trend$threshold),]
trend<-trend[trend$Designated_Use=="drinking_water",]
trend<-trend[!is.na(trend$Result.Value),]
trend.chl<-merge(trend.chl,thresholds,by=c('Characteristic.Name','simpleWC'),all.x = TRUE)
trend.chl<-trend.chl[!is.na(trend.chl$threshold),]
trend.chl<-trend.chl[trend.chl$Designated_Use=="drinking_water",]
trend.chl<-trend.chl[!is.na(trend.chl$Result.Value),]

#identify possibly impaired
trend$PIdrinking<-NA
trend$PIdrinking<-ifelse(trend$Result.Value>trend$threshold,1,trend$PIdrinking)
trend<-unique(trend[c('SAMPLE_ID','Characteristic.Name','Result.Value','Result.Unit','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification','DATA_PROVIDER','basin','PIdrinking')])
#now for Chl
trend.chl$PIdrinking<-NA
trend.chl$PIdrinking<-ifelse(trend.chl$Result.Value>trend.chl$threshold,1,trend.chl$PIdrinking)
trend.chl<-unique(trend.chl[c('SAMPLE_ID','Characteristic.Name','Result.Value','Result.Unit','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification','DATA_PROVIDER','basin','PIdrinking')])
#merge together
trend<-merge(trend,trend.chl,all=TRUE)
trend<-trend[!is.na(trend$PIdrinking),]
rm(list=c('trend.chl','PWS'))
