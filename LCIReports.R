#script to rule it all
#alene Onion
# November 2018

#set working directory
setwd("L:/DOW/StreamDatabase/Lakes/data")

#read data tables
profiles<-read.csv("Depth.Profile.csv")
location<-read.csv("Location.csv")
results<-read.csv("Test.Results.csv")
sample<-read.csv("Sample.csv")
lake<-read.csv("Lake.Master.csv")

#set working directory
setwd("C:/Rscripts/LCIReports")

source('source/Lakes.R')
databackup<-data



#pull the lake ids for each location and use these to replace the lake id field which is incomplete
#this removes all the data without location ids but I'm ok with that
lakeids<-unique(data[c('LOCATION_ID','LAKE_ID')])
lakeids<-lakeids[!is.na(lakeids$LOCATION_ID),]
lakeids<-lakeids[!is.na(lakeids$LAKE_ID),]
data<-data[,!(names(data) %in% c('LAKE_ID'))]
data<-merge(data,lakeids,by=c('LOCATION_ID'),all=TRUE)
rm(lakeids)

#pull the dates for each sampleid and use these to replace the date field which is incomplete
#this removes all the data without sample ids but I'm ok with that
lakeids<-unique(data[c('SAMPLE_ID','SAMPLE_DATE')])
lakeids<-lakeids[!is.na(lakeids$SAMPLE_ID),]
lakeids<-lakeids[!is.na(lakeids$SAMPLE_DATE),]
data<-data[,!(names(data) %in% c('SAMPLE_DATE'))]
data<-merge(data,lakeids,by=c('SAMPLE_ID'),all=TRUE)
rm(lakeids)

#for the sake of this test, I'm restricting the data set to only those records that have secchi and depth profiles
secchi<-data[data$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",]
secchi<-unique(secchi[c('LAKE_ID','LOCATION_ID')])
profiles<-data[data$INFO_TYPE=="DP",]
profiles<-unique(profiles[c('LAKE_ID','LOCATION_ID')])
secchi<-merge(profiles,secchi,by=c('LAKE_ID','LOCATION_ID'),all=FALSE)
data<-merge(data,secchi,by=c('LAKE_ID','LOCATION_ID'),all=FALSE)
rm(list = c('secchi','profiles'))


#long term trend data
temp<-data[data$Characteristic.Name=="PHOSPHORUS"|data$Characteristic.Name=="TOTAL PHOSPHORUS, MIXED FORMS"|data$Characteristic.Name=="ORTHOPHOSPHATE",]
temp<-unique(temp[c('LAKE_ID','LOCATION_ID','SAMPLE_NAME','SAMPLE_ID','Characteristic.Name','SAMPLE_DATE','INFO_TYPE','START_DEPTH','END_DEPTH','Depth','TIME')])
temp<-temp[!is.na(temp$Characteristic.Name),]



lakes<-unique(data$LAKE_ID)

for(lake in lakes){
  temp<-data[data$LAKE_ID==lake,]
  temp<-temp[!is.na(temp$LOCATION_ID),]
  temp<-temp[!is.na(temp$Characteristic.Name),]
  #for the title of the file and the report
  water<-unique(temp$WATER)
  water<-water[!is.na(water)]
  water<-tail(water,1)
  thistitle<-paste("LCI Report for ",water,sep='')
  rmarkdown::render('source/report.Rmd',  # file 2
                    output_file =  paste("report_", water,"(",lake,")_", Sys.Date(), ".html", sep=''), 
                    output_dir = 'reports')
  rm(list = c('temp','water','thistitle'))
}
rm(list = c('lake','lakes'))




rm(list=c('lake','lakes','param','params','thistitle','water','temp','profile','profile1'))

bprofiles<-profiles
blocation<-location
bresults<-results
bsample<-sample
blake<-lake

profiles<-bprofiles
location<-blocation
results<-bresults
sample<-bsample
lake<-blake
rm(list = c('data'))

data<-databackup


surface and deep distinguished between using start depth

PHOSPHORUS
TOTAL PHOSPHORUS, MIXED FORMS
ORTHOPHOSPHATE

CHLOROPHYLL A
CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)
CHLOROPHYLL A (PROBE) CONCENTRATION, CHLOROPHYTE (GREEN ALGAE)
CHLOROPHYLL A (PROBE) CONCENTRATION, CRYPTOPHYTA (CRYPTOPHYTES)
CHLOROPHYLL A (PROBE) CONCENTRATION, CYANOBACTERIA (BLUEGREEN)
CHLOROPHYLL A (PROBE) CONCENTRATION, DINOPHYTA (DIATOMS)

NITROGEN, NITRATE-NITRITE
INORGANIC NITROGEN (NITRATE AND NITRITE)
NITROGEN
TOTAL NITROGEN, MIXED FORMS
NITROGEN, NITRATE (AS N)
INORGANIC NITROGEN
NITROGEN, KJELDAHL, TOTAL

CALCIUM

APPARENT COLOR
COLOR, UNKNOWN
TRUE COLOR

PH
PH FOR COLOR ANALYSIS
PH 

CONDUCTIVITY
SPECIFIC CONDUCTANCE

TEMPERATURE, WATER
TEMPERATURE, AIR


