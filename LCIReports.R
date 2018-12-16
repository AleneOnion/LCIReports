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

info<-read.csv('source/Lake_info.csv')

source('source/Lakes.R')
#create a backup that I can fall back on if I doubt the scripting below
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


#pulling the sites that have long term trend data
#surface and deep distinguished between using start depth
temp<-data[!is.na(data$START_DEPTH),]
#long term parameters
temp<-temp[temp$Characteristic.Name=='PHOSPHORUS'|
             temp$Characteristic.Name=='ORTHOPHOSPHATE'|
             temp$Characteristic.Name=='CHLOROPHYLL A'|
             temp$Characteristic.Name=='CHLOROPHYLL A (PROBE)'|
             temp$Characteristic.Name=='CHLOROPHYLL A (PROBE) CONCENTRATION, DINOPHYTA (DIATOMS)'|
             temp$Characteristic.Name=='NITROGEN, NITRATE-NITRITE'|
             temp$Characteristic.Name=='INORGANIC NITROGEN (NITRATE AND NITRITE)'|
             temp$Characteristic.Name=='NITROGEN'|
             temp$Characteristic.Name=='NITROGEN, NITRATE (AS N)'|
             temp$Characteristic.Name=='NITROGEN, KJELDAHL, TOTAL'|
             temp$Characteristic.Name=='CALCIUM'|
             temp$Characteristic.Name=='APPARENT COLOR'|
             temp$Characteristic.Name=='COLOR, UNKNOWN'|
             temp$Characteristic.Name=='TRUE COLOR'|
             temp$Characteristic.Name=='PH'|
             temp$Characteristic.Name=='PH FOR COLOR ANALYSIS'|
             temp$Characteristic.Name=='CONDUCTIVITY'|
             temp$Characteristic.Name=='SPECIFIC CONDUCTANCE'|
             temp$Characteristic.Name=='TEMPERATURE, WATER',]
temp<-temp[!is.na(temp$Characteristic.Name),]
temp<-unique(temp[c('LAKE_ID','LOCATION_ID')])
data<-merge(data,temp,by=c('LAKE_ID','LOCATION_ID'),all=FALSE)
rm(temp)

#these thresholds will define oligo meso and eutrophic thresholds for future plots
thresholds<-read.csv("source/thresholds.csv")



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






