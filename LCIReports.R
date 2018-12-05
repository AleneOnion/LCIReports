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

lakes<-unique(data$LAKE_ID)

for(lake in lakes){
  temp<-data[data$LAKE_ID==lake,]
  temp<-temp[!is.na(temp$LAKE_ID),]
  #for the title of the file and the report
  water<-tail(unique(temp$WATER),1)
  thistitle<-paste("LCI Report for ",water,sep='')
  rmarkdown::render('source/report.Rmd',  # file 2
                    output_file =  paste("report_", water,"(",lake,")_", Sys.Date(), ".html", sep=''), 
                    output_dir = 'reports')
  rm(list = c('temp','water','thistitle'))
}
rm(list = c('lake','lakes'))


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
rm(list = c('data','Phos','DP'))




