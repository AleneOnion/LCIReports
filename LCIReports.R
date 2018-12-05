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

for(lake in unique(data$LAKE_ID)){
  rmarkdown::render('source/report.Rmd',  # file 2
                    output_file =  paste("report_", lake, '_', Sys.Date(), ".html", sep=''), 
                    output_dir = 'reports')
}



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




