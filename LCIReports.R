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
setwd("C:/Rscripts/Internal_Loading")

#run the rmarkdown script for this list
library(rmarkdown)
render("LCIReports.Rmd")




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




