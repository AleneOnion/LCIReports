#creating ROckland report
#alene Onion
# April 2019
library(dplyr)
data<-read.csv("sections/data/data.backup.all.csv", stringsAsFactors=FALSE)
Rockland<-data[data$LAKE_ID=="1403BIG0345",]
Rockland<-unique(Rockland[c('LAKE_ID','SAMPLE_ID','SAMPLE_NAME','LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','Characteristic.Name','Result.Value','Result.Unit','Result.Sample.Fraction','Depth','WATER','LocationName','Y_Coordinate','X_Coordinate','INFO_TYPE')])
Rockland$SAMPLE_DATE<-as.Date(Rockland$SAMPLE_DATE,format="%Y-%m-%d")


profiles<- Rockland %>% 
  filter(Characteristic.Name %in% c('TEMPERATURE, WATER','PH','SPECIFIC CONDUCTANCE','DISSOLVED OXYGEN SATURATION','DISSOLVED OXYGEN (DO)','OXIDATION REDUCTION POTENTIAL (ORP)','DEPTH, SECCHI DISK DEPTH'))
profiles$INFO_TYPE<-"DP"
profiles$INFO_TYPE<-ifelse(profiles$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH","SD",profiles$INFO_TYPE)
#add 2017 rockland secchi data
#secchi<-read.csv("sections/data/Rockland.secchi.2017.csv",stringsAsFactors = FALSE)
#secchi$SAMPLE_DATE<-as.Date(secchi$SAMPLE_DATE,format="%m/%d/%Y")
#profiles<-merge(profiles,secchi,all=TRUE)
#rm(secchi)

phosphorus<-Rockland %>% 
  filter(Characteristic.Name %in% c('PHOSPHORUS')) %>% 
  filter(DATA_PROVIDER %in% c('LCI','IL'))
phosphorus$START_DEPTH<-as.numeric(phosphorus$START_DEPTH)

titles<-Rockland$WATER[1]

rmarkdown::render("Lake.Report.rmd")


#scraps
junk<-unique(phosphorus[c('START_DEPTH','INFO_TYPE','DATA_PROVIDER','SAMPLE_NAME','SAMPLE_DATE')])
junk<-junk[order(junk$START_DEPTH),]
junk
