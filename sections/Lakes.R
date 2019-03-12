#2018/12/5
#Onion
#this script reformats and merges lakes data

#renaming columns so they match
names(location)[names(location)=="LocationID"]<-"LOCATION_ID"
names(location)[names(location)=="LakeID"]<-"LAKE_ID"
names(lake)[names(lake)=="LakeID"]<-"LAKE_ID"
names(profiles)[names(profiles)=="Characteristic"]<-"Characteristic.Name"
names(profiles)[names(profiles)=="LocationID"]<-"LOCATION_ID"
names(profiles)[names(profiles)=="Result"]<-"Result.Value"
names(profiles)[names(profiles)=="Result_Value_Type"]<-"Result.Value.Type"
names(profiles)[names(profiles)=="Sample_ID"]<-"SAMPLE_ID"
names(profiles)[names(profiles)=="Unit"]<-"Result.Unit"
names(lake)[names(lake)=="LakeID"]<-"LAKE_ID"

#restrict to the necessary fields
sample<-unique(sample[c('LAKE_ID','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH')])
profiles<-unique(profiles[c('SAMPLE_ID','Characteristic.Name','Depth','Result.Value','Result.Unit')])
results<-unique(results[c('SAMPLE_NAME','Characteristic.Name','Result.Sample.Fraction','Result.Value','Result.Unit','VALIDATOR_QUALIFIERS','INTERPRETED_QUALIFIERS')])
lake<-unique(lake[c('LAKE_ID','WATER','Waterbody_Classification')])

#now capitalize all the values in these columns which overlap between the tables
#If we didn't do this, two records would be considered different if they were written as "Lake" and "lake"
results$SAMPLE_NAME<-toupper(results$SAMPLE_NAME)
sample$LAKE_ID<-toupper(sample$LAKE_ID)
sample$LOCATION_ID<-toupper(sample$LOCATION_ID)
sample$SAMPLE_NAME<-toupper(sample$SAMPLE_NAME)
sample$INFO_TYPE<-toupper(sample$INFO_TYPE)
lake$LAKE_ID<-toupper(lake$LAKE_ID)

#2. Merge the data tables
#merge results table with sample table 
data<-merge(sample,results,by=c("SAMPLE_NAME"),all.x=TRUE)

#merge with profiles data table
data2<-merge(sample,profiles,by=c("SAMPLE_ID"),all.x=TRUE)

#merge the two together
data<-merge(data,data2,by=c('LAKE_ID','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','Characteristic.Name',
                            'Result.Value','Result.Unit'),all=TRUE)
rm(data2)

#merge with lake data
data<-merge(data,lake,by=c('LAKE_ID'),all.x=TRUE)

#convert blank values to NA
data$LAKE_ID[data$LAKE_ID==""] <- NA
data$LOCATION_ID[data$LOCATION_ID==""] <- NA
data$SAMPLE_DATE[data$SAMPLE_NAME==""] <- NA
data$SAMPLE_ID[data$SAMPLE_ID==""] <- NA

#4.capitalize fields so they can be compared accurately
data$Characteristic.Name<-toupper(data$Characteristic.Name)
#5.make date field a date field
data$SAMPLE_DATE<-as.Date(data$SAMPLE_DATE,format="%m/%d/%Y")

#remove sub tables
rm(list = c('lake','profiles','location','results','sample'))


