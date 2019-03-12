#script to rule it all
#alene Onion
# November 2018

############################################################################################################
#pull historic data, insert 2018 data, and clean up data file
source('L:/DOW/StreamDatabase/Lakes/data/2018.cleanup.R')
############################################################################################################

#set working directory
setwd("C:/Rscripts/LCIReports")

#restricting to 2018 only
#convert date fields to dates
profiles$Sample.4..SAMPLE_DATE<-as.Date(profiles$Sample.4..SAMPLE_DATE,format="%m/%d/%Y")
sample$SAMPLE_DATE<-as.Date(sample$SAMPLE_DATE,format="%m/%d/%Y")
#restrict to only 2018 data
profiles<-profiles[profiles$Sample.4..SAMPLE_DATE>"2018-01-01",]
results<-results[results$Sample.5..SAMPLE_DATE>"2018-01-01",]
sample<-sample[sample$SAMPLE_DATE>"2018-01-01",]
#remove na sample date values
profiles<-profiles[!is.na(profiles$Sample.4..SAMPLE_DATE),]
profiles<-profiles[!is.na(profiles$Depth),]
results<-results[!is.na(results$Sample.5..SAMPLE_DATE),]
sample<-sample[!is.na(sample$SAMPLE_DATE),]

############################################################################################################
#simplifying and merging the tables
source('L:/DOW/StreamDatabase/Lakes/data/2018/Lakes.R')

#Fixing the data set
data$Result.Sample.Fraction[data$Result.Sample.Fraction==""]<-NA

#correct erroneous info_types
infos<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/fix.info.types.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#set working directory
setwd("C:/Rscripts/LCIReports")
infos<-unique(infos[c('LAKE_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE','new_INFO_TYPE')])
sites<-unique(data[c('LAKE_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE')])
sites<-merge(sites,infos,by=c('LAKE_ID','SAMPLE_ID','SAMPLE_NAME','INFO_TYPE'),all=TRUE)
sites$INFO_TYPE<-ifelse(!is.na(sites$new_INFO_TYPE),sites$new_INFO_TYPE,sites$INFO_TYPE)
sites$new_INFO_TYPE<-NULL
data$INFO_TYPE<-NULL
data<-merge(data,sites,by=c('LAKE_ID','SAMPLE_ID','SAMPLE_NAME'),all=TRUE)
rm(list=c('infos','sites'))

#identify LCI samples
samples<-unique(data[c('SAMPLE_ID','SAMPLE_NAME')])
samples$sample<-substring(samples$SAMPLE_NAME,1,5)
samples<-unique(samples[c('SAMPLE_NAME','sample')])
samples$DATA_PROVIDER<-NA
samples$DATA_PROVIDER<-ifelse(samples$sample=="18BLK","LCI",samples$DATA_PROVIDER)
samples$DATA_PROVIDER<-ifelse(samples$sample=="18SRB","LCI",samples$DATA_PROVIDER)
samples$DATA_PROVIDER<-ifelse(samples$sample=="18LIS","LCI",samples$DATA_PROVIDER)
samples$DATA_PROVIDER<-ifelse(samples$sample=="18CMG","LCI",samples$DATA_PROVIDER)
samples$DATA_PROVIDER<-ifelse(samples$sample=="18LHB","LCI",samples$DATA_PROVIDER)
samples$DATA_PROVIDER<-ifelse(samples$sample=="18LCB","LCI",samples$DATA_PROVIDER)
samples<-samples[!is.na(samples$DATA_PROVIDER),]
samples<-unique(samples[c('SAMPLE_NAME','DATA_PROVIDER')])
#merge back in
data<-merge(data,samples,all = TRUE)
rm(samples)
data<-data[!is.na(data$Characteristic.Name),]
#now restrict to LCI samples only
data<-data[data$DATA_PROVIDER=="LCI",]

#now restrict to only the intensive basins
intensive<-data
intensive$basin<-substring(intensive$LAKE_ID,1,2)
intensive<-intensive[intensive$basin=="17"|
                       intensive$basin=="06"|
                       intensive$basin=="10"|
                       intensive$LAKE_ID=="0902BAR0262"|
                       intensive$LAKE_ID=="1301MORXXX1"|
                       intensive$LAKE_ID=="1301THE1027"|
                       intensive$LAKE_ID=="1301THE1034"|
                       intensive$LAKE_ID=="1301UWB1031",]



############################################################################################################
#ADDING THRESHOLDS

#adding deep water DO values
DO<-intensive[intensive$Characteristic.Name=='DISSOLVED OXYGEN (DO)',]
DO<-DO[!is.na(DO$Characteristic.Name),]
DOsmall<-aggregate(Depth~SAMPLE_ID,DO,max)
DO<-merge(DOsmall,DO,by=c('SAMPLE_ID','Depth'),all.x=TRUE)
rm(DOsmall)
DO$Characteristic.Name[DO$Characteristic.Name=="DISSOLVED OXYGEN (DO)"]<-"Bottom_Oxygen"
DO$Result.Unit<-"mg/L"
trend<-merge(intensive,DO,all=TRUE)
trend<-trend[!is.na(trend$Characteristic.Name),]
rm(DO)

#adding TN:TP
#pull unique values for non-relavent columns to add back in later
samples<-unique(trend[c('SAMPLE_ID','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','WATER','Waterbody_Classification')])
NOX<-trend[trend$Characteristic.Name=='NITROGEN, NITRATE-NITRITE',]
names(NOX)[names(NOX)=="Result.Value"]<-"NOX"
NOX<-unique(NOX[c('SAMPLE_ID','NOX')])
TKN<-trend[trend$Characteristic.Name=='NITROGEN, KJELDAHL, TOTAL',]
names(TKN)[names(TKN)=="Result.Value"]<-"TKN"
TKN<-unique(TKN[c('SAMPLE_ID','TKN')])
TP<-trend[trend$Characteristic.Name=='PHOSPHORUS'&trend$Result.Sample.Fraction=='T',]
names(TP)[names(TP)=="Result.Value"]<-"TP"
TP<-unique(TP[c('SAMPLE_ID','TP')])
TNTP<-merge(NOX,TKN,all=TRUE)
TNTP<-merge(TNTP,TP,all=TRUE)
TNTP<-TNTP[!is.na(TNTP$SAMPLE_ID),]
TNTP<-TNTP[!is.na(TNTP$NOX),]
TNTP<-TNTP[!is.na(TNTP$TKN),]
TNTP<-TNTP[!is.na(TNTP$TP),]
rm(list=c('NOX','TKN','TP'))
TNTP$Characteristic.Name<-"TNTP"
TNTP$Result.Value<-(TNTP$NOX+TNTP$TKN)/TNTP$TP
#now add back in non-relavent columns
TNTP<-merge(TNTP,samples,all.x = TRUE)
TNTP<-unique(TNTP[c('SAMPLE_ID','Characteristic.Name','Result.Value','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification')])
#merge back with trend
trend<-merge(trend,TNTP,all=TRUE)
rm(list=c('TNTP','samples'))

#adding UV254:DOC
#pull unique values for non-relavent columns to add back in later
samples<-unique(trend[c('SAMPLE_ID','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','WATER','Waterbody_Classification')])
UV254<-trend[trend$Characteristic.Name=='UV 254',]
names(UV254)[names(UV254)=="Result.Value"]<-"UV254"
UV254<-unique(UV254[c('SAMPLE_ID','UV254')])
DOC<-trend[trend$Characteristic.Name=='DISSOLVED ORGANIC CARBON',]
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
UVDOC<-unique(UVDOC[c('SAMPLE_ID','Characteristic.Name','Result.Value','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification')])
#merge back with trend
trend<-merge(trend,UVDOC,all=TRUE)
rm(list=c('UVDOC','samples'))

#these thresholds will define oligo meso and eutrophic thresholds for future plots
thresholds<-read.csv("sections/data/thresholds.csv", stringsAsFactors=FALSE)
thresholds<-thresholds[thresholds$thresholdMAX!=0,]

#add thresholds
trend<-merge(trend,thresholds,by=c('Characteristic.Name','Result.Sample.Fraction'),all=TRUE)

#calculate assessments state
trend$assessments<-NA
trend$assessments<-ifelse(is.na(trend$thresholdMAX),NA,ifelse(trend$Result.Value>trend$thresholdMAX,"eutrophic",ifelse(trend$Result.Value>trend$thresholdMIN,"mesotrophic","oligotrophic")))
#now doing specific calculations for pH and secchi
trend$assessments<-ifelse(trend$Characteristic.Name=="PH",ifelse(trend$Result.Value>trend$thresholdMAX,"high PH",ifelse(trend$Result.Value<trend$thresholdMIN,"low PH","healthy")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",ifelse(trend$Result.Value<trend$thresholdMIN,"eutrophic",ifelse(trend$Result.Value<trend$thresholdMAX,"mesotrophic","oligotrophic")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="DISK, SECCHI DISK DEPTH",ifelse(trend$Result.Value<trend$thresholdMIN,"eutrophic",ifelse(trend$Result.Value<trend$thresholdMAX,"mesotrophic","oligotrophic")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="Bottom_Oxygen",ifelse(trend$Result.Value>trend$thresholdMAX,"healthy DO",ifelse(trend$Result.Value<trend$thresholdMIN,"anoxic","lower DO than WQS")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="UVDOC",ifelse(trend$Result.Value>trend$thresholdMAX,"UVDOC high","healthy UVDOC"),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="CALCIUM",ifelse(trend$Result.Value>trend$thresholdMAX,"high CA",ifelse(trend$Result.Value<trend$thresholdMIN,"low CA","healthy")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="SPECIFIC CONDUCTANCE",ifelse(trend$Result.Value>trend$thresholdMAX,"hardwater",ifelse(trend$Result.Value<trend$thresholdMIN,"softwater","healthy")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="TNTP",ifelse(trend$Result.Value>trend$thresholdMAX,"high TNTP",ifelse(trend$Result.Value<trend$thresholdMIN,"low TNTP","healthy")),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="TRUE COLOR",ifelse(trend$Result.Value>trend$thresholdMAX,"high color","healthy"),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="PHOSPHORUS",(ifelse(trend$INFO_TYPE=="BS","healthy",trend$assessments)),trend$assessments)
trend$assessments<-ifelse(trend$Characteristic.Name=="CHLOROPHYLL A",(ifelse(trend$INFO_TYPE=="DP","ignore ChlA from DP",trend$assessments)),trend$assessments)

#remove rows with NA in assessments state and simplify data set
trend<-unique(trend[c('assessments','LAKE_ID','WATER','Waterbody_Classification','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Sample.Fraction','Result.Unit',
                      'INFO_TYPE','START_DEPTH','END_DEPTH','VALIDATOR_QUALIFIERS','INTERPRETED_QUALIFIERS','Depth','thresholdMIN','thresholdMAX','DATA_PROVIDER')])
trend<-trend[!is.na(trend$Characteristic.Name),]

#create a simplified list of the assessments results only
trendsimple<-plyr::count(trend,vars=c('LAKE_ID','Waterbody_Classification','WATER','assessments'))
trendsimple$assessments<-factor(trendsimple$assessments,levels=c("eutrophic","mesotrophic","oligotrophic","anoxic","lower DO than WQS","healthy DO","low PH","high PH","high color","UVDOC high","healthy UVDOC","high CA","low CA","hardwater","softwater","healthy","ignore ChlA from DP"))
trendsimple<-trendsimple[order(trendsimple$LAKE_ID,trendsimple$assessments),]
#add basin
trendsimple$basin<-substring(trendsimple$LAKE_ID,1,2)
trendsimple$basin<-paste("_",trendsimple$basin,sep="")
#reorder columns
trendsimple<-unique(trendsimple[c('basin','LAKE_ID','Waterbody_Classification','WATER','assessments','freq')])

#spread the assessments
library(tidyr)
trendsimple <- trendsimple %>% 
  spread(assessments, freq, fill = 0)

#add a count of rejected data
errors<-unique(trend[c('LAKE_ID','SAMPLE_ID','INFO_TYPE','Characteristic.Name','VALIDATOR_QUALIFIERS')])
errors<-errors[errors$VALIDATOR_QUALIFIERS=="R",]
errors<-errors[errors$INFO_TYPE=="OW",]
errors<-errors[!is.na(errors$Characteristic.Name),]
errors1<-plyr::count(errors$LAKE_ID)
errors1$freq<-ifelse(is.na(errors1$freq),0,errors1$freq)
names(errors1)[names(errors1)=="x"]<-"LAKE_ID"
names(errors1)[names(errors1)=="freq"]<-"rejected epi data"
#now produce a table of summarized list of errors per lake
library(dplyr)
errors<-errors %>% 
  group_by(LAKE_ID) %>% 
  summarise(errors = paste(unique(Characteristic.Name), collapse = ', '))
#add it back to the trendsimple table
trendsimple<-merge(trendsimple,errors1,by=c('LAKE_ID'),all=TRUE)
trendsimple<-merge(trendsimple,errors,by=c('LAKE_ID'),all=TRUE)
rm(list=c('errors','errors1'))

#add eutrophic2 to subtract errors
trendsimple$eut.wo.err<-NA
trendsimple$eut.wo.err<-ifelse(grepl("PHOSPHORUS",trendsimple$errors),ifelse(trendsimple$eutrophic==0,trendsimple$eutrophic,trendsimple$eutrophic-1),trendsimple$eutrophic)

#add needs verification list from waterbody inventory
waterinv<-read.csv("sections/data/Waterbody.Inventory.Input.csv", stringsAsFactors=FALSE)
waterinv<-waterinv[!is.na(waterinv$BASIN_CODE),]
pwl<-unique(blake[c('LakeID','PWLID')])
pwl$basin<-substring(pwl$LakeID,1,2)
pwl$basin<-paste("_",pwl$basin,sep="")
waterinv<-merge(waterinv,pwl,by=c('PWLID'),all.x = TRUE)
waterinv<-unique(waterinv[c('PWLID','LakeID','basin')])
names(waterinv)[names(waterinv)=="LakeID"]<-"LAKE_ID"
rm(pwl)
waterinv$WIPWL<-"needs verification"
trendsimple<-merge(trendsimple,waterinv,by=c('LAKE_ID','basin'),all=TRUE)

#now order table
trendsimple<-trendsimple %>%
  select(LAKE_ID,basin,Waterbody_Classification,WATER,WIPWL,eut.wo.err,eutrophic,everything()) %>%
  arrange(basin,WIPWL,desc(eut.wo.err),Waterbody_Classification,desc(anoxic),desc(`lower DO than WQS`))

#remove WI entries which make a mess
#trendsimple<-trendsimple[is.na(trendsimple$WIPWL),]
#trendsimple<-trendsimple[!is.na(trendsimple$basin),]

#write output
write.csv(trend,file="2018.Data.for.review.csv",row.names=FALSE)
write.csv(trendsimple,file="2018.Data.for.review.simple.csv",row.names=FALSE)



rmarkdown::render("report.rmd")

###########################################################################################################
#lakes<-unique(data$LAKE_ID)

#for(lake in lakes){
#  temp<-data[data$LAKE_ID==lake,]
#  temp<-temp[!is.na(temp$Characteristic.Name),]
#  #for the title of the file and the report
#  water<-unique(temp$WATER)
#  water<-water[!is.na(water)]
#  water<-tail(water,1)
#  thistitle<-paste("LCI Report for ",water,sep='')
#  rmarkdown::render('report.Rmd',  # file 2
#                    output_file =  paste("report_", water,"(",lake,")_", Sys.Date(), ".html", sep=''), 
#                    output_dir = 'reports')
#  rm(list = c('temp','water','thistitle'))
#}
#rm(list = c('lake','lakes'))
###########################################################################################################


