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

#write backup
write.csv(data,file="sections/data/data.backup.csv",row.names=FALSE)
data<-read.csv("sections/data/data.backup.csv", stringsAsFactors=FALSE)
data<-data[!is.na(data$Characteristic.Name),]
data$SAMPLE_DATE<-as.Date(data$SAMPLE_DATE,format="%Y-%m-%d")

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

#remove erroneous DP infotypes
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59017"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59018"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59019"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59020"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59021"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59022"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59023"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59024"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59025"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59026"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59027"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59028"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59103"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59120"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive$INFO_TYPE<-ifelse(intensive$SAMPLE_ID=="59121"&&intensive$INFO_TYPE=="DP","OW",intensive$INFO_TYPE)
intensive<-unique(intensive)

############################################################################################################
#ADDING THRESHOLDS

#these thresholds will define thresholds for future plots
thresholds<-read.csv("sections/data/thresholds.csv", stringsAsFactors=FALSE)
thresholds<-thresholds[thresholds$Characteristic.Name!=0,]
thresholds$notes<-NULL


#simplify waterbody classification
intensive$simpleWC<-NA
intensive$simpleWC<-ifelse(grepl("C",intensive$Waterbody_Classification),"C",intensive$simpleWC)
intensive$simpleWC<-ifelse(grepl("B",intensive$Waterbody_Classification),"B",intensive$simpleWC)
intensive$simpleWC<-ifelse(grepl("A",intensive$Waterbody_Classification),"A",intensive$simpleWC)
intensive$simpleWC<-ifelse(grepl("AA",intensive$Waterbody_Classification),"AA",intensive$simpleWC)
intensive<-intensive[!is.na(intensive$simpleWC),]
intensive$simpleT<-NA
intensive$simpleT<-ifelse(grepl("T",intensive$Waterbody_Classification),"T",intensive$simpleT)
intensive$simpleT<-ifelse(grepl("TS",intensive$Waterbody_Classification),"TS",intensive$simpleT)

############################################################################################################
#class A waters

#pulling class A waters to identify possible impairments to the drinking water designated use
PWS<-intensive[grepl("A",intensive$simpleWC),]

#adding UV254:DOC
#pull unique values for non-relavent columns to add back in later
samples<-unique(PWS[c('SAMPLE_ID','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','WATER','Waterbody_Classification','DATA_PROVIDER','basin','simpleWC','simpleT')])
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
trend<-unique(trend[c('SAMPLE_ID','Characteristic.Name','Result.Value','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification','DATA_PROVIDER','basin','PIdrinking')])
#now for Chl
trend.chl$PIdrinking<-NA
trend.chl$PIdrinking<-ifelse(trend.chl$Result.Value>trend.chl$threshold,1,trend.chl$PIdrinking)
trend.chl<-unique(trend.chl[c('SAMPLE_ID','Characteristic.Name','Result.Value','LAKE_ID','LOCATION_ID','SAMPLE_NAME','INFO_TYPE','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','WATER','Waterbody_Classification','DATA_PROVIDER','basin','PIdrinking')])
#merge together
trend<-merge(trend,trend.chl,all=TRUE)
rm(trend.chl)

######################################################################################################################
#merge back to intensive
intensive<-merge(intensive,trend,all = TRUE)
intensive<-intensive[!is.na(intensive$Characteristic.Name),]

#create simplified count table
#remove rejected data
#create simplified table
intsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,PIdrinking) %>%
  dplyr::group_by(LAKE_ID,WATER) %>%
  dplyr::summarize(PIdrinking = sum(PIdrinking, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(intsimple)
unique(intsimple$PIdrinking)


############################
#errors
#iron is in Depth profile data???!!!!










#calculate exceedances state
trend$exceedances<-NA
trend$exceedances<-ifelse(is.na(trend$thresholdMAX),NA,ifelse(trend$Result.Value>trend$thresholdMAX,"eutrophic",ifelse(trend$Result.Value>trend$thresholdMIN,"mesotrophic","oligotrophic")))
#now doing specific calculations for pH and secchi
trend$exceedances<-ifelse(trend$Characteristic.Name=="PH",ifelse(trend$Result.Value>trend$thresholdMAX,"high PH",ifelse(trend$Result.Value<trend$thresholdMIN,"low PH","healthy")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",ifelse(trend$Result.Value<trend$thresholdMIN,"eutrophic",ifelse(trend$Result.Value<trend$thresholdMAX,"mesotrophic","oligotrophic")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="DISK, SECCHI DISK DEPTH",ifelse(trend$Result.Value<trend$thresholdMIN,"eutrophic",ifelse(trend$Result.Value<trend$thresholdMAX,"mesotrophic","oligotrophic")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="Bottom_Oxygen",ifelse(trend$Result.Value>trend$thresholdMAX,"healthy DO",ifelse(trend$Result.Value<trend$thresholdMIN,"anoxic","lower DO than WQS")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="UVDOC",ifelse(trend$Result.Value>trend$thresholdMAX,"UVDOC high","healthy UVDOC"),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="CALCIUM",ifelse(trend$Result.Value>trend$thresholdMAX,"high CA",ifelse(trend$Result.Value<trend$thresholdMIN,"low CA","healthy")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="SPECIFIC CONDUCTANCE",ifelse(trend$Result.Value>trend$thresholdMAX,"hardwater",ifelse(trend$Result.Value<trend$thresholdMIN,"softwater","healthy")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="TNTP",ifelse(trend$Result.Value>trend$thresholdMAX,"high TNTP",ifelse(trend$Result.Value<trend$thresholdMIN,"low TNTP","healthy")),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="TRUE COLOR",ifelse(trend$Result.Value>trend$thresholdMAX,"high color","healthy"),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="PHOSPHORUS",(ifelse(trend$INFO_TYPE=="BS","healthy",trend$exceedances)),trend$exceedances)
trend$exceedances<-ifelse(trend$Characteristic.Name=="CHLOROPHYLL A",(ifelse(trend$INFO_TYPE=="DP","ignore ChlA from DP",trend$exceedances)),trend$exceedances)

#remove rows with NA in exceedances state and simplify data set
trend<-unique(trend[c('exceedances','LAKE_ID','WATER','Waterbody_Classification','LOCATION_ID','SAMPLE_ID','SAMPLE_NAME','SAMPLE_DATE','TIME','Characteristic.Name','Result.Value','Result.Sample.Fraction','Result.Unit',
                      'INFO_TYPE','START_DEPTH','END_DEPTH','VALIDATOR_QUALIFIERS','INTERPRETED_QUALIFIERS','Depth','thresholdMIN','thresholdMAX','DATA_PROVIDER')])
trend<-trend[!is.na(trend$Characteristic.Name),]

#create a simplified list of the exceedances results only
trendsimple<-plyr::count(trend,vars=c('LAKE_ID','Waterbody_Classification','WATER','exceedances'))
trendsimple$exceedances<-factor(trendsimple$exceedances,levels=c("eutrophic","mesotrophic","oligotrophic","anoxic","lower DO than WQS","healthy DO","low PH","high PH","high color","UVDOC high","healthy UVDOC","high CA","low CA","hardwater","softwater","healthy","ignore ChlA from DP"))
trendsimple<-trendsimple[order(trendsimple$LAKE_ID,trendsimple$exceedances),]
#add basin
trendsimple$basin<-substring(trendsimple$LAKE_ID,1,2)
trendsimple$basin<-paste("_",trendsimple$basin,sep="")
#reorder columns
trendsimple<-unique(trendsimple[c('basin','LAKE_ID','Waterbody_Classification','WATER','exceedances','freq')])

#spread the exceedances
library(tidyr)
trendsimple <- trendsimple %>% 
  spread(exceedances, freq, fill = 0)

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
#waterinv<-read.csv("sections/data/Waterbody.Inventory.Input.csv", stringsAsFactors=FALSE)
#waterinv<-waterinv[!is.na(waterinv$BASIN_CODE),]
#pwl<-unique(blake[c('LakeID','PWLID')])
#pwl$basin<-substring(pwl$LakeID,1,2)
#pwl$basin<-paste("_",pwl$basin,sep="")
#waterinv<-merge(waterinv,pwl,by=c('PWLID'),all.x = TRUE)
#waterinv<-unique(waterinv[c('PWLID','LakeID','basin')])
#names(waterinv)[names(waterinv)=="LakeID"]<-"LAKE_ID"
#rm(pwl)
#waterinv$WIPWL<-"needs verification"
#trendsimple<-merge(trendsimple,waterinv,by=c('LAKE_ID','basin'),all=TRUE)

#now order table
#trendsimple<-trendsimple %>%
#  select(LAKE_ID,basin,Waterbody_Classification,WATER,WIPWL,eut.wo.err,eutrophic,everything()) %>%
#  arrange(basin,WIPWL,desc(eut.wo.err),Waterbody_Classification,desc(anoxic),desc(`lower DO than WQS`))

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


