#script to rule it all
#alene Onion
# November 2018

############################################################################################################
#pull historic data, insert 2018 data, and clean up data file
source('L:/DOW/StreamDatabase/Lakes/data/2018.cleanup.R')
############################################################################################################

#set working directory
setwd("C:/Rscripts/LCIReports")

############################################################################################################
#simplifying and merging the tables
source('L:/DOW/StreamDatabase/Lakes/data/2018/Lakes.R')

#Fixing the data set
data$Result.Sample.Fraction[data$Result.Sample.Fraction==""]<-NA

#remove old data
data<-data[data$SAMPLE_DATE>'2000-01-01',]
#remove na
data<-data[!is.na(data$Characteristic.Name),]

#write backup
write.csv(data,file="sections/data/data.backup.all.csv",row.names=FALSE)



#restricting to 2018 only
data<-data[data$SAMPLE_DATE>'2018-01-01',]
backup<-data

#fix info types
#write function to capture last characters in a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
data$end<-substrRight(data$SAMPLE_NAME,2)
data$INFO_TYPE<-NA
data$INFO_TYPE<-ifelse(data$end=="DP","DP",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="SD","SD",data$INFO_TYPE)
#remove cslap and habs samples
data$end<-substr(data$SAMPLE_NAME,1,3)
data<-data[data$end!="18-",]
#remove samples that don't start with 18
data$end<-substr(data$SAMPLE_NAME,1,2)
data<-data[data$end=="18",]
#identify samples that end in an even number as BS and odd as OW 
data$end<-substrRight(data$SAMPLE_NAME,1)
data$INFO_TYPE<-ifelse(data$end=="1","OW",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="3","OW",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="5","OW",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="7","OW",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="9","OW",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="0","BS",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="2","BS",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="4","BS",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="6","BS",data$INFO_TYPE)
data$INFO_TYPE<-ifelse(data$end=="8","BS",data$INFO_TYPE)
data$end<-NULL

#write backup
write.csv(data,file="sections/data/data.backup.csv",row.names=FALSE)


















###########################################################################################################
#reviewing and ranking 2018 data
###########################################################################################################
#loading data
bresults<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Test.Results.csv", stringsAsFactors=FALSE)
bsample<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Sample.csv", stringsAsFactors=FALSE)
blake<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Lake.Master.csv", stringsAsFactors=FALSE)
blocation<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Location.csv", stringsAsFactors=FALSE)
bhabs<-read.csv("L:/DOW/StreamDatabase/Lakes/data/HABstatus.csv", stringsAsFactors=FALSE)

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

#now had historic data for needs verification and minor impacts waterbody inventory sites
#add needs verification list from waterbody inventory
waterinv<-read.csv("sections/data/Waterbody.Inventory.Input.csv", stringsAsFactors=FALSE)
waterinv<-waterinv[!is.na(waterinv$BASIN_CODE),]
pwl<-unique(blake[c('LakeID','PWLID')])
pwl$basin<-substring(pwl$LakeID,1,2)
pwl$basin<-paste("_",pwl$basin,sep="")
waterinv<-merge(waterinv,pwl,by=c('PWLID'),all.x = TRUE)
waterinv<-unique(waterinv[c('PWLID','LakeID')])
names(waterinv)[names(waterinv)=="LakeID"]<-"LAKE_ID"
rm(pwl)
waterinv$WIPWL<-"needs verification"
#read complete data set
data1<-read.csv("sections/data/data.backup.all.csv", stringsAsFactors=FALSE)
data1<-data1[!is.na(data1$Characteristic.Name),]
data1$SAMPLE_DATE<-as.Date(data1$SAMPLE_DATE,format="%Y-%m-%d")

library(dplyr)
data1<-merge(waterinv,data1,by=c('LAKE_ID','PWLID'),all.x = TRUE)
data1 <- data1 %>% 
  filter(Characteristic.Name %in% c("AMMONIA", "NITROGEN, NITRATE (AS N)","SODIUM","IRON","MANGANESE","MAGNESIUM","NITROGEN, NITRATE-NITRITE","PH","DEPTH, SECCHI DISK DEPTH","NITROGEN, KJELDAHL, TOTAL","SILICA","CHLORIDE (AS CL)","ALKALINITY, TOTAL (AS CACO3)","TOTAL ORGANIC CARBON","CHLOROPHYLL A","PHOSPHORUS","SULFATE (AS SO4)","TRUE COLOR","CALCIUM","SULFATE","CHLORIDE","SPECIFIC CONDUCTANCE","DEPTH, BOTTOM","SULFATE","DISSOLVED ORGANIC CARBON","NITROGEN","ARSENIC","NITRITE","TEMPERATURE, WATER","DEPTH","TEMPERATURE, AIR","DISSOLVED OXYGEN","CONDUCTIVITY"))
data1<-data1[!is.na(data1$Characteristic.Name),]

intensive<-merge(intensive,data1,all = TRUE)
rm(data1)

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

#class A waters################################################################################################################
source('sections/PWS.R')

#CLASS B WATERS################################################################################################################
source('sections/Recreation.R')

#aquatic life################################################################################################################
source('sections/AquaticLife.R')

#trophic state################################################################################################################
source('sections/trophic.R')

######################################################################################################################
#merge back to intensive
intensive<-merge(intensive,trend,all = TRUE)
rm(trend)
intensive<-intensive[!is.na(intensive$Characteristic.Name),]

#create simplified count table
#remove rejected data
#create simplified table
PWsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,basin,PIdrinking) %>%
  dplyr::group_by(LAKE_ID,WATER,basin) %>%
  dplyr::summarize(PIdrinking = sum(PIdrinking, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(PWsimple)
unique(PWsimple$PIdrinking)
#create simplified recreation table
RECsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,basin,PIrecreation) %>%
  dplyr::group_by(LAKE_ID,WATER,basin) %>%
  dplyr::summarize(PIrecreation = sum(PIrecreation, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(RECsimple)
unique(RECsimple$PIrecreation)
#create simplified aquatic life table
AQUsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,basin,PIaquatic) %>%
  dplyr::group_by(LAKE_ID,WATER,basin) %>%
  dplyr::summarize(PIaquatic = sum(PIaquatic, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(AQUsimple)
unique(AQUsimple$PIaquatic)

#create simplified trophic tables
#eutrophic
EUTsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,basin,eutrophic) %>%
  dplyr::group_by(LAKE_ID,WATER,basin) %>%
  dplyr::summarize(eutrophic = sum(eutrophic, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(EUTsimple)
unique(EUTsimple$eutrophic)
#mesotrophic
MESsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,basin,mesotrophic) %>%
  dplyr::group_by(LAKE_ID,WATER,basin) %>%
  dplyr::summarize(mesotrophic = sum(mesotrophic, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(MESsimple)
unique(MESsimple$mesotrophic)
#oligotrophic
OLIsimple<- intensive %>%
  dplyr::select(LAKE_ID,WATER,basin,oligotrophic) %>%
  dplyr::group_by(LAKE_ID,WATER,basin) %>%
  dplyr::summarize(oligotrophic = sum(oligotrophic, na.rm = TRUE)) %>%
  dplyr::ungroup()
head(OLIsimple)
unique(OLIsimple$oligotrophic)

trendsimple<-merge(PWsimple,RECsimple,by=c('basin','LAKE_ID','WATER'),all=TRUE)
trendsimple<-merge(trendsimple,AQUsimple,by=c('basin','LAKE_ID','WATER'),all=TRUE)
trendsimple<-merge(trendsimple,EUTsimple,by=c('basin','LAKE_ID','WATER'),all=TRUE)
trendsimple<-merge(trendsimple,MESsimple,by=c('basin','LAKE_ID','WATER'),all=TRUE)
trendsimple<-merge(trendsimple,OLIsimple,by=c('basin','LAKE_ID','WATER'),all=TRUE)
trendsimple<-trendsimple[order(trendsimple$PIdrinking,trendsimple$PIrecreation,trendsimple$PIaquatic,trendsimple$eutrophic,trendsimple$mesotrophic,trendsimple$oligotrophic,decreasing = TRUE),]
rm(list=c('EUTsimple','MESsimple','OLIsimple','PWsimple','RECsimple','AQUsimple'))

#add silver lake which we didn't get to but may be considered for sampling
silver<-data.frame(LAKE_ID="1702SIL1076",basin="17",WATER="Silver Lake Reservoir")
trendsimple<-merge(trendsimple,silver,by=c('LAKE_ID','basin','WATER'),all=TRUE)
rm(silver)

#identify PWS waters and Beaches
class<-unique(blake[c('LakeID','PWS','Beaches','Waterbody_Classification','ACRES','PWLID')])
names(class)[names(class)=="LakeID"]<-"LAKE_ID"
class$PWS<-ifelse(class$PWS=="No",NA,class$PWS)
PWS<-unique(class[c('LAKE_ID','PWS')])
PWS<-PWS[!is.na(PWS$PWS),]
PWS<-unique(PWS)
beach<-unique(class[c('LAKE_ID','Beaches')])
beach<-beach[!is.na(beach$Beaches),]
beach<-unique(beach)
acres<-unique(class[c('LAKE_ID','ACRES')])
acres<-acres[!is.na(acres$ACRES),]
acres<-unique(acres)
wqc<-unique(class[c('LAKE_ID','Waterbody_Classification')])
wqc<-wqc[!is.na(wqc$Waterbody_Classification),]
names(wqc)[names(wqc)=="Waterbody_Classification"]<-"Classification"
wqc<-unique(wqc)
pwl<-unique(class[c('LAKE_ID','PWLID')])
pwl<-pwl[!is.na(pwl$PWLID),]
pwl<-unique(pwl)
hypo<-unique(intensive[c('LAKE_ID','INFO_TYPE')])
hypo<-hypo[hypo$INFO_TYPE=="BS",]
hypo$hypo<-"yes"
hypo<-unique(hypo[c('LAKE_ID','hypo')])
trendsimple<-merge(trendsimple,PWS,all.x=TRUE)
trendsimple<-merge(trendsimple,beach,all.x = TRUE)
trendsimple<-merge(trendsimple,wqc,all.x = TRUE)
trendsimple<-merge(trendsimple,acres,all.x = TRUE)
trendsimple<-merge(trendsimple,pwl,all.x = TRUE)
trendsimple<-merge(trendsimple,hypo,all.x = TRUE)
rm(list=c('class','beach','PWS','acres','pwl','wqc','hypo'))

#pull the coordinates for each lake
local<-blocation[blocation$Type=="Centroid"|blocation$Type=="Deep Hole"|blocation$Type=="centroid",]
names(local)[names(local)=="LakeID"]<-"LAKE_ID"
local<-unique(local[c('LAKE_ID','Y_Coordinate','X_Coordinate')])
local<-local[!duplicated(local$LAKE_ID),]
trendsimple<-merge(trendsimple,local,by=c('LAKE_ID'),all.x = TRUE)
rm(local)

#pull habs samples
lakeids<-unique(trendsimple[c('LAKE_ID','basin')])
lakeids$lakes<-"yes"
lakeids<-unique(lakeids[c('LAKE_ID','lakes')])

intensivehabs<-merge(bhabs,bsample,by=c('SAMPLE_ID'),all.x=TRUE)
intensivehabs<-unique(intensivehabs[c('LAKE_ID','STATUS')])
intensivehabs<-merge(lakeids,intensivehabs,by=c('LAKE_ID'),all.x = TRUE)
rm(lakeids)
#remove No Blooms
intensivehabs<-intensivehabs[intensivehabs$STATUS!="No Bloom",]
intensivehabs<-intensivehabs[!is.na(intensivehabs$LAKE_ID),]
intensivehabs$bloom<-"yes"
intensivehabs<-unique(intensivehabs[c('LAKE_ID','bloom')])
#add to trendsimple
trendsimple<-merge(trendsimple,intensivehabs,by=c('LAKE_ID'),all.x = TRUE)
rm(intensivehabs)

#figuring out when last sampled
samples<-unique(bsample[c('LAKE_ID','SAMPLE_DATE','SAMPLE_ID')])
sampless<-unique(bresults[c('SAMPLE_ID','Characteristic.Name')])
samples<-merge(samples,sampless,by=c('SAMPLE_ID'))
rm(sampless)
samples$SAMPLE_DATE<- as.Date(samples$SAMPLE_DATE,format="%m/%d/%Y")
samples<-samples %>% 
  filter(Characteristic.Name %in% c("Depth, Secchi disk depth","Depth, Secchi Disk Depth","Disk, Secchi Disk Depth")) %>% 
  mutate(year = format(SAMPLE_DATE, "%Y")) %>% 
  select(LAKE_ID,year)%>% 
  group_by(LAKE_ID) %>% 
  summarize(year = max(year)) %>% 
  ungroup()
trendsimple<-merge(trendsimple,samples,by=c('LAKE_ID'),all.x = TRUE)
rm(samples)

trendsimple<-unique(trendsimple[c('basin','LAKE_ID','WATER','PWS','PIdrinking','PIrecreation','PIaquatic','bloom','eutrophic','mesotrophic','oligotrophic','year','PWLID','hypo','Classification','Beaches','ACRES','Y_Coordinate','X_Coordinate')])
#identify those in waterbody inventory needs verification
trendsimple<-merge(trendsimple,waterinv,by=c('PWLID','LAKE_ID'),all=TRUE)
rm(waterinv)

#remove those that are in cslap
trendsimplebackup<-trendsimple
cslap<-read.csv("sections/data/2018CSLAP.csv", stringsAsFactors=FALSE)
cslap$in2018<-"yes"
cslap<-unique(cslap[c('LAKE_ID','in2018')])
trendsimple<-merge(trendsimple,cslap,by=c('LAKE_ID'),all.x =TRUE)
rm(cslap)
trendsimple<-trendsimple[is.na(trendsimple$in2018),]
trendsimple$in2018<-NULL


#remove those that are less than 6.5 acres
trendsimple<-trendsimple[trendsimple$ACRES>6.5,]
#convert NA to 0 before ranking
trendsimple$PIdrinking[is.na(trendsimple$PIdrinking)] <- 0
trendsimple$PIaquatic[is.na(trendsimple$PIaquatic)] <- 0
trendsimple$PIrecreation[is.na(trendsimple$PIrecreation)] <- 0
#remove Kissena lake because it was sampled 5 years ago
trendsimple<-trendsimple[trendsimple$LAKE_ID!="1702KIS0076",]

trendsimple<-trendsimple[order(trendsimple$PWS,trendsimple$PIdrinking,trendsimple$PIrecreation,trendsimple$PIaquatic,trendsimple$bloom,trendsimple$eutrophic,trendsimple$mesotrophic,trendsimple$oligotrophic,trendsimple$Classification,decreasing = TRUE),]

trendsimple$basin<-substr(trendsimple$LAKE_ID,1,2)

#add costs
costs<-read.csv("sections/data/price.list.parameters.csv", stringsAsFactors=FALSE)
costs<-costs[costs$CHEMICAL_NAME=="cost",]
trendsimple$cost<-NA
trendsimple$cost<-ifelse(grepl("A",trendsimple$Classification) & trendsimple$hypo=="yes",4*(costs$class_a_epi[1]+costs$class_a_hyp[1]),trendsimple$cost)
trendsimple$cost<-ifelse(grepl("A",trendsimple$Classification) & is.na(trendsimple$hypo),4*(costs$class_a_unstratified),trendsimple$cost)
trendsimple$cost<-ifelse(!grepl("A",trendsimple$Classification) & trendsimple$hypo=="yes",4*(costs$class_b_epi[1]+costs$class_b_hyp[1]),trendsimple$cost)
trendsimple$cost<-ifelse(!grepl("A",trendsimple$Classification) & is.na(trendsimple$hypo),4*(costs$class_b_unstratified[1]),trendsimple$cost)
trendsimple<-trendsimple[!is.na(trendsimple$LAKE_ID),]
rm(costs)

#make basin _basin
trendsimple$basin<-paste("_",trendsimple$basin,sep="")


#write output
write.csv(trendsimple,file="2018.ranked.intensive.sites.csv",row.names=FALSE)
write.csv(intensive,file="2018.ranked.intensive.Data.csv",row.names=FALSE)


rmarkdown::render("intensive.site.selection.Rmd")

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