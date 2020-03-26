#script to rule it all
#alene Onion
# November 2018

############################################################################################################
#loading raw data tables 
blake<-read.csv("H:/Lakes.Database/data/current/Lake.Master.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
bresults<-read.csv("H:/Lakes.Database/data/current/Test.Results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
bsample<-read.csv("H:/Lakes.Database/data/current/Sample.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
bhabs<-read.csv("H:/Lakes.Database/data/current/HABstatus.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
############################################################################################################

#set working directory
setwd("C:/Rscripts/LCIReports")

library(dplyr)

screening<-read.csv("sections/data/Screening_Lakes_Final_Output.txt", stringsAsFactors=FALSE,colClasses = c("ReachCode"="character"))

screening<-unique(screening[c('BasNumber','ReachCode','LakeID','GNIS_Name','Water',
                              'WBCATGRY','MGMTSTAT','AnthroLoad','LkAreaLoad','LakeAcres','Beach','PWL_ID')])
names(screening)[names(screening)=="AnthroLoad"]<-"anthroLoad"
names(screening)[names(screening)=="LkAreaLoad"]<-"totalLoad"
names(screening)[names(screening)=="LakeAcres"]<-"ACRES"

#remove samples collected in past 10 yrs
# even if there's only one sample, we wouldn't screen it but would instead slate it for consideration for intensive sampling next year
resultss<-unique(bresults[c('SAMPLE_ID','Characteristic.Name')])
resultss<-resultss[resultss$Characteristic.Name=="Depth, Secchi disk depth"|resultss$Characteristic.Name=="Depth, Secchi Disk Depth"|resultss$Characteristic.Name=="Disk, Secchi Disk Depth",]
samples<-unique(bsample[c('SAMPLE_ID','LAKE_ID','SAMPLE_DATE')])
samples<-merge(resultss,samples,by=c('SAMPLE_ID'),all.x = TRUE)
rm(resultss)
samples<-unique(samples[c('LAKE_ID','SAMPLE_DATE')])
names(samples)[names(samples)=="LAKE_ID"]<-"LakeID"
names(samples)[names(samples)=="SAMPLE_DATE"]<-"sampled"
samples$sampled<-as.Date(samples$sampled,format="%m/%d/%Y")
samples<-samples %>% 
  mutate(sampled = format(sampled, "%Y")) 
samples<-samples[!is.na(samples$sampled),]
samples<- samples %>%
  dplyr::group_by(LakeID) %>%
  dplyr::summarize(sampled = max(sampled, na.rm = TRUE)) %>%
  dplyr::ungroup()
samples<-unique(samples)
#pull 2018 cslap data
cslap<-read.csv("sections/data/2018CSLAP.csv", stringsAsFactors=FALSE)
cslap$sampled<-2018
cslap<-unique(cslap[c('LAKE_ID','sampled')])
names(cslap)[names(cslap)=="LAKE_ID"]<-"LakeID"
samples<-merge(samples,cslap,by=c('LakeID','sampled'),all=TRUE)
rm(cslap)
screening<-merge(screening,samples,by=c('LakeID'),all.x = TRUE)
rm(samples)
screening$sampled<-ifelse(is.na(screening$sampled),0,screening$sampled)
screening<-screening[screening$sampled<2009,]
#remove assessed in waterbody inventory
screening<-screening[screening$WBCATGRY!="Impaired",]
screening<-screening[screening$WBCATGRY!="No Known Impact",]
screening<-screening[!is.na(screening$ReachCode),]

#pull PWS directly from the DOH intake layer
PWS<-read.csv("sections/data/FIN_waterbody_public_intakes.csv", stringsAsFactors=FALSE)
PWS$PWS1<-"yes"
PWS<-unique(PWS[c('FIN','PWS1')])
PWS<-merge(PWS,blake,by=c('FIN'),all.x = TRUE)
PWS<-unique(PWS[c('LAKE_ID','PWS1')])
PWS$PWS<-PWS$PWS1
PWS$PWS1<-NULL
names(PWS)[names(PWS)=="LAKE_ID"]<-"LakeID"
screening<-merge(screening,PWS,by=c('LakeID'),all.x = TRUE)

#pull lakes with habs blooms more than one sample 
lakeids<-unique(screening[c('LakeID')])
lakeids$lakes<-"yes"
lakeids<-unique(lakeids[c('LakeID','lakes')])

habscreening<-merge(bhabs,bsample,by=c('SAMPLE_ID'),all.x=TRUE)
names(habscreening)[names(habscreening)=="LAKE_ID"]<-"LakeID"
habscreening<-unique(habscreening[c('LakeID','STATUS','SAMPLE_DATE')])
habscreening$SAMPLE_DATE<-as.Date(habscreening$SAMPLE_DATE,format="%m/%d/%Y")
habscreening<-habscreening[!is.na(habscreening$SAMPLE_DATE),]

habscreening<-merge(lakeids,habscreening,by=c('LakeID'),all.x = TRUE)
rm(lakeids)
#remove No Blooms
habscreening<-habscreening[habscreening$STATUS!="No Bloom",]
habscreening<-habscreening[!is.na(habscreening$LakeID),]
habscreening$bloom<-1
habscreening<-unique(habscreening[c('LakeID','SAMPLE_DATE','bloom')])
habscreening<- habscreening %>%
  group_by(LakeID) %>%
  summarize(bloom = sum(bloom, na.rm = TRUE)) %>%
  ungroup()
habscreening<-habscreening[habscreening$bloom>=2,]
habscreening<-unique(habscreening[c('LakeID','bloom')])
habscreening$bloom<-"yes"
#add to trendsimple
screening<-merge(screening,habscreening,by=c('LakeID'),all.x = TRUE)

#order the list
screening<-unique(screening[c('LakeID','BasNumber','ReachCode','GNIS_Name','Water','PWS','bloom','anthroLoad','totalLoad','WBCATGRY','MGMTSTAT','ACRES','Beach','PWL_ID','sampled')])
screening<-screening[order(screening$PWS,screening$bloom,screening$anthroLoad,screening$totalLoad,decreasing = TRUE),]

#add classification
wqc<-unique(blake[c('LAKE_ID','Waterbody_Classification')])
wqc<-wqc[!is.na(wqc$Waterbody_Classification),]
names(wqc)[names(wqc)=="Waterbody_Classification"]<-"Classification"
wqc<-unique(wqc)
wqc$Classification[wqc$Classification==""]<-NA
wqc<-wqc[!is.na(wqc$Classification),]
screening<-merge(screening,wqc,by=c('LakeID'),all.x = TRUE)
rm(list=c('wqc'))


#add costs
costs<-read.csv("sections/data/price.list.parameters.csv", stringsAsFactors=FALSE)
costs<-costs[costs$CHEMICAL_NAME=="cost",]
screening$cost<-NA
screening$cost<-ifelse(grepl("A",screening$Classification),mean(c((costs$class_a_epi[1]+costs$class_a_hyp[1]),costs$class_a_unstratified)),screening$cost)
screening$cost<-ifelse(!grepl("A",screening$Classification),mean(c((costs$class_b_epi[1]+costs$class_b_hyp[1]),costs$class_b_unstratified)),screening$cost)

#rank the list
screening<-screening[order(screening$PWS,screening$bloom,screening$anthroLoad,screening$totalLoad,decreasing = TRUE),]


#write file
write.csv(screening,file="ranked.list.screening.DONT.OVERIDE.MATT.and.Jesses.file.csv",row.names=FALSE)
rm(list=c('PWS','habscreening','costs'))



#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

#creating a ranked list of pristine lakes for screening
data1<-read.csv("sections/data/data.backup.all.csv", stringsAsFactors=FALSE)
data1<-data1[!is.na(data1$Characteristic.Name),]
data1$SAMPLE_DATE<-as.Date(data1$SAMPLE_DATE,format="%Y-%m-%d")
library(dplyr)

#restricting to the past 20 yrs of data
data1<-data1[data1$SAMPLE_DATE>'1990-01-01',]

#fix characteristic names
data1$Characteristic.Name<-ifelse(data1$Characteristic.Name=="DISK, SECCHI DISK DEPTH","DEPTH, SECCHI DISK DEPTH",data1$Characteristic.Name)
data1$Characteristic.Name<-ifelse(data1$Characteristic.Name=="DISSOLVED OXYGEN","DISSOLVED OXYGEN (DO)",data1$Characteristic.Name)

#pull only data relevant to the water quality standards
intensive <- data1 %>% 
  filter(Characteristic.Name %in% c("UV 254","AMMONIA", "NITROGEN, NITRATE (AS N)","SODIUM","IRON","MANGANESE","MAGNESIUM","NITROGEN, NITRATE-NITRITE",
                                    "PH","DEPTH, SECCHI DISK DEPTH","NITROGEN, KJELDAHL, TOTAL","SILICA","CHLORIDE (AS CL)",
                                    "ALKALINITY, TOTAL (AS CACO3)","TOTAL ORGANIC CARBON","CHLOROPHYLL A","PHOSPHORUS","SULFATE (AS SO4)",
                                    "TRUE COLOR","CALCIUM","SULFATE","CHLORIDE","SPECIFIC CONDUCTANCE","DEPTH, BOTTOM","SULFATE",
                                    "DISSOLVED ORGANIC CARBON","NITROGEN","ARSENIC","NITRITE","TEMPERATURE, WATER","DEPTH","TEMPERATURE, AIR",
                                    "DISSOLVED OXYGEN (DO)","CONDUCTIVITY"))
intensive<-intensive[!is.na(intensive$Characteristic.Name),]

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

#add basin
intensive$basin<-substring(intensive$LAKE_ID,1,2)
#restrict to those in screening basins
intensive<-intensive %>% 
  filter(basin %in% c("04","09","14"))


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

#pull year sampled
CSL<-data1 %>% 
  select(LAKE_ID, WATER,Characteristic.Name,SAMPLE_DATE,DATA_PROVIDER,Open.Water,Developed.Open.Space,Developed.Low.Intensity,Developed.Medium.Intensity,Developed.High.Intensity,Barren.Land,Deciduous.Forest,Evergreen.Forest,Mixed.Forest,Scrub_Shrub,Grassland_Herbaceous,Pasture_Hay,Cultivated.Crops,Woody.Wetlands,Emergent.Herbaceous.Wetland,Lake..Wetlands,Agricultural,Forest.shrub.grasses,Residential,Urban) %>% 
  filter(Characteristic.Name=="DEPTH, SECCHI DISK DEPTH") %>% 
  mutate(year = format(SAMPLE_DATE, "%Y")) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(LAKE_ID,WATER,year,Open.Water,Developed.Open.Space,Developed.Low.Intensity,Developed.Medium.Intensity,Developed.High.Intensity,Barren.Land,Deciduous.Forest,Evergreen.Forest,Mixed.Forest,Scrub_Shrub,Grassland_Herbaceous,Pasture_Hay,Cultivated.Crops,Woody.Wetlands,Emergent.Herbaceous.Wetland,Lake..Wetlands,Agricultural,Forest.shrub.grasses,Residential,Urban) %>% 
  unique() 

#identify lakes sampled in 2018
cslap<-read.csv("sections/data/2018CSLAP.csv", stringsAsFactors=FALSE)
cslap$CSLAPin2018<-"yes"
cslap<-unique(cslap[c('LAKE_ID','CSLAPin2018')])
CSL<-merge(CSL,cslap,by=c('LAKE_ID'),all=TRUE)
rm(cslap)

#now count the number of years each lake was sampled
CSLsum<-CSL %>% 
  mutate(yrs = 1) %>% 
  group_by(LAKE_ID) %>% 
  summarize(yrs = sum(yrs)) %>% 
  ungroup()
#simplify CSL
CSL<-unique(CSL[c('LAKE_ID','WATER','CSLAPin2018')])
CSL<-merge(CSL,CSLsum,by=c('LAKE_ID'),all=TRUE)
CSL<-unique(CSL[c('LAKE_ID','WATER','yrs','CSLAPin2018')])
rm(CSLsum)

#merge back with trendsimple
trendsimple<-merge(trendsimple,CSL,by=c('LAKE_ID','WATER'),all.x = TRUE)

#write the file
write.csv(trendsimple,"Trend.ranking.DONT.OVERIDE.MATT.JESSE.WORK.csv",row.names=FALSE)
rm(list=c('CSL','data1','intensive','thresholds','trendsimple'))


#########################################################################################################################################################################
#########################################################################################################################################################################
#########################################################################################################################################################################
#########################################################################################################################################################################
#create list of screening reference sites
#read in ranked list of historic data and unassessed lakes
screening<-read.csv("ranked.list.screening.csv", stringsAsFactors=FALSE,colClasses = c("ReachCode"="character"))
trend<-read.csv("Trend.ranking.csv", stringsAsFactors=FALSE)

#adding reach code to trend sites
reachcode<-read.csv("sections/data/Screening_Lakes_Final_Output.txt",colClasses = c("ReachCode"="character"))
reachcode<-unique(reachcode[c('ReachCode','LakeID')])
names(reachcode)[names(reachcode)=="LakeID"]<-"LAKE_ID"
names(screening)[names(screening)=="LakeID"]<-"LAKE_ID"
trend<-merge(trend,reachcode,by=c('LAKE_ID'),all.x = TRUE)
rm(reachcode)

#remove %anthro and %natural landcover because we calculated that for all later
trend$anthro<-NULL
trend$natural<-NULL
screening$cost<-NULL

#read in landcover data
landcover<-read.csv("sections/data/Screening_Lakes_LandUse_Ecoregion.csv", stringsAsFactors=FALSE,colClasses = c("ReachCode"="character"))
#read in impervious surface coverage
impervious<-read.csv("sections/data/Screening_Lakes_impervious.csv", stringsAsFactors=FALSE,colClasses = c("ReachCode"="character"))
impervious<-unique(impervious[c('ReachCode','Mean_Imperviousness')])
landcover<-merge(landcover,impervious,by=c('ReachCode'),all = TRUE)
rm(impervious)

#remove columns so don't confuse merge
screening$Water<-NULL
trend$WATER<-NULL
screening$Beach<-NULL
trend$Beaches<-NULL
names(screening)[names(screening)=="BasNumber"]<-"basin"
screening$PWL_ID<-NULL
trend$PWLID<-NULL
screening$sampled<-NULL

#add missing reach codes
trend$ReachCode<-ifelse(trend$LAKE_ID=="1403BEA5572","02040102002925",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="1403UWB0368E","02040102002999",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="0906BON0024","04150303002332",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="0404NUN0084A","04130002001416",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="0403UWB5243","04130002004677",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="0902ALL5147","04150306001707",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="1401UWB5217","02040104007673",trend$ReachCode)
trend$ReachCode<-ifelse(trend$LAKE_ID=="0403UWB0124A","04130002004720",trend$ReachCode)

screening<-merge(screening,trend,by=c('ReachCode','LAKE_ID','basin','ACRES'),all = TRUE)
#fix headers
screening$Classification.x<-ifelse(is.na(screening$Classification.x),screening$Classification.y,screening$Classification.x)
screening$Classification.y<-NULL
names(screening)[names(screening)=="Classification.x"]<-"Classification"
screening$PWS.x<-ifelse(is.na(screening$PWS.x),screening$PWS.y,screening$PWS.x)
screening$PWS.y<-NULL
names(screening)[names(screening)=="PWS.x"]<-"PWS"
rm(trend)

#add landcover
screening<-merge(screening,landcover,by=c('ReachCode'),all.x = TRUE)
rm(landcover)

#condense to natural and anthro landcover
screening$anthro<-screening$Developed.Open.Space + screening$Developed.Low.Intensity + screening$Developed.Medium.Intensity + screening$Developed.High.Intensity + screening$Pasture.Hay + screening$Cultivated.Crops
screening$other<-screening$Barren.Land + screening$Shrub.Scrub + screening$Grassland.Herbaceous
screening$forest_wetland<-screening$Deciduous.Forest + screening$Evergreen.Forest + screening$Mixed.Forest + screening$Woody.Wetlands + screening$Emergent.Herbaceous.Wetlands
##########
#NOTE
##########
#we need to figure out something to remove the error caused by lake surface area
# a watershed with no development will get a lower score if we don't include lake area as natural
# a watershed with 30% development will get ahigher score if we DO include lake area as natural
# so we need to exclude lake area from the % development calculation
#########################################################################################################
#########################################################################################################

screening<-unique(screening[c('ReachCode','LAKE_ID','basin','ACRES','GNIS_Name','PWS','bloom','anthroLoad','totalLoad','anthro','other','forest_wetland','water','Mean_Imperviousness','WBCATGRY','Classification','PIdrinking','PIrecreation','PIaquatic','eutrophic','mesotrophic','oligotrophic','hypo','yrs','CSLAPin2018','Total','US_L4CODE','US_L4NAME','US_L3CODE','US_L3NAME','NA_L3CODE','NA_L3NAME','NA_L2CODE','NA_L2NAME')])

#remove the one lake without a reach code
screening<-screening[!is.na(screening$ReachCode),]

#########################################################################################################################################################################
#pulling out reference, long term trend, and dept interest sites

#add costs
lake<-read.csv("C:/Rscripts/DatabaseModernization/Database.proposal/lakes/Lake.Master.csv", stringsAsFactors=FALSE)
lake<-unique(lake[c('LakeID','Waterbody_Classification')])
names(lake)[names(lake)=="LakeID"]<-"LAKE_ID"
names(lake)[names(lake)=="Waterbody_Classification"]<-"Classification"
screening$Classification<-NULL
screening<-merge(screening,lake,by=c('LAKE_ID'),all.x = TRUE)
rm(lake)
costs<-read.csv("sections/data/price.list.parameters.csv", stringsAsFactors=FALSE)
costs<-costs[costs$CHEMICAL_NAME=="cost",]
screening$cost<-NA
screening$cost<-ifelse(grepl("A",screening$Classification),mean(c((costs$class_a_epi[1]+costs$class_a_hyp[1]),costs$class_a_unstratified)),screening$cost)
screening$cost<-ifelse(!grepl("A",screening$Classification),mean(c((costs$class_b_epi[1]+costs$class_b_hyp[1]),costs$class_b_unstratified)),screening$cost)
rm(costs)

################################################
#reference locations
screening$forest_wetland95<-ifelse(screening$forest_wetland>95,"1","0")
screening$forest_wetland75<-ifelse(screening$forest_wetland>75,"1","0")

#number of ecoregions per basin
#95% forest_wetland
screening %>% 
  filter(forest_wetland95 %in% c("1")) %>% 
  filter(basin %in% c("14","9")) %>% 
  mutate(lakes=1) %>% 
  group_by(basin,US_L3CODE) %>%
  summarize(lakes=sum(lakes)) %>% 
  ungroup
#75% forest_wetland
screening %>% 
  filter(forest_wetland75 %in% c("1")) %>% 
  filter(Mean_Imperviousness<2) %>% 
  filter(basin==4) %>% 
  mutate(lakes=1) %>% 
  group_by(basin,US_L3CODE) %>%
  summarize(lakes=sum(lakes)) %>% 
  ungroup

#creating reference list
#95% forest_wetland
reference95<- screening %>% 
  filter(forest_wetland95 %in% c("1")) %>% 
  filter(basin %in% c("14","9"))  
#75% forest_wetland
reference75 <- screening %>% 
  filter(forest_wetland75 %in% c("1")) %>% 
  filter(Mean_Imperviousness<2) %>% 
  filter(basin==4) 
reference<-merge(reference95,reference75,all=TRUE)
rm(list=c('reference95','reference75'))

names(reference)[names(reference)=="yrs"]<-"yrs_sampled"
reference<-reference %>% 
  arrange(desc(ACRES))
reference$selected_ref<-NA
reference$selected_ref<-ifelse((reference$ReachCode=="04130003001277"|reference$ReachCode=="04130003004086"|
                                  reference$ReachCode=="04150305001308"|reference$ReachCode=="04150305004968"|reference$ReachCode=="04150304003156"|
                                  reference$ReachCode=="02040104002228"|reference$ReachCode=="02040104002098"),"x",reference$selected_ref)
reference<-unique(reference[c('selected_ref','CSLAPin2018','ReachCode','LAKE_ID','basin','ACRES','forest_wetland','yrs_sampled','US_L3CODE','US_L4CODE','eutrophic','mesotrophic','oligotrophic','cost')])

write.table(reference,file="reference.site.list.txt",sep=",",row.names=FALSE)

#####################################
#Long term trend sites
data1<-read.csv("sections/data/data.backup.all.csv", stringsAsFactors=FALSE)
data1<-data1[!is.na(data1$Characteristic.Name),]
data1$SAMPLE_DATE<-as.Date(data1$SAMPLE_DATE,format="%Y-%m-%d")
data1<-data1 %>% 
  mutate(yr = format(SAMPLE_DATE, "%Y")) 
data1<-unique(data1[c('LAKE_ID','yr')])
data1<-data1 %>% 
  mutate(yrs =1) %>% 
  group_by(LAKE_ID) %>% 
  summarize(yrs=sum(yrs)) %>% 
  ungroup()
library(dplyr)
screening$yrs<-NULL
screening<-merge(screening,data1,by=c('LAKE_ID'),all.x = TRUE)
rm(data1)

longterm<-screening[screening$yrs>5,]
longterm<-longterm[!is.na(longterm$ReachCode),] 
longterm$selected_LT<-NA
longterm$selected_LT<-ifelse(longterm$ReachCode=="04130003001262"|longterm$ReachCode=="04130003001267"|longterm$ReachCode=="04130002001356"|longterm$ReachCode=="04130002005274"|longterm$ReachCode=="02040104002114"|longterm$ReachCode=="02040101001304"|longterm$ReachCode=="02040101001343"|longterm$ReachCode=="02040104002183"|longterm$ReachCode=="02040104002195"|longterm$ReachCode=="04150303001947"|longterm$ReachCode=="04150303002332"|longterm$ReachCode=="04150303000595"|longterm$ReachCode=="04150303000596"|longterm$ReachCode=="04150304000849"|longterm$ReachCode=="04150306001198","x",longterm$selected_LT)
names(longterm)[names(longterm)=="yrs"]<-"yrs_sampled"
longterm<-unique(longterm[c('selected_LT','CSLAPin2018','LAKE_ID','ReachCode','basin','US_L3CODE','yrs_sampled','ACRES','PWS','Classification','bloom','forest_wetland','Mean_Imperviousness','eutrophic','mesotrophic','oligotrophic','cost')])
longterm<-longterm %>% 
  arrange(basin,US_L3CODE,desc(ACRES))
write.table(longterm,file="longterm.trend.txt",sep=",",row.names=FALSE)

#########################
#Identifying TMDL lakes
tmdl<-screening[screening$ReachCode=="04150303000570"|screening$ReachCode=="04150303000572",]
tmdl$selected_tmdl<-"x"
tmdl<-unique(tmdl[c('selected_tmdl','LAKE_ID','ReachCode','basin','US_L3CODE','ACRES','PWS','Classification','forest_wetland','Mean_Imperviousness','cost')])


#######################
#identifying dept interest sites

deptinterest<-read.csv("ranked.list.screening.csv", stringsAsFactors=FALSE,colClasses = c("ReachCode"="character"))
names(deptinterest)[names(deptinterest)=="LakeID"]<-"LAKE_ID"
names(deptinterest)[names(deptinterest)=="BasNumber"]<-"basin"
deptinterest$MGMTSTAT<-NULL
deptinterest$PWL_ID<-NULL
deptinterest$sampled<-NULL
deptinterest$selected_dept<-NA
deptinterest<-deptinterest %>% 
  arrange(desc(PWS),desc(bloom),desc(anthroLoad),desc(totalLoad)) %>% 
  mutate(anthroLoad = format(anthroLoad,scientific=F))
deptinterest[1:64,]$selected_dept<-"x"
deptinterest<-deptinterest[,c('selected_dept','LAKE_ID','basin','ReachCode','GNIS_Name','Water','PWS','bloom','anthroLoad','totalLoad','WBCATGRY','ACRES','Beach','Classification','cost')]

###########################
#totals
ref<-reference[reference$selected_ref=="x",]
ref<-unique(ref[c('selected_ref','CSLAPin2018','ReachCode','LAKE_ID','basin','cost')])
LT<-longterm[longterm$selected_LT=="x",]
LT<-unique(LT[c('selected_LT','CSLAPin2018','ReachCode','LAKE_ID','basin','cost')])
tmdls<-tmdl[tmdl$selected_tmdl=="x",]
tmdls<-unique(tmdls[c('selected_tmdl','ReachCode','LAKE_ID','basin','cost')])
tmdls$CSLAPin2018<-NA
dept<-deptinterest[deptinterest$selected_dept=="x",]
dept<-unique(dept[c('selected_dept','ReachCode','LAKE_ID','basin','cost')])
dept$CSLAPin2018<-NA
totals<-merge(ref,LT,by=c('CSLAPin2018','ReachCode','LAKE_ID','basin','cost'),all=TRUE)
totals<-merge(totals,tmdls,by=c('CSLAPin2018','ReachCode','LAKE_ID','basin','cost'),all=TRUE)
totals<-merge(totals,dept,by=c('CSLAPin2018','ReachCode','LAKE_ID','basin','cost'),all=TRUE)
rm(list=c('ref','LT','tmdls','dept'))

totals$cost<-ifelse(!is.na(totals$CSLAPin2018),0,totals$cost)

#write all the files in one workbook
library("xlsx")
write.xlsx(totals, file="LCI.screening.sites.xlsx", sheetName="Screening.List", row.names=FALSE,append=FALSE)
write.xlsx(reference, file="LCI.screening.sites.xlsx", sheetName="Reference", row.names=FALSE,append=TRUE)
write.xlsx(longterm, file="LCI.screening.sites.xlsx", sheetName="Long.Term", row.names=FALSE,append=TRUE)
write.xlsx(tmdl, file="LCI.screening.sites.xlsx", sheetName="TMDL", row.names=FALSE,append=TRUE)
write.xlsx(deptinterest, file="LCI.screening.sites.xlsx", sheetName="dept.interest",row.names=FALSE,append=TRUE)

rmarkdown::render("LCI.Screening.Sites.rmd")
