#and to return to these copies for rinse and repeat:
habs<-bhabs
land<-bland
lake<-blake
location<-blocation
profiles<-bprofiles
results<-bresults
sample<-bsample
rm(list = c('data','thresholds','results2018','DP','trend','junk','info','notresults','trend','thresholds',
            'trophic','p','profile','profiles','temp','i','lakes','nlakes','nparams','params','display','trendlakes','DO',
            'errors','trendsimple','j','lakes','nlakes','p','profile'))

#creating pivot table of samples present per lake/sample date
junk<-data
junk<-unique(junk[c('LAKE_ID','SAMPLE_DATE','INFO_TYPE','DATA_PROVIDER')])
junk<-junk[!is.na(junk$LAKE_ID),]
junk<-junk[junk$DATA_PROVIDER=="LCI",]
junk<-junk[!is.na(junk$LAKE_ID),]
junk$presence<-1
junk$INFO_TYPE<-factor(junk$INFO_TYPE,levels=c('DP','OW','BS','SB','SD'))
library(tidyr)
junk<-junk %>%
  spread(INFO_TYPE,presence,fill=0)
write.csv(junk,file="jesse.simpler.csv",row.names=FALSE)


lakes<-lakes[lakes$LAKE_ID=="1302CAS0059B",]

#checking if sample ids match in profiles and sample table
profiles<-bprofiles
sample<-bsample
sample<-unique(sample[c("SAMPLE_ID","SAMPLE_NAME","INFO_TYPE")])
sample<-sample[sample$INFO_TYPE=="DP",]
sample$SAMPLE_NAME<-gsub('_..$','',sample$SAMPLE_NAME)
names(sample)[names(sample)=="SAMPLE_ID"]<-"SAMPLE_SAMPLE_ID"
sample<-sample[!is.na(sample$SAMPLE_SAMPLE_ID),]

profiles<-unique(profiles[c("Sample_ID","DP_Key","Sample.4..SAMPLE_DATE")])
profiles$SAMPLE_NAME<-gsub('_DP$','',profiles$DP_Key)
names(profiles)[names(profiles)=="Sample_ID"]<-"PROFILES_SAMPLE_ID"
names(profiles)[names(profiles)=="Sample.4..SAMPLE_DATE"]<-"PROFILES_SAMPLE_DATE"

combined<-merge(profiles,sample,by=c('SAMPLE_NAME'),all.x = TRUE)
combined<-combined[!is.na(combined$PROFILES_SAMPLE_ID),]
combined$match<-ifelse(combined$PROFILES_SAMPLE_ID==combined$SAMPLE_SAMPLE_ID,"match","no")
combined<-combined[combined$match=="no",]
combined<-combined[!is.na(combined$SAMPLE_NAME),]
write.csv(combined,file="mismatched.sample.ids.csv",row.names = FALSE)
tail(combined)

#checking if sample ids match in secchi and sample table
results<-bresults
sample<-bsample
sample<-unique(sample[c("SAMPLE_ID","SAMPLE_NAME","INFO_TYPE")])
sample<-sample[sample$INFO_TYPE=="SD",]
sample$SAMPLE_NAME<-gsub('_..$','',sample$SAMPLE_NAME)
names(sample)[names(sample)=="SAMPLE_ID"]<-"SAMPLE_SAMPLE_ID"
sample<-sample[!is.na(sample$SAMPLE_SAMPLE_ID),]

results<-unique(results[c("SAMPLE_ID","Sample.5..SAMPLE_DATE","SAMPLE_NAME")])
results<-results[grep("_SD$",results$SAMPLE_NAME),]
names(results)[names(results)=="SAMPLE_NAME"]<-"SD_SAMPLE_NAME"
results$SAMPLE_NAME<-gsub('_SD$','',results$SD_SAMPLE_NAME)
names(results)[names(results)=="Sample.5..SAMPLE_DATE"]<-"SD_SAMPLE_DATE"
names(results)[names(results)=="SAMPLE_ID"]<-"SD_SAMPLE_ID"

combined<-merge(results,sample,by=c('SAMPLE_NAME'),all.x = TRUE)
combined<-combined[!is.na(combined$SD_SAMPLE_ID),]
combined$match<-ifelse(combined$SD_SAMPLE_ID==combined$SAMPLE_SAMPLE_ID,"match","no")
combined<-combined[combined$match=="no",]
combined<-combined[!is.na(combined$SAMPLE_NAME),]
write.csv(combined,file="mismatched.SD.ids.csv",row.names = FALSE)
tail(combined)

#figuring out which files in the thumb drive we don't have
results2018<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/LCI.2018.raw.concatinated.old.csv", stringsAsFactors=FALSE)
thumb<-read.csv("L:/DOW/SMAS/LabData/ChemData/2018/ALS thumb drives/LMAS/LCI/thumbs.csv", stringsAsFactors=FALSE)
results2018<-unique(results2018[c('sample_delivery_group','lab_name_code')])
names(thumb)[names(thumb)=="thumbs"]<-"sample_delivery_group"
thumb$thumb<-"X"
results2018$lab_name_code<-NULL
results2018$have<-"X"
together<-merge(thumb,results2018,by=c('sample_delivery_group'),all=TRUE)
together<-together[is.na(together$have),]
#these files need to be separated into test, batch, sample text files before tranferring
#R1805953
#R1808598
#R1808882
#R1809020
library(rmarkdown)
library(knitr)
library(kableExtra)
library(xlsx)
revisions<-read.xlsx("markdown/inputs/PEERS.xlsx" , 2, header=TRUE)
participants<-read.xlsx("markdown/inputs/PEERS.xlsx", 3, header=TRUE)
descriptions<-read.xlsx("markdown/inputs/PEERS.xlsx", 4, header=TRUE)
parameters<-read.xlsx("markdown/inputs/PEERS.xlsx", 5, header=TRUE)
#now pull the unique values to add to old data table
results2018<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/LCI.raw.concatenated.csv", stringsAsFactors=FALSE)
results2018<-merge(together,results2018,by=c('sample_delivery_group'),all.x = TRUE)
results2018$thumb<-NULL
results2018$have<-NULL
results2018<-results2018[!is.na(results2018$sys_loc_code),]
write.csv(results2018,"new.raw.data.csv",row.names=FALSE)

#for stephanie
results<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Test.Results.csv", stringsAsFactors=FALSE)
results<-unique(results[c('SAMPLE_NAME','Characteristic.Name','LAB_NAME_CODE')])
steph<-read.csv("C:/Rscripts/LCIReports/stephanie.csv")
steph<-unique(steph[c('SAMPLE_NAME','Characteristic.Name','labnamecode')])
names(steph)[names(steph)=="labnamecode"]<-"LAB_NAME_CODE"
#names(steph)[names(steph)=="Lab.Sample.Name"]<-"Lab_Sample_Name"
steph$steph<-"X"
results$results<-"X"
excluded<-merge(steph,results,by=c('SAMPLE_NAME','Characteristic.Name','LAB_NAME_CODE'),all.x = TRUE)
excluded<-excluded[is.na(excluded$results),]
write.csv(excluded,file="excluded.for.stephanie.csv",row.names=FALSE)

#checking if sites are in sample table 
newdata<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/new.raw.data.csv",stringsAsFactors=FALSE)
newdata<-unique(newdata[c('sys_sample_code','lab_name_code')])
newdata$sample<-substring(newdata$sys_sample_code,1,5)
newdata$DATA_PROVIDER<-NA
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18BLK","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18SRB","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18LIS","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18CMG","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18LHB","LCI",newdata$DATA_PROVIDER)
newdata$DATA_PROVIDER<-ifelse(newdata$sample=="18LCB","LCI",newdata$DATA_PROVIDER)
newdata<-newdata[newdata$DATA_PROVIDER=="LCI",]
newdata<-newdata[!is.na(newdata$sys_sample_code),]
newdata$sys_sample_code<-gsub('MSD$','',newdata$sys_sample_code)
newdata$sys_sample_code<-gsub('MS$','',newdata$sys_sample_code)
newdata$sys_sample_code<-gsub('18......DUP','junk',newdata$sys_sample_code)
newdata<-newdata[newdata$sys_sample_code!="junk",]
names(newdata)[names(newdata)=="sys_sample_code"]<-"SAMPLE_NAME"
newdata$newdata<-"X"
newdata$sample<-NULL
newdata$DATA_PROVIDER<-NULL
newdata$lab_name_code<-NULL
head(newdata)
sample<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Sample.csv", stringsAsFactors=FALSE)
sample<-unique(sample[c('SAMPLE_NAME','SAMPLE_ID')])
head(sample)
sample<-merge(newdata,sample,by=c('SAMPLE_NAME'),all.x = TRUE)
head(sample)
sample<-sample[is.na(sample$SAMPLE_ID),]
sample<-unique(sample[c('SAMPLE_NAME','SAMPLE_ID')])
sample
length(sample$SAMPLE_NAME)
newdata<-read.csv("L:/DOW/StreamDatabase/Lakes/data/2018/LCI.2018/output/new.raw.data.csv",stringsAsFactors=FALSE)
newdata<-unique(newdata[c('sys_sample_code','sample_date')])
names(newdata)[names(newdata)=="sys_sample_code"]<-"SAMPLE_NAME"
sample<-merge(sample,newdata,by=c('SAMPLE_NAME'),all.x = TRUE)
sample
length(sample$SAMPLE_NAME)

#pulling syssamplecode duplicates
junk$sample<-substring(junk$sys_sample_code,1,5)
junk$DATA_PROVIDER<-NA
junk$DATA_PROVIDER<-ifelse(junk$sample=="18BLK","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18SRB","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18LIS","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18CMG","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18LHB","LCI",junk$DATA_PROVIDER)
junk$DATA_PROVIDER<-ifelse(junk$sample=="18LCB","LCI",junk$DATA_PROVIDER)
junk<-junk[junk$DATA_PROVIDER=="LCI",]
junk<-junk[!is.na(junk$sys_sample_code),]
length(junk$sys_sample_code)
junk2<-unique(junk$sys_sample_code)
length(junk2)
write.csv(junk,file="junk.csv",row.names = FALSE)
#18LIS053	R1807212	7/31/2018 13:45	18LIS	LCI
#18LHB299	R1807670	8/13/2018 13:42	18LHB	LCI


#creating histogram of waterbodies not in the waterbody inventory
rmarkdown::render("Analysis.not.in.waterbody.inventory.rmd")








#creating a ranked list of cslap lakes for stephanie
data$DATA_PROVIDER<-toupper(data$DATA_PROVIDER)
data<-data[!is.na(data$Characteristic.Name),]
data$SAMPLE_DATE<-as.Date(data$SAMPLE_DATE,format="%Y-%m-%d")
library(dplyr)
#pull CSL data and year sampled
CSL<-data %>% 
      select(LAKE_ID, WATER,Characteristic.Name,SAMPLE_DATE,DATA_PROVIDER,Open.Water,Developed.Open.Space,Developed.Low.Intensity,Developed.Medium.Intensity,Developed.High.Intensity,Barren.Land,Deciduous.Forest,Evergreen.Forest,Mixed.Forest,Scrub_Shrub,Grassland_Herbaceous,Pasture_Hay,Cultivated.Crops,Woody.Wetlands,Emergent.Herbaceous.Wetland,Lake..Wetlands,Agricultural,Forest.shrub.grasses,Residential,Urban) %>% 
      filter(DATA_PROVIDER=="CSL",Characteristic.Name == "DEPTH, SECCHI DISK DEPTH") %>% 
      mutate(year = format(SAMPLE_DATE, "%Y")) %>% 
      mutate(year = as.numeric(year)) %>% 
      select(LAKE_ID,WATER,year,Open.Water,Developed.Open.Space,Developed.Low.Intensity,Developed.Medium.Intensity,Developed.High.Intensity,Barren.Land,Deciduous.Forest,Evergreen.Forest,Mixed.Forest,Scrub_Shrub,Grassland_Herbaceous,Pasture_Hay,Cultivated.Crops,Woody.Wetlands,Emergent.Herbaceous.Wetland,Lake..Wetlands,Agricultural,Forest.shrub.grasses,Residential,Urban) %>% 
      unique() 

#identify lakes sampled in 2018
cslap<-read.csv("sections/data/2018CSLAP.csv", stringsAsFactors=FALSE)
cslap$in2018<-"yes"
cslap<-unique(cslap[c('LAKE_ID','in2018')])
CSL<-merge(CSL,cslap,by=c('LAKE_ID'),all=TRUE)
rm(cslap)

#now count the number of years each CSLAP lake was sampled
CSLsum<-CSL %>% 
      mutate(yrs = 1) %>% 
      group_by(LAKE_ID) %>% 
      summarize(yrs = sum(yrs)) %>% 
      ungroup()
#simplify CSL
CSL<-unique(CSL[c('LAKE_ID','WATER','in2018','Open.Water','Developed.Open.Space','Developed.Low.Intensity','Developed.Medium.Intensity','Developed.High.Intensity','Barren.Land','Deciduous.Forest','Evergreen.Forest','Mixed.Forest','Scrub_Shrub','Grassland_Herbaceous','Pasture_Hay','Cultivated.Crops','Woody.Wetlands','Emergent.Herbaceous.Wetland','Lake..Wetlands','Agricultural','Forest.shrub.grasses','Residential','Urban')])
CSL<-merge(CSL,CSLsum,by=c('LAKE_ID'),all=TRUE)
CSL<-unique(CSL[c('LAKE_ID','WATER','yrs','in2018','Open.Water','Developed.Open.Space','Developed.Low.Intensity','Developed.Medium.Intensity','Developed.High.Intensity','Barren.Land','Deciduous.Forest','Evergreen.Forest','Mixed.Forest','Scrub_Shrub','Grassland_Herbaceous','Pasture_Hay','Cultivated.Crops','Woody.Wetlands','Emergent.Herbaceous.Wetland','Lake..Wetlands','Agricultural','Forest.shrub.grasses','Residential','Urban')])
rm(CSLsum)

#sum anthropogenic impacts v natural
CSL$anthro<-CSL$Developed.Open.Space + CSL$Developed.Low.Intensity + CSL$Developed.Medium.Intensity + CSL$Developed.High.Intensity + CSL$Pasture_Hay + CSL$Cultivated.Crops
CSL$natural<-CSL$Open.Water + CSL$Barren.Land + CSL$Deciduous.Forest + CSL$Evergreen.Forest + CSL$Mixed.Forest + CSL$Scrub_Shrub + CSL$Grassland_Herbaceous + CSL$Woody.Wetlands + CSL$Emergent.Herbaceous.Wetland
CSL<-unique(CSL[c('LAKE_ID','WATER','yrs','in2018','anthro','natural','Open.Water','Developed.Open.Space','Developed.Low.Intensity','Developed.Medium.Intensity','Developed.High.Intensity','Barren.Land','Deciduous.Forest','Evergreen.Forest','Mixed.Forest','Scrub_Shrub','Grassland_Herbaceous','Pasture_Hay','Cultivated.Crops','Woody.Wetlands','Emergent.Herbaceous.Wetland','Lake..Wetlands','Agricultural','Forest.shrub.grasses','Residential','Urban')])

#make NA values 0
CSL[is.na(CSL)]<-0

#rank the list
CSL<-CSL[order(CSL$anthro,CSL$yrs,decreasing = TRUE),]


#write the file
write.csv(CSL,"RMN.ranking.csv",row.names=FALSE)
rm(CSL)





#pulling dates for jesse
junk<-results %>% 
  filter(SAMPLE_NAME %in% c("18CMG996","18CMG997","18LHB098","18LHB099","18LHB298","18LIS098","18LIS099","18LCB097","18LCB198"))


#pull SBU sites since 2010 to create unassessed layer for WAVE
field<-read.csv("C:/Users/amonion/Downloads/Streams_ Field.csv", stringsAsFactors=FALSE)
field<-unique(field[c('BAS_LOC_RM','COLL_DATE')])
sites<-read.csv("C:/Users/amonion/Downloads/Streams_ Biomonitoring Sites.csv", stringsAsFactors=FALSE)
sites<-unique(sites[c('BAS_LOC_RM','LATITUDE','LONGITUDE')])
field<-merge(field,sites,by=c('BAS_LOC_RM'),all.x=TRUE)
rm(sites)
field$COLL_DATE<-as.Date(field$COLL_DATE,format="%m/%d/%Y")
field<-field[field$COLL_DATE>"2010-01-01",]
field<-unique(field[c('BAS_LOC_RM','LATITUDE','LONGITUDE')])
write.csv(field,"C:/Users/amonion/Downloads/WAVE.csv",row.names = FALSE)
rm(field)

junk<-data[data$PWLID=="1501-0021",]
junk<-junk[!is.na(junk$Characteristic.Name),]
junk<-unique(junk[c('LAKE_ID','SAMPLE_ID','SAMPLE_NAME','LOCATION_ID','WATER','Waterbody_Classification','PWLID','County','Y_Coordinate','X_Coordinate','SAMPLE_DATE','TIME','START_DEPTH','END_DEPTH','Depth','Characteristic.Name','Result.Value','Result.Unit','Result.Sample.Fraction')])
junk$Characteristic.Name<-paste(junk$Characteristic.Name,junk$Result.Unit,junk$Result.Sample.Fraction,sep="_")
junk$Result.Sample.Fraction<-NULL
junk$Result.Unit<-NULL
junk<-unique(junk)

library(tidyr)
junk2 <- junk %>% 
  spread(Characteristic.Name, Result.Value)

#pulling intensive samples from last year
junk<-data
junk$basin<-substring(junk$LAKE_ID,1,2)
junk<-junk[junk$basin=="05"|
             junk$basin=="08"|
             junk$basin=="13",]
junk<-junk[junk$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",]
unique(junk$LAKE_ID)




rm(list=c('data1','intensive','PWS','thresholds','trend','c_y3','c_y4','i','nsamples','r_x1','r_x2','samples','WQS','WQST','temp','temp1','AQ','AQU','ammonia'))
blocation<-read.csv("L:/DOW/StreamDatabase/Lakes/data/Location.csv", stringsAsFactors=FALSE)
junk<-blocation[c('LakeID','LocationID','Y_Coordinate','X_Coordinate')]
names(junk)[names(junk)=="LakeID"]<-"LAKE_ID"
trendsimple<-merge(trendsimple,junk,by=c('LAKE_ID'),all.x = TRUE)
trendsimple<-unique(trendsimple[c('LAKE_ID','LocationID','WATER','basin','Y_Coordinate','X_Coordinate')])
trendsimple<-trendsimple %>% 
  mutate(coordinates = paste(X_Coordinate,Y_Coordinate,sep=",")) %>% 
  group_by(LAKE_ID,WATER,basin) %>% 
  mutate(coordinates=first(coordinates)) %>% 
  ungroup()
trendsimple<-unique(trendsimple[c('LAKE_ID','WATER','coordinates','basin')])


#pulling how many sites were screened in 2018
data$basin<-substring(data$LAKE_ID,1,2)
data<-data[data$basin=="17"|
                       data$basin=="06"|
                       data$basin=="10"|
                       data$LAKE_ID=="0902BAR0262"|
                       data$LAKE_ID=="1301MORXXX1"|
                       data$LAKE_ID=="1301THE1027"|
                       data$LAKE_ID=="1301THE1034"|
                       data$LAKE_ID=="1301UWB1031",]


#looking at the normal distribution of the data
#those with 3yrs with non-normal distributions (47; max result =0.4346)
junk<- sandboxall[sandboxall$normal<0.05,]
#those with 3 yrs with normal distributions (33; max result = 0.2077)
junk2<-sandboxall[sandboxall$normal>0.05,]
#those with 1 yr and non-normal distributions (45; max result = 0.88)
junk3<- sandbox[sandbox$normal<0.05,]
#those with 1 yr and normal distributions (170; max result = 0.2345)
junk4<-sandbox[sandbox$normal>0.05,]
#reorder lakes by max value
junk %>% 
  group_by(junk$)

junk2<-junk2[order(junk2$Result.Value),]
junk3<-junk3[order(junk3$Result.Value),]
junk4<-junk4[order(junk4$Result.Value),]
ggplot(junk,aes(x=LAKE_ID,y=Result.Value))+
  geom_boxplot()+
  ylim(0,.2)


junk<-bsample[bsample$SAMPLE_ID=="2"|bsample$SAMPLE_ID=="7"|bsample$SAMPLE_ID=="12"|bsample$SAMPLE_ID=="13"|bsample$SAMPLE_ID=="22"|bsample$SAMPLE_ID=="38"|bsample$SAMPLE_ID=="1477"|bsample$SAMPLE_ID=="1792"|bsample$SAMPLE_ID=="2123"|bsample$SAMPLE_ID=="2264"|bsample$SAMPLE_ID=="41401"|bsample$SAMPLE_ID=="51174"|bsample$SAMPLE_ID=="51175"|bsample$SAMPLE_ID=="56264"|bsample$SAMPLE_ID=="61479",]
junk<-unique(junk[c('SAMPLE_ID','SAMPLE_DATE')])
51175            06/25/2014


junk %>% 
  mutate(codes=1) %>% 
  group_by(basin) %>% 
  summarize(codes=sum(codes)) %>% 
  ungroup()

junk<-location[location$LakeID=="0403UWB0124A"|location$LakeID=="0403UWB5243"|location$LakeID=="0404NUN0084A"|location$LakeID=="0902ALL5147"|location$LakeID=="0906BON0024"|location$LakeID=="1401LUX0212"|location$LakeID=="1401UWB5217"|location$LakeID=="1403BEA5572"|location$LakeID=="1403UWB0368E",]



names(chloro)[names(chloro)=="Result.Value"]<-"chloro"
chloro$year<-NULL
names(phosphorus)[names(phosphorus)=="Result.Value"]<-"phosphorus"
phosphorus$year<-NULL
names(secchi)[names(secchi)=="Result.Value"]<-"secchi"
secchi$year<-NULL
combined<-merge(chloro,secchi,by=c('LAKE_ID','SAMPLE_DATE'),all=TRUE)
combined<-merge(combined,phosphorus,by=c('LAKE_ID','SAMPLE_DATE'),all=TRUE)
combined<-combined[!is.na(combined$chloro),]
combined<-combined[!is.na(combined$secchi),]
combined<-combined[!is.na(combined$phosphorus),]
library(ggpubr)
ggplot(combined,aes(x=secchi,y=chloro)) +
 geom_point() +
  xlim(0,5)+
  ylim(0,100)+
  geom_hline(yintercept=10) +
  geom_vline(xintercept=1.2) 

ggplot(combined,aes(x=phosphorus,y=chloro)) +
  geom_point() +
  xlim(0,0.1)+
  ylim(0,100)+
  geom_hline(yintercept=10) +
  geom_vline(xintercept=0.02) 

ggplot(combined,aes(x=secchi,y=phosphorus)) +
  geom_point() +
  xlim(0,5)+
  ylim(0,0.1)+
  geom_hline(yintercept=0.02) +
  geom_vline(xintercept=1.2) 


data<-read.csv("sections/data/data.backup.all.csv", stringsAsFactors=FALSE)
data<-data[data$WATER=="Moreau Lake"|data$WATER=="Rockland Lake",]


junk<-blocation[blocation$LakeID=="1005BAR0338"|
                  blocation$LakeID=="1003ROL0168"|
                  blocation$LakeID=="1004UCA0271"|
                  blocation$LakeID=="1701LAU0496"|
                  blocation$LakeID=="1701MAR0495"|
                  blocation$LakeID=="0601GIL0287"|
                  blocation$LakeID=="0601GOE0378"|
                  blocation$LakeID=="1702SHE1087"|
                  blocation$LakeID=="0601ESI0262"|
                  blocation$LakeID=="0602LLY5637"|
                  blocation$LakeID=="1003COL0106"|
                  blocation$LakeID=="1702OAK0107"|
                  blocation$LakeID=="1003CLE0199"|
                  blocation$LakeID=="0601YOU0408"|
                  blocation$LakeID=="1003LSA0104"|
                  blocation$LakeID=="1702BRY1106"|
                  blocation$LakeID=="1003KUS0055"|
                  blocation$LakeID=="0602ROU0082"|
                  blocation$LakeID=="1701GRE0885"|
                  blocation$LakeID=="1003MOO0083"|
                  blocation$LakeID=="0902BAR0262"|
                  blocation$LakeID=="1701LOW0855"|
                  blocation$LakeID=="1701PEC0555"|
                  blocation$LakeID=="1003FRA5095"|
                  blocation$LakeID=="1101MOR0101"|
                  blocation$LakeID=="1501ROC0985",]


sample<-read.csv("C:/Rscripts/DatabaseModernization/Database.proposal/lakes/Sample.csv", stringsAsFactors=FALSE)

junk$SAMPLE_DATE<-as.Date(junk$SAMPLE_DATE,format="%m/%d/%Y")

bottles<- read.csv("data/New York State Office of Information Technology Services/Sampling Season - Documents/LCI - 2018.ranked.intensive.sites.csv", stringsAsFactors=FALSE) 


Rockland<-Rockland[Rockland$WATER=="Sturgeon Pool",]
Rockland<-Rockland[!is.na(Rockland$Characteristic.Name),]

Rockland<-unique(Rockland[c('LAKE_ID','Waterbody_Classification')])


junk<-unique(sampling[c('basin','Trip','team','Day','Order','Departure','Return','startnext')])
junk<-junk %>% 
  arrange(basin,Trip,team,Day,Order)

lakes<-merge(blake,blocation,by=c('LakeID','County'))
junk<-lakes[lakes$LakeID=="1403BIG0345",]
unique(junk[c('LakeID','WATER','PWLID','Waterbody_Classification','Y_Coordinate','X_Coordinate','LocationName','County')])
#write.csv(junk,file="junk.csv",row.names=FALSE)
head(junk)


library(dplyr)

intensive2<-intensive

intensive2$PIdrinking[is.na(intensive2$PIdrinking)]<-0
intensive2$PIrecreation[is.na(intensive2$PIrecreation)]<-0
intensive2$PIaquatic[is.na(intensive2$PIaquatic)]<-0
intensive2$eutrophic[is.na(intensive2$eutrophic)]<-0
intensive2$LAKE_ID[intensive2$LAKE_ID==""]<-NA
intensive2<-intensive2[!is.na(intensive2$LAKE_ID),]
intensive2<-distinct(intensive2)

intensive2<-intensive2 %>% 
  select(LAKE_ID,WATER,LOCATION_ID,Waterbody_Classification,PWS,PWLID,Beaches,Y_Coordinate,X_Coordinate,Type,SAMPLE_ID,SAMPLE_NAME,DATA_PROVIDER,SAMPLE_DATE,TIME,START_DEPTH,END_DEPTH,Depth,Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS,WIPWL,PIdrinking,PIrecreation,PIaquatic,eutrophic,mesotrophic,oligotrophic) %>% 
  group_by(LAKE_ID) %>% 
  mutate(countdrink=sum(PIdrinking)) %>% 
  mutate(countrecreat=sum(PIrecreation)) %>% 
  mutate(countaquatic=sum(PIaquatic)) %>% 
  mutate(counteutrophic=sum(eutrophic)) %>% 
  ungroup()

intensive2<-intensive2 %>% 
  arrange(LAKE_ID,SAMPLE_DATE,Characteristic.Name)

intensive2<-intensive2[!is.na(intensive2$LAKE_ID),]

#pull most recent sample date
intensive2$SAMPLE_DATE<-as.Date(intensive2$SAMPLE_DATE,format="%m/%d/%Y")


intensive2<-intensive2 %>% 
  group_by(LAKE_ID) %>% 
  mutate(year=substring(SAMPLE_DATE,1,4)) %>% 
  ungroup()

intensive2$year<-as.numeric(intensive2$year)
intensive2$year[is.na(intensive2$year)]<-0
  
intensive2<-intensive2 %>% 
  group_by(LAKE_ID) %>% 
  mutate(yearmax=max(year)) %>% 
  ungroup()

intensive2<-intensive2 %>% 
  select(LAKE_ID,WATER,LOCATION_ID,Waterbody_Classification,PWS,PWLID,Beaches,Y_Coordinate,X_Coordinate,Type,SAMPLE_ID,SAMPLE_NAME,DATA_PROVIDER,SAMPLE_DATE,TIME,START_DEPTH,END_DEPTH,Depth,Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS,WIPWL,PIdrinking,PIrecreation,PIaquatic,eutrophic,mesotrophic,oligotrophic) %>% 
  group_by(LAKE_ID,Characteristic.Name) %>% 
  mutate(countdrink=sum(PIdrinking)) %>% 
  mutate(countrecreat=sum(PIrecreation)) %>% 
  mutate(countaquatic=sum(PIaquatic)) %>% 
  mutate(counteutrophic=sum(eutrophic)) %>% 
  ungroup()


junk<-intensive[intensive$LAKE_ID=="1301LAW0207",]
junk<-junk[junk$Characteristic.Name=="PH",]
junk<-junk[junk$SAMPLE_ID=="37823",]
junk

intensive<-backupintensive

junk<-blocation %>% 
  filter(LakeID %in% c("1202WOO0605D","1301BUC0303","1301BUR0386C","1301COX6780","1301PUD5194","1301SWA0030A","1301UWB0420B","1302UWB0044H","1306BIN0735","1306GLE0668","1306MIL0515","1501UWB1006","1701BLA0788","1701LIL0779"))
junk<-unique(junk[c('LakeID','X_Coordinate','Y_Coordinate')])
junk



backupintensive<-intensive



intensive<-backupintensive
intensive <- intensive %>% 
  filter(Characteristic.Name %in% c("DEPTH, SECCHI DISK DEPTH","DISK, SECCHI DISK DEPTH"))
intensive<-intensive[!is.na(intensive$SAMPLE_DATE),]

intensive<-intensive %>% 
  group_by(LAKE_ID,Characteristic.Name) %>% 
  mutate(yrnumber=n_distinct(year))%>% 
  mutate(minyear=min(year)) %>% 
  mutate(maxyear=max(year)) %>% 
  ungroup()
intensive<-intensive[intensive$yrnumber>9,]
intensive<-unique(intensive[c('LAKE_ID','WATER','X_Coordinate','Y_Coordinate','yrnumber','minyear','maxyear')])
write.csv(intensive,file="formatt.junk.csv",row.names=FALSE)


rebecca.check.RT<-data[data$INFO_TYPE=="RT",]
rebecca.check.RT<-rebecca.check.RT[!is.na(rebecca.check.RT$Characteristic.Name),]
write.csv(rebecca.check.RT,file="junk.rebecca.check.RT.csv",row.names=FALSE)
rebecca.check.MI.DI<-data %>% 
  filter(INFO_TYPE %in% c("MI","DI"))
write.csv(rebecca.check.MI.DI,file="junk.rebecca.check.MI.DI.csv",row.names=FALSE)

matt<-hypoepi[!is.na(hypoepi$TSI),]
matt2<-profiles[!is.na(profiles$TSI),]
matt2$Depth<-NULL
matt<-merge(matt,matt2,all=TRUE)
matt<-matt[!is.na(matt$SAMPLE_ID),]
library(tidyr)
matt<-matt %>% 
  group_by(LAKE_ID,Characteristic.Name,year) %>% 
  summarize(annualav= mean(TSI)) %>% 
  ungroup()

sample$SAMPLE_DATE<-as.Date(sample$SAMPLE_DATE,format="%m/%d/%Y")
#create year column
library(tidyr)
junk<-sample %>% 
  mutate(year=substring(SAMPLE_DATE,1,4))
max(junk$year)
unique(junk$year)

#create year column
library(tidyr)
junk<-results %>% 
  mutate(year=substring(Sample.5..SAMPLE_DATE,1,4))
junk<-junk[junk$year=="2018",]
unique(junk$Characteristic.Name)

junk$SAMPLE_DATE<-as.Date(junk$SAMPLE_DATE,format="%m/%d/%Y")
#create year column
library(tidyr)
junk<-junk %>% 
  mutate(year=substring(SAMPLE_DATE,1,4))
max(junk$year)
unique(junk$year)

library(tidyr)
library(dplyr)
junksample<-unique(sample[c('SAMPLE_ID','SAMPLE_DATE','SAMPLE_NAME')])
junksample$SAMPLE_DATE<-as.Date(junksample$SAMPLE_DATE,format="%m/%d/%Y")
junksample<-junksample %>% 
  mutate(yearsample=substring(SAMPLE_DATE,1,4))
junkresult<-unique(results[c('SAMPLE_ID','Sample.5..SAMPLE_DATE','Characteristic.Name','SAMPLE_NAME')])
junkresult<-junkresult %>% 
  mutate(yearresults=substring(Sample.5..SAMPLE_DATE,1,4))


junk<-merge(junksample,junkresult,by=c('SAMPLE_ID'),all = TRUE)
junk<-junk[junk$yearresults=="2018",]
unique(junk$Characteristic.Name)
junk<-junk[!is.na(junk$SAMPLE_ID),]
unique(junk$Characteristic.Name)


junksample<-unique(sample[c('SAMPLE_ID','SAMPLE_DATE')])
junkresults<-unique(results[c('SAMPLE_ID','Sample.5..SAMPLE_DATE')])
junksample$sample<-"sample"
junksample$results<-"results"
junk<-merge(junksample,junkresults,by=c('SAMPLE_ID'),all=TRUE)


jresults<-unique(results[c('SAMPLE_NAME','SAMPLE_ID')])
jresults$results<-"results"
jresults2018<-unique(results2018[c('SAMPLE_NAME','Characteristic.Name')])
jresults2018$results2018<-"results2018"
junk<-merge(jresults,jresults2018,by=c("SAMPLE_NAME"),all=TRUE)
junk<-junk[!is.na(junk$results2018),]

jsample<-unique(sample[c('SAMPLE_NAME','SAMPLE_ID')])
jsample$sample<-"sample"
jresults2018<-unique(results2018[c('SAMPLE_NAME','Characteristic.Name')])
jresults2018$results2018<-"results2018"
junk<-merge(jsample,jresults2018,by=c("SAMPLE_NAME"),all=TRUE)
junk<-junk[!is.na(junk$results2018),]
unique(junk$Characteristic.Name)

hypoepi$Depth<-NA
sarah<-merge(hypoepi,profiles,all=TRUE)


library(openxlsx)
sample<-read.xlsx("data/New York State Office of Information Technology Services/Sampling Season - Documents/LCI.Field.Season.xlsx" , 3, detectDates = FALSE)

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
#running secchi regression for Brian
secchi<-profiles[profiles$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",]
secchi$depth<-NULL
phchla<-hypoepi[hypoepi$Characteristic.Name=="CHLOROPHYLL A"|hypoepi$Characteristic.Name=="PHOSPHORUS",]
regression<-merge(secchi,phchla,all=TRUE)
rm(list=c('secchi','phchla'))
regression<-regression %>% 
  filter(DATA_PROVIDER %in% c('LCI','IL'))
regression<-regression %>% 
  filter(INFO_TYPE %in% c('epilimnion','SD'))

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
#merging files for stephanie
assess<-merge(hypoepi,profiles,all=TRUE)
assess<-assess[!is.na(assess$Characteristic.Name),]
assess<-unique(assess[c('LAKE_ID','Waterbody_Classification','SAMPLE_DATE','Characteristic.Name','INFO_TYPE','Result.Sample.Fraction','Result.Value','Result.Unit','assessment','violation')])
assess<-assess[!is.na(assess$Characteristic.Name),]
assess$violation<-ifelse(assess$violation=="no WQS violation",0,1)
library(dplyr)
assess<-distinct(assess)
assess<-assess %>% 
  group_by(LAKE_ID,Characteristic.Name) %>% 
  mutate(n = n()) %>% 
  ungroup()
assess<-assess %>% 
  group_by(LAKE_ID,Characteristic.Name,n) %>% 
  summarize(violations = sum(violation)) %>% 
  ungroup()
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

profiles1<-profiles[profiles$LAKE_ID=="1501ROC0985",]
hypoepi1<-hypoepi[hypoepi$LAKE_ID=="1501ROC0985",]
profiles1$Result.Value<-as.numeric(profiles1$Result.Value)
hypoepi1$Result.Value<-as.numeric(hypoepi1$Result.Value)
TP<-hypoepi1[hypoepi1$Characteristic.Name=="PHOSPHORUS",]
TP<-TP[TP$Result.Sample.Fraction=="T",]
TPepi<-TP[TP$INFO_TYPE=="epilimnion",]
TPhypo<-TP[TP$INFO_TYPE=="hypolimnion",]
secchi<-profiles1[profiles1$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",]
chla<-hypoepi1[hypoepi1$Characteristic.Name=="CHLOROPHYLL A",]
mean(TPepi$Result.Value)
mean(TPhypo$Result.Value)
mean(secchi$Result.Value)
mean(chla$Result.Value)

rm(list=c('hypoepi','habs','hypoepi1','lake','lake1','profiles','profiles1','secchi','thresh','thresholds','trophicass','trophicass1','i','ids','lakes','nlakes','titles'))



lake<-blake
habs<-bhabs
land<-bland
location<-blocation
profiles<-bprofiles
results<-bresults
sample<-bsample
rm(list=c('data','infos','Lake.Master','Location','Sample'))

lakes<-lakes[lakes$LAKE_ID=="1302CAS0059B",]


temp_result <- read.table("R1908083/TestResultQC_v3.txt",sep=",",fill=TRUE,header=FALSE,stringsAsFactors=FALSE,
                          col.names = c("sys_sample_code","lab_anl_method_name","analysis_date","fraction","column_number","test_type","lab_matrix_code","analysis_location","basis","container_id","dilution_factor","prep_method","prep_date","leachate_method","leachate_date","lab_name_code","qc_level","lab_sample_id","percent_moisture","subsample_amount","subsample_amount_unit","analyst_name","instrument_id","comment","preservative","final_volume","final_volume_unit","cas_rn","chemical_name","result_value","result_error_delta","result_type_code","reportable_result","detect_flag","lab_qualifiers","validator_qualifiers","interpreted_qualifiers","validated_yn","method_detection_limit","reporting_detection_limit","quantitation_limit","result_unit","detection_limit_unit","tic_retention_time","minimum_detectable_conc","counting_error","uncertainty","critical_value","validation_level","result_comment","qc_original_conc","qc_spike_added","qc_spike_measured","qc_spike_recovery","qc_dup_original_conc","qc_dup_spike_added","qc_dup_spike_measured","qc_dup_spike_recovery","qc_rpd","qc_spike_lcl","qc_spike_ucl","qc_rpd_cl","qc_spike_status","qc_dup_spike_status","qc_rpd_status","lab_sdg"))


temp_result <- read.table("R1907827/TestResultQC_v3.txt",sep=",",fill=TRUE,header=FALSE,stringsAsFactors=FALSE,
                          col.names = c("sys_sample_code","lab_anl_method_name","analysis_date","fraction","column_number","test_type","lab_matrix_code","analysis_location","basis","container_id","dilution_factor","prep_method","prep_date","leachate_method","leachate_date","lab_name_code","qc_level","lab_sample_id","percent_moisture","subsample_amount","subsample_amount_unit","analyst_name","instrument_id","comment","preservative","final_volume","final_volume_unit","cas_rn","chemical_name","result_value","result_error_delta","result_type_code","reportable_result","detect_flag","lab_qualifiers","validator_qualifiers","interpreted_qualifiers","validated_yn","method_detection_limit","reporting_detection_limit","quantitation_limit","result_unit","detection_limit_unit","tic_retention_time","minimum_detectable_conc","counting_error","uncertainty","critical_value","validation_level","result_comment","qc_original_conc","qc_spike_added","qc_spike_measured","qc_spike_recovery","qc_dup_original_conc","qc_dup_spike_added","qc_dup_spike_measured","qc_dup_spike_recovery","qc_rpd","qc_spike_lcl","qc_spike_ucl","qc_rpd_cl","qc_spike_status","qc_dup_spike_status","qc_rpd_status","lab_sdg"))
temp_sample <- read.table("R1905379/Sample_v3.txt",sep=",",fill=TRUE,header=FALSE, stringsAsFactors=FALSE,
                          col.names = c("#data_provider","sys_sample_code","sample_name","sample_matrix_code","sample_type_code","sample_source","parent_sample_code","sample_delivery_group","sample_date","sys_loc_code","start_depth","end_depth","depth_unit","chain_of_custody","sent_to_lab_date","sample_receipt_date","sampler","sampling_company_code","sampling_reason","sampling_technique","task_code","collection_quarter","composite_yn","composite_desc","sample_class","custom_field_1","custom_field_2","custom_field_3","comment"))
# NOTE THAT ROW ORDER OF INPUT FILE IS NOT RETAINED
temp_RSmerge <- merge(temp_result,temp_sample,by="sys_sample_code", all=TRUE)

library(dplyr)

tp<-tp %>% 
  filter(DATA_PROVIDER %in% c("CSL","LCI"))
tp$year<-substr(tp$SAMPLE_NAME,1,2)

tp<-tp %>% filter(year %in% c("18","17","16","15","14","13","12","11","10","09"))

tpsum<-tp
tpsum<-unique(tp[c('LAKE_ID','year')])
tpsum$n<-1
tpsum<-tpsum %>% 
  group_by(year) %>% 
  summarize(lakes=sum(n)) %>% 
  ungroup()

tpsu<-tp
tpsu<-unique(tp[c('LAKE_ID','SAMPLE_NAME','year')])
tpsu$n<-1
tpsu<-tpsu %>% 
  group_by(year) %>% 
  summarize(samples=sum(n)) %>% 
  ungroup()

tps<-merge(tpsum,tpsu,by=c('year'),all=TRUE)


#HABS dat
library(dplyr)
habs<-read.csv("HABs.CandidateLakes_Wkshp.csv")
habshort<-habs %>% 
  select(Lake.Name,Relative.Size,Relative.Depth,Trophic.Status,Number.of.Years.with.HABs..2012.2018.,Mitigation.History,Brief.Summary)
habshort$Relative.Size<-factor(habshort$Relative.Size,levels=c('Very Large','Large','Medium','Small','Very Small'))
habshort<-habshort[order(habshort$Relative.Size,habshort$Relative.Depth,habshort$Trophic.Status,habshort$Number.of.Years.with.HABs..2012.2018),]
habshort$habsyrs<-habshort$Number.of.Years.with.HABs..2012.2018.
habshort$Number.of.Years.with.HABs..2012.2018.<-NULL
habshort <-habshort %>% 
  select()

library(dplyr)
habsadd<-read.csv("HABs.additional.csv")
habsadd$LAKE_ID<-habsadd$LakeID
habsadd$LakeID<-NULL
acres<-data %>% 
  select(LAKE_ID,ACRES)
#acres<-acres[acres$INFO_TYPE=="OW"|acres$INFO_TYPE=="BS",]
habsadd<-merge(habsadd,acres,by=c('LAKE_ID'),all.x = TRUE)
habsadd<-distinct(habsadd)
habsadd$Relative.Size<-NA
head(habsadd)
#write.csv(habsadd,file="HABs.Add.With.Acreage.csv",row.names=FALSE)

habsadd2<-read.csv("HABs.Add.With.Acreage.csv")
habsadd2<-habsadd2[order(habsadd2$Relative.Size,habsadd2$Trophic.Status),]
write.csv(habsadd2,file="HABs.Add.With.Acreage2.csv",row.names=FALSE)


library(dplyr)
tp<-data[data$Characteristic.Name=="PHOSPHORUS",]
tp<-tp[tp$Result.Sample.Fraction=="T",]
tp<-tp[tp$INFO_TYPE=="OW",]
tp<-distinct(tp)
tp$year<-format(as.Date(tp$SAMPLE_DATE,format="%m/%d/%Y"),"%Y")
tp$SAMPLE_DATE<-as.Date(tp$SAMPLE_DATE,format="%m/%d/%Y")
unique(tp$year)
unique(tp$SAMPLE_DATE)
write.csv(tp,file="Leslies.TP.csv",row.names=FALSE)


sample<-read.csv("C:/Rscripts/current.backup.2019.11.20/Sample.csv")
lake<-read.csv("C:/Rscripts/current.backup.2019.11.20/Lake.Master.csv")
location<-read.csv("C:/Rscripts/current.backup.2019.11.20/Location.csv")
results<-read.csv("C:/Rscripts/current.backup.2019.11.20/Test.Results.csv")
habs<-read.csv("C:/Rscripts/current.backup.2019.11.20/HABstatus.csv")
profiles<-read.csv("C:/Rscripts/current.backup.2019.11.20/DEPTH.PROFILE.csv")


data<-merge(sample,results,by=c('SAMPLE_ID','SAMPLE_NAME','INFO_TYPE'),all.x=TRUE)
data2<-merge(sample,profiles,by=c("SAMPLE_ID",'INFO_TYPE'),all.x=TRUE)
data<-merge(data,data2,by=c("SAMPLE_ID","LAKE_ID","LOCATION_ID","SAMPLE_NAME","INFO_TYPE","SAMPLE_DATE","TIME","START_DEPTH",        
                            "END_DEPTH","DATA_PROVIDER","ESF.","UFI.","REMARK","BLOOM_LOC","WIND_DIR","WIND_INT","EXTENT",
                            "BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE","SAMPLE_METHOD","PERCENT","DEPTH_UNIT",
                            "Characteristic.Name","Result.Value","Result.Unit"),all=TRUE)
rm(data2)
data<-merge(data,lake,by=c('LAKE_ID'),all.x=TRUE)
data<-merge(data,location,by=c('LOCATION_ID','LAKE_ID'),all.x = TRUE)
data<-merge(data,habs, by=c('SAMPLE_ID'),all.x = TRUE)

#remove individual data tables in working environment
rm(list = c('lake','profiles','location','results','sample','habs'))
#Make sure records are distinct
library(dplyr)
data<-distinct(data)







junk<-data[data$Characteristic.Name=="CHLORIDE",]
junk<-junk[!is.na(junk$Characteristic.Name),]
library(dplyr)
library(tidyr)
junk$SAMPLE_DATE<-as.Date(junk$SAMPLE_DATE,format="%m/%d/%Y")
junk<-unique(junk[c('LAKE_ID','LOCATION_ID','DATA_PROVIDER','SAMPLE_DATE')])
junk<-junk %>% 
  mutate(year = format(SAMPLE_DATE, "%Y")) %>%
  group_by(DATA_PROVIDER,year) %>% 
  mutate(n=n()) %>% 
  ungroup()
junk<-unique(junk[c('DATA_PROVIDER','year','n')])

junk<-hypoepi[hypoepi$LAKE_ID=="1402GUY0014",]




junk <- lake %>% 
  filter(LAKE_ID %in% c('0102LA 5022','0201SCI0090B','0403AND0164','0501LOW0021','0501MIL0016','0501UWB5107','0502BIR5140','0503ERW0023B','0503UWB5183','0602BAL5648','0703PEN0016','0801WHE5670','0906GRA0051','1201UWB0503','1202WOO0605D','1301BUC0303','1301BUR0386C','1301COX6780','1301PUD5194','1301SWA0030A','1301UWB0420B','1302UWB0044H','1306BIN0735','1306MIL0515','1501UWB1006','1701BLA0788','1701LIL0779','1702IND5821'))

junk2<-unique(junk[c('LAKE_ID','PWLID','Waterbody_Classification',"Y_Coordinate","X_Coordinate")])


























temp<-data
#load packages
library(dplyr)
library(tidyr)
#set date as date
temp$SAMPLE_DATE<-as.Date(data$SAMPLE_DATE,"%m/%d/%Y")

#pull OW TP samples
tempp<-temp %>% 
  filter(Characteristic.Name %in% c('PHOSPHORUS')) %>% 
  filter(INFO_TYPE %in% c('OW')) %>% 
  filter(Result.Sample.Fraction %in% c('T')) %>% 
  select(LAKE_ID,LOCATION_ID,SAMPLE_DATE,Characteristic.Name,Result.Value)
#tempcs<-temp %>% 
#  filter(Characteristic.Name %in% c('CHLOROPHYLL A','DEPTH, SECCHI DISK DEPTH')) %>% 
#  filter(INFO_TYPE %in% c('OW','DP')) %>% 
#  select(LAKE_ID,LOCATION_ID,SAMPLE_DATE,Characteristic.Name,Result.Value)
#temp<-merge(tempp,tempcs,all=TRUE)
temp<-distinct(temp)
rm(list=c('tempcs','tempp'))

#fix wacky sample date
temp[temp$SAMPLE_DATE=="1899-12-31",]$SAMPLE_DATE<-"1999-12-31"

#pull year
temp<-temp %>% 
  mutate(year = format(SAMPLE_DATE, "%Y")) %>%
  mutate(year = as.numeric(year)) %>% 
  mutate(decade = year - year %% 10)
temp<-distinct(temp)
temp$Result.Value<-as.numeric(temp$Result.Value)

temp$TSI<-NA
temp$TSI<-ifelse(temp$Characteristic.Name=="DEPTH, SECCHI DISK DEPTH",
                 ifelse(temp$Result.Value>0,(60-(14.41*(log(temp$Result.Value)))),temp$TSI),
                 temp$TSI)
temp$TSI<-ifelse(temp$Characteristic.Name=="PHOSPHORUS",
                 ifelse(temp$Result.Value>0,(4.15+(14.42*(log(1000*temp$Result.Value)))),temp$TSI),temp$TSI)
temp$TSI<-ifelse(temp$Characteristic.Name=="CHLOROPHYLL A",
                 ifelse(temp$Result.Value>0,(30.6+(14.42*(log(temp$Result.Value)))),temp$TSI),
                 temp$TSI)
temp<-temp[!is.na(temp$TSI),] 
temp<-distinct(temp)


#pull waterbodies with at least one sample in each decade
decade<-unique(temp[c('LAKE_ID','decade')])
decade<-decade %>% 
  group_by(LAKE_ID) %>% 
  summarize(n()) %>% 
  ungroup()
decade$decades<-decade$`n()`
decade$`n()`<-NULL
decade<-decade[decade$decades>3,]
temp<-merge(decade,temp,by=c('LAKE_ID'),all.x = TRUE)
rm(decade)
temp$decade<-NULL
temp$decades<-NULL

library(trend)
library(xts)
junk<-temp[temp$LAKE_ID=="0201CUB0115",]
junk<-unique(junk[c('SAMPLE_DATE','TSI')])
junk <- xts(junk[,-1], order.by=as.Date(junk[,1], "%m/%d/%Y"))
res <- smk.test(junk)


junk<-hypoepi[hypoepi$LAKE_ID=="0404NUN0084A",]
junk<-junk[!is.na(junk$LAKE_ID),]

junk<-profiles[profiles$LAKE_ID=="0404NUN0084A",]
junk<-junk[!is.na(junk$LAKE_ID),]

junk<-data[data$LAKE_ID=="0906MUS0008",]
junk<-junk[!is.na(junk$LAKE_ID),]



junk<-data %>% 
  filter(grepl("Black Lake",WATER))
  hypoepi$simpleWC<-ifelse(grepl("D",hypoepi$Waterbody_Classification),"D",hypoepi$simpleWC)

  
  junk<-profiles[profiles$SAMPLE_ID=="63859",]
  junk<-junk[!is.na(junk$SAMPLE_ID),]
  
  
  
  junk<-data[data$SAMPLE_ID=="63859",]
  junk<-junk[!is.na(junk$SAMPLE_ID),]
  
  junk<-lake[lake$SAMPLE_ID=="63859",]
  junk<-junk[!is.na(junk$SAMPLE_ID),]
  
exist_results_table<-exist_results_table[!is.na(exist_results_table$SAMPLE_ID),]
exist_sample_table<-exist_sample_table[!is.na(exist_sample_table$SAMPLE_ID),]
write.csv(exist_results_table,file="sections/data/projectData/LCI.2019/results_to_add_to_results_table.csv",row.names = FALSE)
write.csv(exist_sample_table,file="sections/data/projectData/LCI.2019/samples_to_add_to_sample_table.csv",row.names = FALSE)


junk<-exist_sample_table
junk$n<-1
junk<-junk %>% 
  select(SAMPLE_ID,n) %>% 
  distinct() %>% 
  arrange(SAMPLE_ID) %>% 
  filter(SAMPLE_ID %in% c('63883','63877')) %>% 
  distinct()

junk<-exist_sample_table%>% 
  filter(SAMPLE_ID %in% c('63883','63877')) %>% 
  distinct()


junk<-exist_sample_table %>% 
  select(SAMPLE_ID,SAMPLE_NAME,INFO_TYPE) %>% 
  distinct()
exist_results_table<-merge(exist_results_table,junk,by=c('SAMPLE_ID'),all.x = TRUE)
exist_results_table<-exist_results_table[exist_results_table$INFO_TYPE!="DP",]
exist_results_table$INFO_TYPE<-NULL
junk<-junk %>% 
  filter(INFO_TYPE!="DP",
         INFO_TYPE!="SD",
         INFO_TYPE!="SB") %>% 
  distinct()