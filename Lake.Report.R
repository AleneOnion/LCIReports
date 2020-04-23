#creating lake report
#alene Onion
# April 2019

library(dplyr)
library(tidyr)

####################################################################################################
#Reading in Data tables
#read data tables
lake<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Lake.Master.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
profiles<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/DEPTH.PROFILE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
results<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Test.Results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sample<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/Sample.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
habs<-read.csv("//dec-home/DEC_HOME/amonion/Lakes.Database/data/current/HABstatus.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#Merge data tables
data<-merge(sample,results,by=c('SAMPLE_ID','SAMPLE_NAME','INFO_TYPE'),all.x=TRUE)
data<-data %>% filter(!is.na(Characteristic.Name))
data2<-merge(sample,profiles,by=c("SAMPLE_ID",'INFO_TYPE'),all.x=TRUE)
data2<-data2 %>% filter(!is.na(Characteristic.Name))
data<-merge(data,data2,by=c("LAKE_ID","LOCATION_ID","SAMPLE_ID","SAMPLE_NAME","DATA_PROVIDER","INFO_TYPE","SAMPLE_DATE",
                            "TIME","START_DEPTH","END_DEPTH","ESF.","UFI.","REMARK","BLOOM_LOC",
                            "WIND_DIR","WIND_INT","EXTENT","BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE",
                            "SAMPLE_METHOD","PERCENT","DEPTH_UNIT","Characteristic.Name","Result.Value","Result.Unit"),all=TRUE)
rm(data2)
data<-merge(data,sample,by=c("LAKE_ID","LOCATION_ID","SAMPLE_ID","SAMPLE_NAME","DATA_PROVIDER","INFO_TYPE","SAMPLE_DATE",
                             "TIME","START_DEPTH","END_DEPTH","ESF.","UFI.","REMARK","BLOOM_LOC",
                             "WIND_DIR","WIND_INT","EXTENT","BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE",
                             "SAMPLE_METHOD","PERCENT","DEPTH_UNIT"),all=TRUE)
data<-merge(data,lake,by=c('LAKE_ID'),all.x=TRUE)
data<-merge(data,location,by=c('LOCATION_ID','LAKE_ID'),all.x = TRUE)
data<-merge(data,habs, by=c('SAMPLE_ID'),all.x = TRUE)

#remove individual data tables in working environment
rm(list = c('lake','profiles','location','results','sample','habs'))
#Make sure records are distinct
library(dplyr)
data<-distinct(data)


####################################################################################################
#Initial subset for this report
library(dplyr)
#data<-read.csv("data.2020.02.15.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
rm(list=c('habs','habs1','hypoepi1','junk','lake1','profiles1','ids','titles','habstatus','hypoepi','lake','laketemp',
          'profiles','thresh','thresholds','trophic','trophicass1','i','lakes','nlakes','p','temp','j','nparams','nsamples',
          'params','samples','display1'))

lake<-data
lake$SAMPLE_DATE<-as.Date(lake$SAMPLE_DATE,format="%Y-%m-%d")
#lake<-lake[lake$SAMPLE_DATE>'2000-01-01',]
lake<-lake %>% 
  mutate(year=substring(SAMPLE_DATE,1,4))
lake<-lake %>% 
  select(LAKE_ID,SAMPLE_ID,SAMPLE_NAME,LOCATION_ID,DATA_PROVIDER,SAMPLE_DATE,TIME,
         Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,Depth,
         WATER,LocationName,Y_Coordinate,X_Coordinate,INFO_TYPE,Waterbody_Classification,PWLID,
         year,ACRES,County,PWS,Beaches,REMARK,STATUS,QUANTITATION_LIMIT,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS)
lake<-lake[!is.na(lake$SAMPLE_ID),]
#####################################################################################################

profiles<- lake[lake$INFO_TYPE=="DP",]
profiles<-profiles[!is.na(profiles$SAMPLE_ID),]
profiles$Result.Value<-as.numeric(profiles$Result.Value)
profiles<-profiles %>% 
  filter(Characteristic.Name %in% c("CHLOROPHYLL A (PROBE)",
                                    "DEPTH",
                                    "DEPTH, BOTTOM",
                                    "DISSOLVED OXYGEN (DO)",
                                    "DISSOLVED OXYGEN SATURATION",
                                    "OXIDATION REDUCTION POTENTIAL (ORP)",
                                    "PH",
                                    "PHYCOCYANIN",
                                    "SPECIFIC CONDUCTANCE",
                                    "TEMPERATURE, AIR",
                                    "TEMPERATURE, WATER",
                                    "TURBIDITY",
                                    "PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)",
                                    "CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)",
                                    "PHYCOCYANIN (PROBE)"))


hypoepi <- lake %>% 
  filter(Characteristic.Name %in% c("ALKALINITY, TOTAL (AS CACO3)",
                                    "AMMONIA", 
                                    "ARSENIC",
                                    "BORON",
                                    "CALCIUM",
                                    "CHLORIDE",
                                    "CHLORIDE (AS CL)",
                                    "CHLOROPHYLL A",
                                    "COLOR, UNKNOWN",
                                    "COPPER",
                                    "DISSOLVED ORGANIC CARBON",
                                    "ETHYLBENZENE",
                                    "FLUORIDE",
                                    "IRON",
                                    "MAGNESIUM",
                                    "MANGANESE",
                                    "NITRITE",
                                    "NITROGEN",
                                    "NITROGEN, KJELDAHL, TOTAL",
                                    "NITROGEN, NITRATE (AS N)",
                                    "NITROGEN, NITRATE-NITRITE",
                                    "NITROGEN, TOTAL",
                                    "NITROGEN, TOTAL DISSOLVED",
                                    "ORTHOPHOSPHATE",
                                    'PH',
                                    "PHOSPHORUS",
                                    "POTASSIUM",
                                    "SILICA",
                                    "SODIUM",
                                    "SOLUBLE REACTIVE PHOSPHORUS (SRP)",
                                    "SODIUM",
                                    'SPECIFIC CONDUCTANCE',
                                    "SULFATE (AS SO4)",
                                    "SULFATE",
                                    'TEMPERATURE, WATER',
                                    "TOTAL ORGANIC CARBON",
                                    "TOTAL DISSOLVED SOLIDS",
                                    "TOTAL HARDNESS",
                                    "TOTAL PHOSPHORUS, MIXED FORMS",
                                    "TOTAL SUSPENDED SOLIDS",
                                    "TRUE COLOR",
                                    "TURBIDITY",
                                    "UV 254"))
hypoepi<-hypoepi %>% 
  filter(INFO_TYPE %in% c('OW','BS'))
hypoepi<-hypoepi[!is.na(hypoepi$Characteristic.Name),]
SD<-lake[lake$INFO_TYPE=="SD",]
SD<-SD[!is.na(SD$SAMPLE_ID),]
hypoepi<-merge(hypoepi,SD,all=TRUE)
#change non-detects to minimum detection value
hypoepi<-hypoepi %>% 
  mutate(Result.Value=ifelse(grepl("U",VALIDATOR_QUALIFIERS),QUANTITATION_LIMIT,Result.Value))



habs<-lake %>% 
  filter(Characteristic.Name %in% c("MICROCYSTIN")) %>% 
  select(SAMPLE_ID,DATA_PROVIDER) %>% 
  distinct()
habs<-merge(habs,lake,by=c('SAMPLE_ID','DATA_PROVIDER'),all.x=TRUE)
habs<-habs %>% 
  filter(Characteristic.Name %in% c("ANATOXIN-A",
                                    "BMAA (BETA-METHYL-AMINO-(L)-ALANINE)",
                                    "CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)",
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, CHLOROPHYTE (GREEN ALGAE)",
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, CHRYSOPHYTA (BROWN ALGAE)",
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, CRYPTOPHYTA (CRYPTOPHYTES)",
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, CYANOBACTERIA (BLUEGREEN)",
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, DINOPHYTA (DIATOMS)",
                                    "CYLINDROSPERMOPSIN",
                                    "DIHYDRO-ATX-A",
                                    "DOMINANT ALGAL SPECIES",
                                    "MICROCYSTIN",
                                    "MICROCYSTIN AR",
                                    "MICROCYSTIN DLR",
                                    "MICROCYSTIN DRR",
                                    "MICROCYSTIN FR",
                                    "MICROCYSTIN HYR",
                                    "MICROCYSTIN LA",
                                    "MICROCYSTIN LR",
                                    "MICROCYSTIN LW",
                                    "MICROCYSTIN LY",
                                    "MICROCYSTIN MLR",
                                    "MICROCYSTIN MRR",
                                    "MICROCYSTIN RR",
                                    "MICROCYSTIN UNKNOWN",
                                    "MICROCYSTIN WR",
                                    "MICROCYSTIN YR")) %>% 
  distinct()

############################################################################################################
#ADDING THRESHOLDS
############################################################################################################

#these thresholds will define thresholds for future plots
thresholds<-read.csv("sections/data/thresholds2.csv", stringsAsFactors=FALSE)
thresholds<-thresholds[thresholds$Characteristic.Name!=0,]
thresholds$notes<-NULL
library(dplyr)
thresholds$Designated_Use<-NULL
thresholds$units<-NULL
thresholds<-distinct(thresholds)
thresholds$simpleT[thresholds$simpleT==""]<-NA
thresholds$limnion[thresholds$limnion==""]<-NA
thresholds$simpleF[thresholds$simpleF==""]<-NA
thresholds$simpleF<-as.character(thresholds$simpleF)
thresholds$simpleF[thresholds$simpleF=="TRUE"]<-"T"


#############################################################################################################
#for hypoepi

#simplify waterbody classification
hypoepi<-hypoepi %>% 
  select(LAKE_ID,WATER,LOCATION_ID,Waterbody_Classification,PWLID,Y_Coordinate,X_Coordinate,SAMPLE_ID,SAMPLE_NAME,DATA_PROVIDER,SAMPLE_DATE,year,TIME,Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,INFO_TYPE,QUANTITATION_LIMIT,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS)
hypoepi$simpleWC<-NA
hypoepi$simpleWC<-ifelse(grepl("D",hypoepi$Waterbody_Classification),"D",hypoepi$simpleWC)
hypoepi$simpleWC<-ifelse(grepl("C",hypoepi$Waterbody_Classification),"C",hypoepi$simpleWC)
hypoepi$simpleWC<-ifelse(grepl("B",hypoepi$Waterbody_Classification),"B",hypoepi$simpleWC)
hypoepi$simpleWC<-ifelse(grepl("A",hypoepi$Waterbody_Classification),"A",hypoepi$simpleWC)
#hypoepi<-hypoepi[!is.na(hypoepi$simpleWC),]
hypoepi$simpleT<-NA
hypoepi$simpleT<-ifelse(grepl("T",hypoepi$Waterbody_Classification),"T",hypoepi$simpleT)
hypoepi$simpleT<-ifelse(grepl("TS",hypoepi$Waterbody_Classification),"TS",hypoepi$simpleT)
hypoepi$limnion<-NA
hypoepi$limnion<-ifelse(hypoepi$Characteristic.Name=="PHOSPHORUS",hypoepi$INFO_TYPE,NA)
hypoepi$simpleF<-NA
hypoepi$simpleF<-ifelse(hypoepi$Characteristic.Name=="PHOSPHORUS",hypoepi$Result.Sample.Fraction,NA)
hypoepi<-distinct(hypoepi)  


#add criteria
hypoepi<-merge(hypoepi,thresholds,by=c('Characteristic.Name','simpleWC','simpleT','limnion','simpleF'),all.x=TRUE)
hypoepi<-distinct(hypoepi)

#calculate violations
#first convert to numeric field
hypoepi$Result.Value.Numeric<-as.numeric(hypoepi$Result.Value)
hypoepi$violation<-NA
hypoepi$violation<-ifelse(hypoepi$direction=="greater",
                            ifelse(hypoepi$Result.Value.Numeric>hypoepi$threshold,1,0),
                            ifelse(hypoepi$direction=="less",
                                   ifelse(hypoepi$Result.Value.Numeric<hypoepi$threshold,1,0),
                                   NA))
hypoepi<-distinct(hypoepi)

#count violations
library(dplyr)
hypoepi<-hypoepi %>% 
  group_by(LAKE_ID,Characteristic.Name,direction,Result.Sample.Fraction) %>% 
  mutate(violationcount=sum(violation)) %>% 
  mutate(samplenumber=length(violation))%>% 
  mutate(yrnumber=n_distinct(year))%>% 
  ungroup()
assess<-hypoepi %>% 
  filter(violation==1) %>% 
  group_by(LAKE_ID,Characteristic.Name,direction) %>% 
  mutate(yrviol=n_distinct(year)) %>% 
  ungroup()
assess<-unique(assess[c('LAKE_ID','Characteristic.Name','yrviol','direction')])
hypoepi<-merge(hypoepi,assess,by=c('LAKE_ID','Characteristic.Name','direction'),all.x = TRUE)
rm(assess)

#can we assess
hypoepi$assess<-NA
hypoepi$assess<-ifelse(hypoepi$yrnumber>1,
                         ifelse(hypoepi$samplenumber>5,'yes','no'),
                         'no')
hypoepi$assessment<-ifelse(hypoepi$assess=="yes",
                             ifelse(hypoepi$violationcount>1,
                                    ifelse(hypoepi$yrviol>1,"impaired","other"),
                                    "other"),
                             NA)

############################################################################################################
#for profiles

#simplify waterbody classification
profiles<-profiles %>% 
  select(LAKE_ID,WATER,LOCATION_ID,Waterbody_Classification,PWLID,Y_Coordinate,X_Coordinate,SAMPLE_ID,SAMPLE_NAME,DATA_PROVIDER,SAMPLE_DATE,year,TIME,Characteristic.Name,Depth,Result.Value,Result.Unit,Result.Sample.Fraction,INFO_TYPE)
profiles$simpleWC<-NA
profiles$simpleWC<-ifelse(grepl("D",profiles$Waterbody_Classification),"D",profiles$simpleWC)
profiles$simpleWC<-ifelse(grepl("C",profiles$Waterbody_Classification),"C",profiles$simpleWC)
profiles$simpleWC<-ifelse(grepl("B",profiles$Waterbody_Classification),"B",profiles$simpleWC)
profiles$simpleWC<-ifelse(grepl("A",profiles$Waterbody_Classification),"A",profiles$simpleWC)
profiles<-profiles[!is.na(profiles$simpleWC),]
profiles$simpleT<-NA
profiles$simpleT<-ifelse(grepl("T",profiles$Waterbody_Classification),"T",profiles$simpleT)
profiles$simpleT<-ifelse(grepl("TS",profiles$Waterbody_Classification),"TS",profiles$simpleT)
profiles$limnion<-NA
profiles$limnion<-ifelse(profiles$Characteristic.Name=="PHOSPHORUS",profiles$INFO_TYPE,NA)
profiles$simpleF<-NA
profiles$simpleF<-ifelse(profiles$Characteristic.Name=="PHOSPHORUS",hypoepi$Result.Sample.Fraction,NA)
profiles<-distinct(profiles)  


#add criteria
profiles<-merge(profiles,thresholds,by=c('Characteristic.Name','simpleWC','simpleT','limnion','simpleF'),all.x=TRUE)
profiles<-distinct(profiles)

#calculate violations
profiles$violation<-NA
profiles$violation<-ifelse(profiles$direction=="greater",
                          ifelse(profiles$Result.Value>profiles$threshold,1,0),
                          ifelse(profiles$direction=="less",
                                 ifelse(profiles$Result.Value<profiles$threshold,1,0),
                                 0))

profiles<-distinct(profiles)

#count violations
library(dplyr)
profiles<-profiles %>% 
  group_by(LAKE_ID,Characteristic.Name,direction) %>% 
  mutate(violationcount=sum(violation)) %>% 
  mutate(samplenumber=length(violation))%>% 
  mutate(yrnumber=n_distinct(year))%>% 
  ungroup()
assess<-profiles %>% 
  filter(violation==1) %>% 
  group_by(LAKE_ID,Characteristic.Name,direction) %>% 
  mutate(yrviol=n_distinct(year)) %>% 
  ungroup()
assess<-unique(assess[c('LAKE_ID','Characteristic.Name','yrviol','direction')])
profiles<-merge(profiles,assess,by=c('LAKE_ID','Characteristic.Name','direction'),all.x = TRUE)
rm(assess)

#can we assess
profiles$assess<-NA
profiles$assess<-ifelse(profiles$yrnumber>1,
                       ifelse(profiles$samplenumber>5,'yes','no'),
                       'no')
profiles$assessment<-ifelse(profiles$assess=="yes",
                           ifelse(profiles$violationcount>1,
                                  ifelse(profiles$yrviol>1,"impaired","other"),
                                  "other"),
                           NA)

############################################################################################################
#restrict to one lake
############################################################################################################
#simplify thresholds table, profiles, and hypoepi
thresh<-thresholds
thresh$limnion[thresh$limnion=="OW"]<-"epilimnion"




hypoepi[is.na(hypoepi$violation),]$violation<-"no WQS violation"
hypoepi[(hypoepi$violation==0),]$violation<-"no WQS violation"
hypoepi[(hypoepi$violation==1),]$violation<-"WQS violation"

hypoepi$INFO_TYPE<-ifelse(hypoepi$INFO_TYPE=="BS","hypolimnion",hypoepi$INFO_TYPE)
hypoepi$INFO_TYPE<-ifelse(hypoepi$INFO_TYPE=="OW","epilimnion",hypoepi$INFO_TYPE)
hypoepi$INFO_TYPE<-ifelse(hypoepi$INFO_TYPE=="SD","secchi",hypoepi$INFO_TYPE)
hypoepi$year<-as.numeric(hypoepi$year)
hypoepi<-hypoepi %>% 
  mutate(Result.Value=trimws(Result.Value),
         Result.Value=as.numeric(Result.Value))


profiles[is.na(profiles$violation),]$violation<-"no WQS violation"
profiles[(profiles$violation==0),]$violation<-"no WQS violation"
profiles[(profiles$violation==1),]$violation<-"WQS violation"
profiles$year<-as.numeric(profiles$year)

#pull specific lakes
#lakes<-c('1401MOU0114','0903LSI0182')
#lakes<-c('0402GOD0017')

lakes<-unique(lake[c('LAKE_ID','WATER')])
lakes<-lakes[!is.na(lakes$LAKE_ID),]
######################################################
#remove specific lakes that don't work
######################################################
lakes<-lakes[lakes$LAKE_ID!="0501UWB5668",]
lakes<-lakes[lakes$LAKE_ID!="0502UWB5192",]
lakes<-lakes[lakes$LAKE_ID!="0705UWB5898",]
lakes<-lakes[lakes$LAKE_ID!="1104ADI0587A",]


#simplify list
lakes<-unique(hypoepi[c('LAKE_ID','WATER')])
lakes<-lakes %>% 
  mutate(LAKE_ID=gsub("#","",LAKE_ID)) %>% 
  distinct()

#remove because of pandoc error
#run separately using lake.report.self contained
lakes<-lakes %>% 
  filter(!LAKE_ID %in% c('1201STEXXX1'))

lakesnoh<-unique(profiles[c('LAKE_ID','WATER')])
lakesnoh<-anti_join(lakesnoh,lakes,by=c('LAKE_ID'))
lakesnoh<-lakesnoh %>% 
  distinct()
#now pull out those lakes that only have habs data
lakeshabs<-unique(habs[c('LAKE_ID','WATER')])
lakeshabs<-anti_join(lakeshabs,lakes,by=c('LAKE_ID'))
lakeshabs<-anti_join(lakeshabs,lakesnoh,by=c('LAKE_ID'))
lakeshabs<-lakeshabs %>% 
  distinct()

#create list of lakes for website:
lakesweb<-merge(lakes,lakesnoh,all=TRUE)
lakesweb<-merge(lakesweb,lakeshabs,all=TRUE)
lakesweb$html<-paste("report_",lakesweb$WATER,"(",lakesweb$LAKE_ID,").html",sep="")
coords<-lake %>% 
  select(LAKE_ID,X_Coordinate,Y_Coordinate) %>% 
  arrange(LAKE_ID,X_Coordinate,Y_Coordinate) %>% 
  distinct(LAKE_ID,.keep_all=TRUE)
lakesweb<-merge(lakesweb,coords,by=c('LAKE_ID'),all.x = TRUE)
lakesweb<-lakesweb %>% 
  distinct()
write.csv(lakesweb,file="lake.list.for.publishing.csv",row.names = FALSE)
rm(list=c('coords','lakesweb'))

#good tests:
#i=184 #lake moraine
#i=9 #lake la sale
#for lakes with hypoepi data:
lakes<-unique(lakes$LAKE_ID)
nlakes<-length(lakes)
for(i in 581:nlakes){
lake1<-lake[lake$LAKE_ID==lakes[i],]
profiles1<-profiles[profiles$LAKE_ID==lakes[i],]
hypoepi1<-hypoepi[hypoepi$LAKE_ID==lakes[i],]
habs1<-habs[habs$LAKE_ID==lakes[i],]
#remove NA
lake1<-lake1[!is.na(lake1$LAKE_ID),]
profiles1<-profiles1[!is.na(profiles1$LAKE_ID),]
hypoepi1<-hypoepi1[!is.na(hypoepi1$LAKE_ID),]
habs1<-habs1[!is.na(habs1$LAKE_ID),]
#pull title
titles<-lake1$WATER[1]
ids<-lake1$LAKE_ID[1]
rmarkdown::render('Lake.Report.Rmd',  # file 2
                  output_file =  paste("report_", titles,"(",ids,")", ".html", sep=''), 
                  output_dir = 'Lake.Reports')
rm(list=c('lake1','profiles1','hypoepi1','habs1','titles','ids','thistitle','ids'))
}

#now for lakes without hypoepi data:
lakes<-unique(lakesnoh$LAKE_ID)
nlakes<-length(lakes)
for(i in 1:nlakes){
  lake1<-lake[lake$LAKE_ID==lakes[i],]
  profiles1<-profiles[profiles$LAKE_ID==lakes[i],]
  habs1<-habs[habs$LAKE_ID==lakes[i],]
  #remove NA
  lake1<-lake1[!is.na(lake1$LAKE_ID),]
  profiles1<-profiles1[!is.na(habs1$LAKE_ID),]
  habs1<-habs1[!is.na(habs1$LAKE_ID),]
  #pull title
  titles<-lake1$WATER[1]
  ids<-lake1$LAKE_ID[1]
  rmarkdown::render('Lake.Report.nohypoepi.Rmd',  # file 2
                    output_file =  paste("report_", titles,"(",ids,")", ".html", sep=''), 
                    output_dir = 'Lake.Reports')
  rm(list=c('lake1','profiles1','hypoepi1','habs1','titles','ids','thistitle','ids'))
}

#now for lakes with only habs data:
lakes<-unique(lakeshabs$LAKE_ID)
nlakes<-length(lakes)
for(i in 1:nlakes){
  lake1<-lake[lake$LAKE_ID==lakes[i],]
  habs1<-habs[habs$LAKE_ID==lakes[i],]
  #remove NA
  lake1<-lake1[!is.na(lake1$LAKE_ID),]
  habs1<-habs1[!is.na(habs1$LAKE_ID),]
  #pull title
  titles<-lake1$WATER[1]
  ids<-lake1$LAKE_ID[1]
  rmarkdown::render('Lake.Report.onlyhabs.Rmd',  # file 2
                    output_file =  paste("report_", titles,"(",ids,")", ".html", sep=''), 
                    output_dir = 'Lake.Reports')
  rm(list=c('lake1','profiles1','hypoepi1','habs1','titles','ids','thistitle','ids'))
  
}