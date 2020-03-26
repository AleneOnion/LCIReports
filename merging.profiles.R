#Merging three profile tables into one
#ALene Onion
#Februrary 2020


library(dplyr)
library(tidyr)

dss1<-read.csv("2019.profile.data/ProDSS1_2019_Backup_cleaned.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
names(dss1)[names(dss1)=="DATE"] <- "SAMPLE_DATE"
names(dss1)[names(dss1)=="SITE"] <- "SAMPLE_NAME"
names(dss1)[names(dss1)=="Depth..m."] <- "Depth"
names(dss1)[names(dss1)=="GPS.Longitude...."] <- "X_Coordinate"
names(dss1)[names(dss1)=="GPS.Latitude...."] <- "Y_Coordinate"
names(dss1)[names(dss1)=="Chlorophyll..RFU."] <- "CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)"
names(dss1)[names(dss1)=="Chlorophyll..µg.L."] <- "CHLOROPHYLL A (PROBE)"
names(dss1)[names(dss1)=="Sp.Cond..µS.cm."] <- "SPECIFIC CONDUCTANCE"
names(dss1)[names(dss1)=="ODO....Sat."] <- "DISSOLVED OXYGEN SATURATION"
names(dss1)[names(dss1)=="ODO..mg.L."] <- "DISSOLVED OXYGEN (DO)"
names(dss1)[names(dss1)=="ORP..mV."] <- "OXIDATION REDUCTION POTENTIAL (ORP)"
names(dss1)[names(dss1)=="PC..RFU."] <- "PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)"
names(dss1)[names(dss1)=="PC..ug.L."] <- "PHYCOCYANIN (PROBE)"
names(dss1)[names(dss1)=="pH"] <- "PH"
names(dss1)[names(dss1)=="Temp...F."] <- "TEMPERATURE, WATER"
dss1<-unique(dss1[c('SAMPLE_DATE','TIME','SAMPLE_NAME','Depth','X_Coordinate','Y_Coordinate',
                  'CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)',
                  'CHLOROPHYLL A (PROBE)',
                  'SPECIFIC CONDUCTANCE',
                  'DISSOLVED OXYGEN SATURATION',
                  'DISSOLVED OXYGEN (DO)',
                  'OXIDATION REDUCTION POTENTIAL (ORP)',
                  'PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)',
                  'PHYCOCYANIN (PROBE)',
                  'PH',
                  'TEMPERATURE, WATER')])

dss2<-read.csv("2019.profile.data/ProDSS2_2019_Backup_cleaned.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
names(dss2)[names(dss2)=="DATE"] <- "SAMPLE_DATE"
names(dss2)[names(dss2)=="SITE"] <- "SAMPLE_NAME"
names(dss2)[names(dss2)=="Depth..m."] <- "Depth"
names(dss2)[names(dss2)=="GPS.Longitude...."] <- "X_Coordinate"
names(dss2)[names(dss2)=="GPS.Latitude...."] <- "Y_Coordinate"
names(dss2)[names(dss2)=="Chlorophyll..RFU."] <- "CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)"
names(dss2)[names(dss2)=="Chlorophyll..µg.L."] <- "CHLOROPHYLL A (PROBE)"
names(dss2)[names(dss2)=="Sp.Cond..µS.cm."] <- "SPECIFIC CONDUCTANCE"
names(dss2)[names(dss2)=="ODO....Sat."] <- "DISSOLVED OXYGEN SATURATION"
names(dss2)[names(dss2)=="ODO..mg.L."] <- "DISSOLVED OXYGEN (DO)"
names(dss2)[names(dss2)=="ORP..mV."] <- "OXIDATION REDUCTION POTENTIAL (ORP)"
names(dss2)[names(dss2)=="PC..RFU."] <- "PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)"
names(dss2)[names(dss2)=="PC..ug.L."] <- "PHYCOCYANIN (PROBE)"
names(dss2)[names(dss2)=="pH"] <- "PH"
names(dss2)[names(dss2)=="Temp...F."] <- "TEMPERATURE, WATER"
dss2<-unique(dss2[c('SAMPLE_DATE','TIME','SAMPLE_NAME','Depth','X_Coordinate','Y_Coordinate',
                    'CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)',
                    'CHLOROPHYLL A (PROBE)',
                    'SPECIFIC CONDUCTANCE',
                    'DISSOLVED OXYGEN SATURATION',
                    'DISSOLVED OXYGEN (DO)',
                    'OXIDATION REDUCTION POTENTIAL (ORP)',
                    'PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)',
                    'PHYCOCYANIN (PROBE)',
                    'PH',
                    'TEMPERATURE, WATER')])
dss1$sonde<-"dss1"
dss2$sonde<-"dss2"
dss<-merge(dss1,dss2,all=TRUE)
dss<-dss %>% 
  gather(Characteristic.Name,Result.Value,-SAMPLE_DATE,-TIME,-SAMPLE_NAME,-Depth,-X_Coordinate,-Y_Coordinate,-sonde) %>% 
  mutate(Result.Unit=NA,
         Result.Unit=ifelse(Characteristic.Name=="CHLOROPHYLL A (PROBE)","ug/L",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="SPECIFIC CONDUCTANCE","uS/cm",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="DISSOLVED OXYGEN SATURATION","%",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="DISSOLVED OXYGEN (DO)","mg/L",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="OXIDATION REDUCTION POTENTIAL (ORP)","mV",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="PHYCOCYANIN (PROBE)","ug/L",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="PH","pH units",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="TEMPERATURE, WATER","deg C",Result.Unit),
         Result.Value=ifelse(Characteristic.Name=="TEMPERATURE, WATER",((Result.Value-32)*(5/9)),Result.Value))


#############################################################################################
#exo
exo<-read.csv("2019.profile.data/LMAS_Exo_2019_cleaned.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
names(exo)[names(exo)=="Date..MM.DD.YYYY."] <- "SAMPLE_DATE"
names(exo)[names(exo)=="Time..HH.mm.ss."] <- "TIME"
names(exo)[names(exo)=="Site.Name"] <- "SAMPLE_NAME"
names(exo)[names(exo)=="Depth.m"] <- "Depth"
names(exo)[names(exo)=="GPS.Longitude.."] <- "X_Coordinate"
names(exo)[names(exo)=="GPS.Latitude.."] <- "Y_Coordinate"
names(exo)[names(exo)=="Chlorophyll.RFU"] <- "CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)"
names(exo)[names(exo)=="Chlorophyll.ug.L"] <- "CHLOROPHYLL A (PROBE)"
names(exo)[names(exo)=="SpCond.µS.cm"] <- "SPECIFIC CONDUCTANCE"
names(exo)[names(exo)=="ODO...sat"] <- "DISSOLVED OXYGEN SATURATION"
names(exo)[names(exo)=="ODO.mg.L"] <- "DISSOLVED OXYGEN (DO)"
names(exo)[names(exo)=="ORP.mV"] <- "OXIDATION REDUCTION POTENTIAL (ORP)"
names(exo)[names(exo)=="BGA.PC.RFU"] <- "PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)"
names(exo)[names(exo)=="BGA.PC.ug.L"] <- "PHYCOCYANIN (PROBE)"
names(exo)[names(exo)=="Turbidity.FNU"] <- "TURBIDITY"
names(exo)[names(exo)=="pH"] <- "PH"
names(exo)[names(exo)=="Temp..C"] <- "TEMPERATURE, WATER"
exo<-unique(exo[c('SAMPLE_DATE','TIME','SAMPLE_NAME','Depth','X_Coordinate','Y_Coordinate',
                  'CHLOROPHYLL A (PROBE RELATIVE FLUORESCENCE)',
                  'CHLOROPHYLL A (PROBE)',
                  'SPECIFIC CONDUCTANCE',
                  'DISSOLVED OXYGEN SATURATION',
                  'DISSOLVED OXYGEN (DO)',
                  'OXIDATION REDUCTION POTENTIAL (ORP)',
                  'PHYCOCYANIN (PROBE RELATIVE FLUORESCENCE)',
                  'PHYCOCYANIN (PROBE)',
                  'TURBIDITY',
                  'PH',
                  'TEMPERATURE, WATER')])
exo<-exo %>% 
  gather(Characteristic.Name,Result.Value,-SAMPLE_DATE,-TIME,-SAMPLE_NAME,-Depth,-X_Coordinate,-Y_Coordinate) %>% 
  mutate(Result.Unit=NA,
         Result.Unit=ifelse(Characteristic.Name=="CHLOROPHYLL A (PROBE)","ug/L",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="SPECIFIC CONDUCTANCE","uS/cm",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="DISSOLVED OXYGEN SATURATION","%",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="DISSOLVED OXYGEN (DO)","mg/L",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="OXIDATION REDUCTION POTENTIAL (ORP)","mV",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="PHYCOCYANIN (PROBE)","ug/L",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="PH","pH units",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="TEMPERATURE, WATER","deg C",Result.Unit),
         Result.Unit=ifelse(Characteristic.Name=="TURBIDITY","FNU",Result.Unit))
exo$sonde<-"exo"
#############################################################################################
profile<-merge(exo,dss,all=TRUE)
rm(list=c('exo','dss','dss1','dss2'))
#remove degree sign and white space
profile$X_Coordinate<-gsub(" °","",profile$X_Coordinate)
profile$Y_Coordinate<-gsub(" °","",profile$Y_Coordinate)
profile$X_Coordinate<-trimws(profile$X_Coordinate)
profile$Y_Coordinate<-trimws(profile$Y_Coordinate)
profile$SAMPLE_NAME<-ifelse(profile$SAMPLE_NAME=="CSLAPBIGBOW","19-235-99",profile$SAMPLE_NAME)
profile<-profile %>% 
  filter(!SAMPLE_NAME %in% c('FLOATWETPKS','WP'))

#############################################################################################

#plotting all the profiles
profile<-profile %>% 
#  filter(SAMPLE_NAME %in% c("19STL043","19LCB119","19ONE301D","19WPLB01","19WPLD01","19WPLA103")) %>% 
  arrange(sonde,SAMPLE_DATE,Characteristic.Name)

  rmarkdown::render('2019.profile.data/merging.profiles.Rmd')
