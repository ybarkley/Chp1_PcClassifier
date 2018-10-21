## Sept 14, 2018

pcdata<- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles/ROCCA Towed Data/BigRoccaFile_PMN_TowedAll_EDIT_20180816.csv')

pcdata$EncounterID = as.character(pcdata$EncounterID)
pcdata$EncounterID[pcdata$EncounterID == 'PcMHI31'] = "M2"
pcdata$EncounterID[pcdata$EncounterID == 'PcMHI32'] = "M3"
pcdata$EncounterID[pcdata$EncounterID == 'PcMHI33'] = "M4"
pcdata$EncounterID[pcdata$EncounterID == 'PcMHI34'] = "M1"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL1'] = "P6"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL2'] = "P1"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL3'] = "P2"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL4'] = "P4"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL5'] = "P5"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL6'] = "P3"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL7'] = "P8"
pcdata$EncounterID[pcdata$EncounterID == 'PcPEL8'] = "P7"
pcdata$EncounterID[pcdata$EncounterID == 'PcNWHI14'] = "N2"
pcdata$EncounterID[pcdata$EncounterID == 'PcNWHI15'] = "N3"
pcdata$EncounterID[pcdata$EncounterID == 'PcNWHI16'] = "N4"
pcdata$EncounterID[pcdata$EncounterID == 'PcNWHI17'] = "N1"
pcdata$EncounterID = as.factor(pcdata$EncounterID)

pcdata$population = as.character(pcdata$population)
pcdata$population[pcdata$population == 'mhi'] = "MHI"
pcdata$population[pcdata$population == 'pel'] = "Pelagic"
pcdata$population[pcdata$population == 'nwhi'] = "NWHI"
pcdata$population = as.factor(pcdata$population)




# pcdata$population = with(pcdata, factor(population, levels = rev(levels(population))))
# levels(pcdata$EncounterID) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "N1", "N2", "N3", "N4", "M1", "M2", "M3", "M4" )
write.csv(pcdata, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles//ROCCA Towed Data/BigRoccaFile_PMN_TowedAll_EDIT_20180914.csv', row.names = FALSE)




## June 9, 2017
## These are the steps taken to format the whistle data set for the Pseudorca whistles
## After a conversation with Julie, the MHI encounters were renumbered to consolidate Enc 25 & 26 and 28 & 29 because they are from the same encounter.
## This stems from the PcMaster_DataManipulation20160426 file.


# Pc_All<-read.csv("PcClass.off.nwhi.mhi.csv")
# Pc_PMRF<-read.csv("PcPMRF_Alledit.csv")
# Pc_MHI<-read.csv("PcMHI_Alledit.csv")
#
# #Subset Pc_All in order to only extract Offshore and NWHI data
# Off_Pc<-subset(Pc_All, Pc_All$SpeciesCode==0) #subsets all rows for correct Offshore subpopulation detections
# NWHI_Pc<-subset(Pc_All, Pc_All$SpeciesCode==2) #subsets all rows for original 5 NWHI subpopulation detections
#
# #Reorder columns so they match with PcPMRF_all data
# Off_Pc<-Off_Pc[, c(1,3,5,2,4, 6:60)]
# NWHI_Pc<-NWHI_Pc[, c(1,3,5,2,4, 6:60)]
# #change offshore code from 0 to 1.


#July 22, 2018
#Combining HARP and Fostex MHI data into 2 separate encounters
library(dplyr) #ilter fxn

pc_fostex <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles/ROCCA Data_ProbNotUsing/BIG ROCCA FILE.csv')
pc_fostex <- filter(pc_fostex, population == 'mhi' & group < 30)
pc_fostex <- pc_fostex[, -c(1)]
write.csv(pc_fostex, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles/ROCCA Data_ProbNotUsing/pc_fostex.csv', row.names = FALSE)

pc_harp <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles/ROCCA Data_ProbNotUsing/pc_harp.csv')
pc_harp <- filter(pc_harp, population == 'MHI' & ClassifiedSpecies == 'HARP')

pc_harp$group[pc_harp$EncounterID=="MHI31"]<-"31"
pc_harp$group[pc_harp$EncounterID=="MHI32"]<-"32"
pc_harp$group[pc_harp$EncounterID=="MHI33"]<-"33"
pc_harp$group[pc_harp$EncounterID=="MHI34"]<-"34"
pc_harp <- pc_harp[, -c(1)]
write.csv(pc_harp, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles/ROCCA Data_ProbNotUsing/pc_harp.csv', row.names = FALSE)


#####
Pc_Pred<-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1005.csv')

# Changes made 6/09/17
# Renumber group codes and id's for MHI only. NWHI and Pel stay the same.
Pc_Pred$group[Pc_Pred$group=="25"]<-"26"
Pc_Pred$id[Pc_Pred$id=="mhi25"]<-"mhi26"

Pc_Pred$group[Pc_Pred$group=="29"]<-"28"
Pc_Pred$id[Pc_Pred$id=="mhi29"]<-"mhi28"

Pc_Pred$group[Pc_Pred$group=="30"]<-"29"
Pc_Pred$id[Pc_Pred$id=="mhi30"]<-"mhi29"

Pc_Pred$group[Pc_Pred$group=="31"]<-"30"
Pc_Pred$id[Pc_Pred$id=="mhi31"]<-"mhi30"

Pc_Pred$group[Pc_Pred$group=="32"]<-"31"
Pc_Pred$id[Pc_Pred$id=="mhi32"]<-"mhi31"

Pc_Pred$group[Pc_Pred$group=="33"]<-"32"
Pc_Pred$id[Pc_Pred$id=="mhi33"]<-"mhi32"

Pc_Pred$group[Pc_Pred$group=="34"]<-"33"  
Pc_Pred$id[Pc_Pred$id=="mhi34"]<-"mhi33"

Pc_Pred$group[Pc_Pred$group=="35"]<-"34"
Pc_Pred$id[Pc_Pred$id=="mhi35"]<-"mhi34"

Pc_Pred$group[Pc_Pred$group=="36"]<-"35"
Pc_Pred$id[Pc_Pred$id=="mhi36"]<-"mhi35"

Pc_Pred$group[Pc_Pred$group=="37"]<-"36" #### Oct 5, 2017: Lumped mhi36 and mhi37 together, both Pc17 (& Pc13)
Pc_Pred$id[Pc_Pred$id=="mhi37"]<-"mhi36"

Pc_Pred$group[Pc_Pred$group=="38"]<-"37"
Pc_Pred$id[Pc_Pred$id=="mhi38"]<-"mhi37"

Pc_Pred$group[Pc_Pred$group=="20"]<-"19"
Pc_Pred$id[Pc_Pred$id=="nwhi20"]<-"nwhi19"

# Changes made 6/11/17
# Adding column for recorder type and bft again
#Input Bft values for each group
#offshore
Pc_Pred$Bft[Pc_Pred$group==1]<-'4'
Pc_Pred$Bft[Pc_Pred$group==2]<-'4'
Pc_Pred$Bft[Pc_Pred$group==3]<-'4'
Pc_Pred$Bft[Pc_Pred$group==4]<-'3'
Pc_Pred$Bft[Pc_Pred$group==5]<-'4'
Pc_Pred$Bft[Pc_Pred$group==6]<-'3'
Pc_Pred$Bft[Pc_Pred$group==7]<-'4'

##NWHI
Pc_Pred$Bft[Pc_Pred$group==14]<-"3"
Pc_Pred$Bft[Pc_Pred$group==15]<-"3"
Pc_Pred$Bft[Pc_Pred$group==16]<-"1"
Pc_Pred$Bft[Pc_Pred$group==17]<-"2"
Pc_Pred$Bft[Pc_Pred$group==18]<-"3"
Pc_Pred$Bft[Pc_Pred$group==19]<-"NA"
Pc_Pred$Bft[Pc_Pred$group==20]<-"NA"
Pc_Pred$Bft[Pc_Pred$group==21]<-"NA"
Pc_Pred$Bft[Pc_Pred$group==22]<-"NA"
Pc_Pred$Bft[Pc_Pred$group==23]<-"NA"
Pc_Pred$Bft[Pc_Pred$group==24]<-"NA"

#MHI
Pc_Pred$Bft[Pc_Pred$group==26]<-'1'
Pc_Pred$Bft[Pc_Pred$group==27]<-'2'
Pc_Pred$Bft[Pc_Pred$group==28]<-'2'
Pc_Pred$Bft[Pc_Pred$group==29]<-'2'
Pc_Pred$Bft[Pc_Pred$group==30]<-'1'
Pc_Pred$Bft[Pc_Pred$group==31]<-'6'


Pc_Pred$Recorder<-""
Pc_Pred$Recorder[1:600]<-'array'
Pc_Pred$Recorder[601:1250]<-'pmrf'
Pc_Pred$Recorder[1251:1550]<-'fostex'
Pc_Pred$Recorder[1551:1600]<-'array'
Pc_Pred$Recorder[1601:1900]<-'harp'

write.csv(Pc_Pred, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1005.csv')



################################################################################################
## Sept 22, 2017
## Add Pelagic Encounter 8 to the master list

pel8<-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\PcOFF_1303A39S20\\PcOFF_A39S20_ROCCA\\PcOFF_A39S20_RoccaContourStats.csv')
#master <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-0818.csv')
write.csv(pel8, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\PcOFF_1303A39S20\\PcOFF_A39S20_ROCCA\\PcOFF_A39S20_RoccContourstatsRAW.csv')

pel8<- pel8[, -c(1:3, 6:13, 20,21,33:36,68:100)]

pel8$EncounterCount<-1:nrow(pel8)
pel8$EncounterNumber<-"1303_A39_S20" 

#Rename EncounterNumber and EncounterCount
names(pel8)[1] <- "whistle_count"
names(pel8)[2] <- "cruise_ac_vis"

#Add population column
pel8$population <- "pelagic"

#Add group column
pel8$group <- "8"

#Add id column
pel8$id <- "pel8"

#Add Bft column
pel8$Bft <- "4"

#Add Recorder column
pel8$Recorder <- "array"

#REORDER
pel8 <- pel8[, c(2, 51:53, 1, 54:55, 3:50)]

write.csv(pel8, "C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\PcOFF_1303A39S20\\PcOFF_A39S20_ROCCA\\PcOFF_A39S20_RoccaContourStats.csv")


new <- rbind(master, pel8)
write.csv(new, "C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-0922.csv", row.names = F)

################################################################################################
## Oct 3, 2017
## Redo MHI Encounter 30 due to outlying FREQPOSLOPEMEAN values, then add to the master list.
## DID NOT SAVE A RAW FILE, OOPS.
mhi30redo<-read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\PcMHI_Enc07_20101219_mhi30\\PcMHI_Enc07_ROCCA_REDO\\PcMHI_Enc07_20101219REDO_RoccaContourStats.csv')
#master <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-0922.csv')


mhi30redo<- mhi30redo[, -c(1:3, 6:13, 20,21,33:36,68:100)]

mhi30redo$EncounterCount<-1:nrow(mhi30redo)
mhi30redo$EncounterNumber<-"PcMHI_Enc07_20091219" 

#Rename EncounterNumber and EncounterCount
names(mhi30redo)[1] <- "whistle_count"
names(mhi30redo)[2] <- "cruise_ac_vis"

#Add population column
mhi30redo$population <- "mhi"

#Add group column
mhi30redo$group <- "30"

#Add id column
mhi30redo$id <- "mhi30redo"


#Add Bft column
mhi30redo$Bft <- "2"

#Add Recorder column
mhi30redo$Recorder <- "fostex"

#REORDER
mhi30redo <- mhi30redo[, c(2, 51:53, 1, 54:55, 3:50)]

write.csv(mhi30redo, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\PcMHI_Enc07_20101219_mhi30\\PcMHI_Enc07_ROCCA_REDO\\PcMHI_Enc07_20101219REDO_RoccaContourStats.csv', row.names=F)
master <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-0922.csv')

master=new[-c(1501:1550), ]

new <- rbind(master, mhi30redo)



write.csv(new, "C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1003.csv", row.names = F)


################################
### Changes October 5, 2017 #### After meeting with EMO, decide to lump mhi33 & mhi34 and mhi36 & mhi37 and nwhi14 & nwhi15 ####

Pc_Pred <- read.csv("C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1003.csv")

Pc_Pred$group[Pc_Pred$group=="34"]<-"33"  
Pc_Pred$id[Pc_Pred$id=="mhi34"]<-"mhi33"

Pc_Pred$group[Pc_Pred$group=="37"]<-"36"  
Pc_Pred$id[Pc_Pred$id=="mhi37"]<-"mhi36"

Pc_Pred$group[Pc_Pred$group=="15"]<-"14"
Pc_Pred$id[Pc_Pred$id=="nwhi15"]<-"nwhi14"



Pc_Pred$id = droplevels(Pc_Pred$id)


write.csv(Pc_Pred,'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1005.csv', row.names = F)

################################
### Changes to NWHI encounter numbering. Combining all PMRF whistles into a single encounter ####
pcdata=Pc_Pred

Pc_Pred$group[Pc_Pred$group == '21'] <-'19'
Pc_Pred$group[Pc_Pred$group == '22'] <-'19'
Pc_Pred$group[Pc_Pred$group == '23'] <-'19'
Pc_Pred$group[Pc_Pred$group == '24'] <-'19'

Pc_Pred$id[Pc_Pred$id == 'nwhi21'] <-'nwhi19'
Pc_Pred$id[Pc_Pred$id == 'nwhi22'] <-'nwhi19'
Pc_Pred$id[Pc_Pred$id == 'nwhi23'] <-'nwhi19'
Pc_Pred$id[Pc_Pred$id == 'nwhi24'] <-'nwhi19'

write.csv(Pc_Pred, "C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1007.csv", row.names = F)

### Changes: Oct 16, 2016 ####
# Changed encounter ID to capital letters to match poster labels
Pc_Pred <- read.csv("C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1007.csv", stringsAsFactors = F)

Pc_Pred$id[Pc_Pred$id == 'nwhi14'] <-"NWHI14"
Pc_Pred$id[Pc_Pred$id == 'nwhi16'] <-'NWHI15'
Pc_Pred$id[Pc_Pred$id == 'nwhi17'] <-'NWHI16'
Pc_Pred$id[Pc_Pred$id == 'nwhi18'] <-'NWHI17'
Pc_Pred$id[Pc_Pred$id == 'nwhi19'] <-'NWHI18'

Pc_Pred$id[Pc_Pred$id == 'mhi26'] <-"MHI26"
Pc_Pred$id[Pc_Pred$id == 'mhi28'] <-'MHI27'
Pc_Pred$id[Pc_Pred$id == 'mhi29'] <-'MHI28'
Pc_Pred$id[Pc_Pred$id == 'mhi30'] <-'MHI29'
Pc_Pred$id[Pc_Pred$id == 'mhi31'] <-'MHI30'
Pc_Pred$id[Pc_Pred$id == 'mhi32'] <-'MHI31'
Pc_Pred$id[Pc_Pred$id == 'mhi33'] <-'MHI32'
Pc_Pred$id[Pc_Pred$id == 'mhi35'] <-'MHI33'
Pc_Pred$id[Pc_Pred$id == 'mhi36'] <-'MHI34'

Pc_Pred$id[Pc_Pred$id == 'pel1'] <-"PEL1"
Pc_Pred$id[Pc_Pred$id == 'pel2'] <-'PEL2'
Pc_Pred$id[Pc_Pred$id == 'pel3'] <-'PEL3'
Pc_Pred$id[Pc_Pred$id == 'pel4'] <-'PEL4'
Pc_Pred$id[Pc_Pred$id == 'pel5'] <-'PEL5'
Pc_Pred$id[Pc_Pred$id == 'pel6'] <-'PEL6'
Pc_Pred$id[Pc_Pred$id == 'pel7'] <-'PEL7'
Pc_Pred$id[Pc_Pred$id == 'pel8'] <-'PEL8'

Pc_Pred$population[Pc_Pred$population == 'mhi'] <-'MHI'
Pc_Pred$population[Pc_Pred$population == 'nwhi'] <-'NWHI'
Pc_Pred$population[Pc_Pred$population == 'pelagic'] <-'PEL'

Pc_Pred$Recorder[Pc_Pred$Recorder == 'array'] <-'Towed Array'
Pc_Pred$Recorder[Pc_Pred$Recorder == 'fostex'] <-'Fostex'
Pc_Pred$Recorder[Pc_Pred$Recorder == 'harp'] <-'HARP'
Pc_Pred$Recorder[Pc_Pred$Recorder == 'pmrf'] <-'PMRF'

write.csv(Pc_Pred, "C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\Pc_MasterListEdit-1016labs.csv")












#Renumber group codes for Offshore only. NWHI stays as is.
# Off_Pc$group[Off_Pc$group=="8"]<-"1"
# Off_Pc$group[Off_Pc$group=="9"]<-"2"
# Off_Pc$group[Off_Pc$group=="10"]<-"3"
# Off_Pc$group[Off_Pc$group=="11"]<-"4"
# Off_Pc$group[Off_Pc$group=="12"]<-"5"
# Off_Pc$group[Off_Pc$group=="13"]<-"6"
# Off_Pc$group[Off_Pc$group=="88"]<-"7"
#Combine all datasets for all three subpopulations into one dataframe
# Pc_All<-rbind(Off_Pc, NWHI_Pc, Pc_PMRF, Pc_MHI)
# 
# #Convert group column from character to numeric
# #Pc<-transform(Pc_All, group=as.numeric(group))
# 
# #Order the data by the Species Code
# Pc_All<-Pc_All[order(Pc_All$SpeciesCode),] 
# 
# Pc_All$Index<-1:nrow(Pc_All)#Renumbers Index column 
# 
# write.csv(Pc_All, "Pc_MasterList.csv", row.names = F)
# 
# ## Add in predictor columns to Pc_MasterList.csv
# #Read in Pc_MasterList.csv file
# Pc_Pred<-read.csv("Pc_MasterList.csv")
# 
# #Input Lat/Lon
# ##Offshore
# Pc_Pred$Lat[Pc_Pred$group==1]<-'19.2856' #1203_A186_S76
# Pc_Pred$Lon[Pc_Pred$group==1]<-'-157.3481' 
# Pc_Pred$Lat[Pc_Pred$group==2]<-'28.9011' #1641_A71_S35
# Pc_Pred$Lon[Pc_Pred$group==2]<-'-176.9941'
# Pc_Pred$Lat[Pc_Pred$group==3]<-'29.2329' #1641_A83_S47
# Pc_Pred$Lon[Pc_Pred$group==3]<-'-174.8820'
# Pc_Pred$Lat[Pc_Pred$group==4]<-'29.6430' #1641_A116_S74
# Pc_Pred$Lon[Pc_Pred$group==4]<-'-173.2723'
# Pc_Pred$Lat[Pc_Pred$group==5]<-'26.7177' #1641_A325_S241
# Pc_Pred$Lon[Pc_Pred$group==5]<-'179.0239'
# Pc_Pred$Lat[Pc_Pred$group==6]<-'31.3668' #1641_A98_S61
# Pc_Pred$Lon[Pc_Pred$group==6]<-'-177.2191'
# Pc_Pred$Lat[Pc_Pred$group==7]<-'25.362' #1303_A88_S59
# Pc_Pred$Lon[Pc_Pred$group==7]<-'-172.237'
# 
# #NWHI
# Pc_Pred$Lat[Pc_Pred$group==14]<-'24.4720'
# Pc_Pred$Lon[Pc_Pred$group==14]<-'-168.6277'
# Pc_Pred$Lat[Pc_Pred$group==15]<-'24.5352'
# Pc_Pred$Lon[Pc_Pred$group==15]<-'-168.6704'
# Pc_Pred$Lat[Pc_Pred$group==16]<-'23.4578'
# Pc_Pred$Lon[Pc_Pred$group==16]<-'-162.2288'
# Pc_Pred$Lat[Pc_Pred$group==17]<-'23.1770'
# Pc_Pred$Lon[Pc_Pred$group==17]<-'-162.0642'
# Pc_Pred$Lat[Pc_Pred$group==18]<-'23.0163'
# Pc_Pred$Lon[Pc_Pred$group==18]<-'-162.5203'
# Pc_Pred$Lat[Pc_Pred$group==19]<-'22.085'
# Pc_Pred$Lon[Pc_Pred$group==19]<-'-159.868'
# Pc_Pred$Lat[Pc_Pred$group==20]<-'22.157'
# Pc_Pred$Lon[Pc_Pred$group==20]<-'-159.879'
# Pc_Pred$Lat[Pc_Pred$group==21]<-'22.237'
# Pc_Pred$Lon[Pc_Pred$group==21]<-'-159.954'
# Pc_Pred$Lat[Pc_Pred$group==22]<-'22.284'
# Pc_Pred$Lon[Pc_Pred$group==22]<-'-159.963'
# Pc_Pred$Lat[Pc_Pred$group==23]<-'22.125975'
# Pc_Pred$Lon[Pc_Pred$group==23]<-'-159.897757'
# Pc_Pred$Lat[Pc_Pred$group==24]<-'22.107'
# Pc_Pred$Lon[Pc_Pred$group==24]<-'-159.895'
# 
# #MHI
# Pc_Pred$Lat[Pc_Pred$group==25]<-'19.47028'
# Pc_Pred$Lon[Pc_Pred$group==25]<-'-155.96542'
# Pc_Pred$Lat[Pc_Pred$group==26]<-'19.47028'
# Pc_Pred$Lon[Pc_Pred$group==26]<-'-155.96542'
# Pc_Pred$Lat[Pc_Pred$group==27]<-'21.21321'
# Pc_Pred$Lon[Pc_Pred$group==27]<-'-158.11572'
# Pc_Pred$Lat[Pc_Pred$group==28]<-'19.74301'
# Pc_Pred$Lon[Pc_Pred$group==28]<-'-156.49725'
# Pc_Pred$Lat[Pc_Pred$group==29]<-'19.74301'
# Pc_Pred$Lon[Pc_Pred$group==29]<-'-156.49725'
# Pc_Pred$Lat[Pc_Pred$group==30]<-'19.21355'
# Pc_Pred$Lon[Pc_Pred$group==30]<-'-156.01451'
# Pc_Pred$Lat[Pc_Pred$group==31]<-'19.58045'
# Pc_Pred$Lon[Pc_Pred$group==31]<-'-156.1171'
# Pc_Pred$Lat[Pc_Pred$group==32]<-'21.5037'
# Pc_Pred$Lon[Pc_Pred$group==32]<-'-157.592 '
# Pc_Pred$Lat[Pc_Pred$group==33]<-'19.574'
# Pc_Pred$Lon[Pc_Pred$group==33]<-'-156.014'
# Pc_Pred$Lat[Pc_Pred$group==34]<-'21.98'
# Pc_Pred$Lon[Pc_Pred$group==34]<-'-159.893'
# Pc_Pred$Lat[Pc_Pred$group==35]<-'21.937'
# Pc_Pred$Lon[Pc_Pred$group==35]<-'-159.864'
# Pc_Pred$Lat[Pc_Pred$group==36]<-'21.932'
# Pc_Pred$Lon[Pc_Pred$group==36]<-'-159.881'
# Pc_Pred$Lat[Pc_Pred$group==37]<-'22.013'
# Pc_Pred$Lon[Pc_Pred$group==37]<-'-160.069'
# Pc_Pred$Lat[Pc_Pred$group==38]<-'21.913'
# Pc_Pred$Lon[Pc_Pred$group==38]<-'-159.908'
# 
# 
# 
# #Input known group sizes for detections
# #
# ##Offshore 
# Pc_Pred$GroupSize[Pc_Pred$group==2]<-'22.6'
# Pc_Pred$GroupSize[Pc_Pred$group==3]<-'10.3'
# Pc_Pred$GroupSize[Pc_Pred$group==4]<-'18.3'
# Pc_Pred$GroupSize[Pc_Pred$group==5]<-'41'
# Pc_Pred$GroupSize[Pc_Pred$group==6]<-'29.9'
# 
# ##NWHI
# Pc_Pred$GroupSize[Pc_Pred$group==14]<-"12.1"
# Pc_Pred$GroupSize[Pc_Pred$group==15]<-"1.7"
# Pc_Pred$GroupSize[Pc_Pred$group==16]<-"8.8"
# Pc_Pred$GroupSize[Pc_Pred$group==17]<-"20.4"
# Pc_Pred$GroupSize[Pc_Pred$group==18]<-"52"
# 
# #MHI
# Pc_Pred$GroupSize[Pc_Pred$group==25]<-"35"
# Pc_Pred$GroupSize[Pc_Pred$group==26]<-"35"
# Pc_Pred$GroupSize[Pc_Pred$group==27]<-"28"
# Pc_Pred$GroupSize[Pc_Pred$group==28]<-"13"
# Pc_Pred$GroupSize[Pc_Pred$group==29]<-"13"
# Pc_Pred$GroupSize[Pc_Pred$group==30]<-"21"
# Pc_Pred$GroupSize[Pc_Pred$group==31]<-"14"
# 
# 
# 
# 

# 
# #Reorder new columns to be at the beginning of dataframe
# Pc_Pred<-Pc_Pred[, c(1:6, 61:65, 7:60)]
# 
# Pc_Pred$SpeciesCode[Pc_Pred$SpeciesCode==0]<-'off'
# Pc_Pred$SpeciesCode[Pc_Pred$SpeciesCode==2]<-'nwhi'
# Pc_Pred$SpeciesCode[Pc_Pred$SpeciesCode==2]<-'nwhi'
# Pc_Pred$SpeciesCode[Pc_Pred$SpeciesCode==3]<-'mhi'
# 
# 
# write.csv(Pc_Pred, "Pc_MasterListwPredictors.csv", row.names = F)