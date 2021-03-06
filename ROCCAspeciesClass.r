## 9/24/18

#Loooking at the original ROCCA classification from species classifier to evaluate how well the whistles were initially identified as Pc 
library(dplyr)
pcdata <- read.csv('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data/2017- 100 New Whistles\\ROCCA Towed Data\\BigRoccaFile_PMN_TowedAll_EDIT_20180914.csv')
ROCCAclass <-pcdata[, c(1,3,6)]
ROCCAclass_EncID <- ROCCAclass %>% group_by(EncounterID, ClassifiedSpecies) %>% tally()
ROCCAclass_Pop <- ROCCAclass %>% group_by(population, ClassifiedSpecies) %>% tally()

ROCCAclass_Pop$population2 <- as.numeric(ROCCAclass_Pop$population)


#Clunky way of calculating the percentage of whistles classied as each species for each population
#use ifelse to handle multiple logicals, aka handles vectorized inputs
ROCCAclass_Pop <- within(ROCCAclass_Pop, {
  percent = ifelse(ROCCAclass_Pop$population2 == 1, ROCCAclass_Pop$n/600*100,ROCCAclass_Pop$n/650*100)
  percent2 = ifelse(ROCCAclass_Pop$population2 == 3, ROCCAclass_Pop$n/1200*100, 'NA')
                    
})


write.csv(ROCCAclass_Pop, "C:\\Users\\Yvonne\\OneDrive\\PHD\\CHP1-FKW\\data\\results\\2018\\ROCCA_InitialClassification_byPop.csv")
write.csv(ROCCAclass_EncID, "C:\\Users\\Yvonne\\OneDrive\\PHD\\CHP1-FKW\\data\\results\\2018\\ROCCA_InitialClassification_byEnc.csv")


#aggregate(ROCCAclass_Pop$n, by=list(Category=ROCCAclass_Pop$population), FUN=sum)
ROCCA_Pop <- read.csv("C:\\Users\\Yvonne\\OneDrive\\PHD\\CHP1-FKW\\data\\results\\2018\\ROCCA_InitialClassification_byPop.csv")
ROCCA_Pop<- ROCCA_Pop[, -1]

ROCCA_Pop$ClassifiedSpecies <- as.character(ROCCA_Pop$ClassifiedSpecies)
ROCCA_Pop$ClassifiedSpecies[ROCCA_Pop$ClassifiedSpecies=='Ambig'] = 'Ambiguous'
ROCCA_Pop$ClassifiedSpecies[ROCCA_Pop$ClassifiedSpecies=='Gm'] = 'Pilot Whale'
ROCCA_Pop$ClassifiedSpecies[ROCCA_Pop$ClassifiedSpecies=='Dc_Dd'] = 'Common Dolphin'
ROCCA_Pop$ClassifiedSpecies[ROCCA_Pop$ClassifiedSpecies=='Pc'] = 'False Killer Whale'
ROCCA_Pop$ClassifiedSpecies[ROCCA_Pop$ClassifiedSpecies=='Sb'] = 'Rough-toothed Dolphin'
ROCCA_Pop$ClassifiedSpecies[ROCCA_Pop$ClassifiedSpecies=='Sc'] = 'Striped Dolphin'
ROCCA_Pop$ClassifiedSpecies <- as.factor(ROCCA_Pop$ClassifiedSpecies)

ROCCA_Pop$population <-factor(ROCCA_Pop$population, levels=c("Pelagic", 'NWHI', 'MHI'), ordered = T)

bar <- ggplot(ROCCA_Pop, aes(x=population, y=percent, fill = factor(ClassifiedSpecies))) + geom_bar(stat = "identity") +
  theme_bw()+ 
  theme(plot.background = element_rect(fill = "white"),
        axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        plot.title=element_text(size=15, hjust = 0.5, face = 'bold')) +
  guides(fill=guide_legend(title="Classified Species"))+
  labs(x="Population", y="Percentage", title = "Initial Species Classification by ROCCA") +
  scale_fill_grey(start = 0.05, end = .85)

ggsave("C:\\Users\\Yvonne\\OneDrive\\PHD\\CHP1-FKW\\data\\results\\2018\\plots\\ROCCA_InitialClass.png", width = 10, height = 8, units = "in", type = "cairo-png")

###########


pcdata.pel <- droplevels(subset(pcdata.pm, population == 'pel'))
pcdata.mhi <- droplevels(subset(pcdata.pm, population == 'mhi'))



#PIFSC desktop
pcdata<- read.csv('C:\\Users\\yvonne.barkley\\Documents\\PHD\\BigRoccaFile_PMN_TowedAll_EDIT_20180914.csv')

pcdata_sub <- pcdata[, c(1,3,7,13,15,16,21,22,24)]

pcdata.pel <- droplevels(subset(pcdata, population == 'Pelagic'))
pcdata.nwhi <- droplevels(subset(pcdata, population == 'NWHI'))
pcdata.mhi <- droplevels(subset(pcdata, population == 'MHI'))

#Calculate means for 7 variables that significantly differed according to Dunn's test
means <- aggregate(pcdata_sub[,3:9], list(pcdata_sub$population), mean)

# Calculate standard error for 7 variables that significantly differed according to Dunn's test
se <- function(x) sd(x)/sqrt(length(x))
standErrors <- aggregate(pcdata_sub[,3:9], list(pcdata_sub$population), se)
standErrorsA <- round(standErrors[,c(2:7)],2)
standErrorsB <- data.frame(round(standErrors[,c(8)],4))

standErrors <- data.frame(cbind(standErrors[,1], standErrorsA, standErrorsB))

# means.pel<-as.data.frame(colMeans(pcdata.pel[, 7, 13,15,16,21,22,24]))
# means.nwhi<-as.data.frame(colMeans(pcdata.nwhi[, 7:54]))
# means.mhi<-as.data.frame(colMeans(pcdata.mhi[, 7:54]))

means <- cbind("PEL" = means.pel[,1], "NWHI" = means.nwhi, "MHI" = means.mhi)

df_mhi = droplevels(subset(pcplot, pcplot$population=='mhi'))
posslope_mhi = mean(df_mhi$FREQPOSSLOPEMEAN)
plot(as.factor(df_mhi$group), df_mhi$FREQPOSSLOPEMEAN, xlab="Encounter ID", ylab="Mean Positive Slope", main="Main Hawaiian Island Population")
plot(as.factor(df_mhi$group), df_mhi$FREQQUARTER3, xlab="Encounter ID", ylab="Mean Positive Slope", main="Main Hawaiian Island Population")
plot(as.factor(df_mhi$group), df_mhi$FREQSWEEPFLATPERCENT, xlab="Encounter ID", ylab="% of the whistle with zero slope ", main="Main Hawaiian Island Population")

plot(pcdata.pel$FREQSPREAD, pcdata.mhi$FREQSPREAD)
plot(pcdata.mhi$DURATION)
poop <- lm(DURATION ~ population, pcdata.mhi)


anova(poop)

#standard deviation of a variable for each encounter?

varsOnly <- pcdata.pm[, c(1,3, 7:54)]



library(data.table)
vars.dt <-data.table(varsOnly)
df3<-setDF(vars.dt[, as.list(unlist(lapply(.SD, function(x) list(mean=mean(x),median=median(x), sd=sd(x), IQR=IQR(x), quantile(x, 0.25, type=2), quantile(x, 0.75, type=2), range=range(x))))),
          by="EncounterID", .SDcols=colnames(varsOnly[,3:48])])
population<- c('mhi','mhi','mhi','mhi','mhi','nwhi','nwhi','nwhi','nwhi','pel','pel','pel','pel','pel','pel','pel','pel')
df3 <- cbind(population, df3)

df.mean_sd <- setDF(vars.dt[, as.list(unlist(lapply(.SD, function(x) list(mean=mean(x), sd=sd(x))))), by="EncounterID", .SDcols=colnames(varsOnly[,2:48])])

df.median_IQR <- setDF(vars.dt[, as.list(unlist(lapply(.SD, function(x) list(median=median(x), IQR=IQR(x), quantile(x, 0.25, type=2), quantile(x, 0.75, type=2), range=range(x))))), by="EncounterID", .SDcols=varsOnly[,2:48]])
                          
                                



