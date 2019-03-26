
#Load HUGE data set
whistles <- read.csv('data/results/PcResults_TotalWhistleClass_PMN_20181209.csv')
whistles <- whistles[, c(1,3:10)]

whistles.m <- filter(whistles, EncounterID == 'M1' | EncounterID == 'M2' | EncounterID == 'M3' | EncounterID == 'M4')

whistles.n <- filter(whistles, EncounterID == 'N1' | EncounterID == 'N2' | EncounterID == 'N3' | EncounterID == 'N4')

whistles.P <- filter(whistles, EncounterID == 'P1' | EncounterID == 'P2' | EncounterID == 'P3' | EncounterID == 'P4'| EncounterID == 'P5'| EncounterID == 'P6'| EncounterID == 'P7'| EncounterID == 'P8')

# Compute how many whistles were classified to a population for each acoustic encounter

whistles.agg <- aggregate(x = whistles, 
          by = list(unique.values = whistles$EncounterID,
                    total = whistles$PredictedPop), 
          FUN = length)
whistles.agg <- whistles.agg[, 1:3]

write.csv(whistles.agg,'data/results/PcResults_TotalWhistleClass_PMN_Agreggated_20190319.csv')

