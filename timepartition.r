# Partitioning the acoustic data into time increments 
# This script calculates the time increments used for the random selection
# of whistles in each encounter.


s = (2*60*60)+37*60        #Length of total wav files in seconds
w = 150         #Number of whistles (time increment)

tp = s/w        #Time partition in seconds

list_tp = seq(0, s, tp)

write.csv(list_tp, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles\\Raven Data\\NWHI17\\NWHI17_timepartitions.csv')

#If time partition does not have whistles in it, then randomly select another time partition and choose the second whistle in it.
sort(sample(list_tp, 50), decreasing=F)

sort(sample(list_tp[1:50],22), decreasing=F)

sort(sample(list_tp[c(17:44, 48:81)],15), decreasing=F)

#For NWHI15, subtract the tp without whistles in the first 50 whistles (it's a list with 1 dimension)
list_tp = list_tp[-c(1:4, 6:17, 19, 22:24, 27, 28, 31:33, 33, 36, 38, 40, 42, 51:101)]
sort(sample(list_tp, 1, replace=T), decreasing=F)






