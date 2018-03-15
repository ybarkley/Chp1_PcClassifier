#March 9, 2018  

###This SORT OF works ###  

#This script concatenates multiple files with different columns into a single dataframe.
#Originally used for combining different ROCCA files for the same encounter

setwd('C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles\\ROCCA Data')
library('gtools')
### THIS WOULD WORK IF ALL FILES HAD SAME COLUMNS

my_files = list.files(pattern="*.csv$")# Gather the csv files in the directory
my_data <- list()                 # This creates a list of the dataframe names

for (i in seq_along(temp)){
  my_data[[i]] <- read.csv(file = temp[i])
}

#this one works to add csv files into a list. then one function can be applied to the dataframe list as a whole
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

#(OR use lapply)
# my_data <- lapply(temp, read.csv)



# https://stackoverflow.com/questions/19655431/reading-multiple-csv-files-in-r

fileList <- list.files(pattern=".csv")
my_data <- sapply(fileList, read.csv)



#Combine all ROCCA files from my_data list into one file
big_ROCCA = do.call(what = rbind.data.frame, args = my_data)


write.csv(big_ROCCA, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles\\BIG ROCCA FILE3.csv', row.names=F)




##Load separate dataframes
# If csv files need to be loaded as separate dataframes (separate objects) in the glob env
# Example: Combining different ROCCA files from YB, PR, and EJ for MHI28
filenames <- list.files(path="C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles\\Raven Data\\MHI28\\concatenate", 
                        pattern=".*csv")

#Pulls out file name of csv from first character to X character
names <-substr(filenames, 1, 29)  

#Load csvs
for(i in names){
  filepath <- file.path("C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles\\Raven Data\\MHI28\\concatenate" ,paste(i,".csv",sep=""))
  assign(i, read.csv(filepath, header=F))  ###MAKE HEADER 'F' IN CASE THERE ARE MORE COLUMNS THAN COLUMN NAMES!!!
}




library('gtools')


do.call(smartbind, my_data)

bindIT<-smartbind(temp)

write.csv(bindIT, 'C:\\Users\\Yvonne\\Documents\\PHD\\CHP1-FKW\\data\\2017- 100 New Whistles\\ROCCA Data\\PcMHI28_Enc06_20101211_RoccaContourStats_ALL.csv', row.names=F)




