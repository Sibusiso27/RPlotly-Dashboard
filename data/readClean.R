
##############################Import + Transform + Clean###########################

library(reshape2)

#Read data using the db connection R script
#source(file = "data/dbConnection.R")
#dat <- dbaseConnection("ATHENA" ,'01/JAN/2018', '31/MAR/2023', "BASIC")

#Alternative: read data from the csv file
dat <- read.csv("data/datBasic.csv")

#Convert string duration to hours
dat$PROCESS_DURATION <- round(sapply(dat$PROCESS_DURATION, function(x) 
  sum(as.numeric(strsplit(x, ":")[[1]]) * c(1, 1/60, 1/3600))), 2) 
dat$TOTAL_DURATION <- round(sapply(dat$TOTAL_DURATION, function(x) sum(as.numeric(strsplit(x, ":")[[1]]) * c(1, 1/60, 1/3600))), 2)

#Exclude if Start time between 6am (21600 from midnight) and 8:45pm (74700 sec from 00:00) 
startOutliers <- unique(dat[which(dat$PROCESS_NAME == 'START_01' & 
                                    dat$START_TIME_MIDNIGHT >= 21600 & dat$START_TIME_MIDNIGHT < 74700), "COB_DATE"])
dat <- dat[!(dat$COB_DATE %in% startOutliers),]

#Exclude days where START_01 is not the starting script
minStartTime <- dcast(data = dat, formula = COB_DATE ~ ., fun.aggregate = min, value.var = "TOTAL_DURATION")
names(minStartTime) <- c("COB_DATE", "TOTAL_DURATION")

startProcess <- merge(minStartTime, dat[, c("COB_DATE", "TOTAL_DURATION", "PROCESS_NAME")], 
                     by = c("COB_DATE", "TOTAL_DURATION"), all.x = T, all.y = F, no.dups = F)

dat <- dat[!(dat$COB_DATE %in% startProcess[startProcess$PROCESS_NAME != 'START_01', "COB_DATE"]),]

#Exclude days where START_01 occurs multiple times during the day
allStart <- dat[dat$PROCESS_NAME == 'START_01', "COB_DATE"]
dat <- dat[!(dat$COB_DATE %in% unique(allStart[duplicated(allStart)])),]

#Exclude data for scripts that are run after the overnight process
dat <- dat[!(dat$PROCESS_NAME %in% c('INTRADAY_01', 'REGENERATION_01',
                                     'REGENERATION_02', 'REGENERATION_05',
                                     'REGENERATION_03', 'REPORT_PREP_04',
                                     'REGENERATION_04', 'CONSOLIDATION_01',
                                     'DETAILED_CONSO_01', 'RESULTS_01',
                                     'REPAIR_01')),]

#Save data
write.table(x = dat, file = "data/datBasicClean.csv", quote = F, sep = ",", row.names = F, dec = ".")
