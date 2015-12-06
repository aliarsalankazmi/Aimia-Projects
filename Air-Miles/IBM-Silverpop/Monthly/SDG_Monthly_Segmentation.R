#Load libraries and directories
library(RODBC)
library(dplyr)
library(RCurl)
library(lubridate)


#Connect to VPN and load my FTP credentials
source(file = 'C:\\Users\\kazami\\Desktop\\Aimia\\SilverPop\\Connect_SSTP.R')
source(file = 'C:\\Users\\kazami\\Desktop\\Aimia\\SilverPop\\ftpCredentials.R')



outputDir <- "Z:\\SILVERPOP\\Monthly Statement"
outputFileName <- "RT_SDG_SEGMENTATION.TXT"
#Mark stores this file in Silverpop Files Created -> Relational Tables; may want to overwrite that file...
outputFile <- paste(outputDir, outputFileName, sep = "/")


#Connecting to Insights database in RMMEL 10
odbcChannel <- odbcConnect("Rmmel10_Insights")



#running our stored proc
system.time(amDB <- sqlQuery(channel = odbcChannel, query = "EXEC sp_sdg_segmentation"))




#re-formatting our results and exporting them
names(amDB) <- "record"
amDBFormat <- paste0(amDB$record, collapse = "\n")



#Export the file
writeLines(text = amDBFormat, con = outputFile)



#Uploading it to FTP Server for Silverpop
ftpUploadFile <- paste("ftp://transfer3.silverpop.com/upload", 
			outputFileName, sep = "/")

#Upload to FTP
handle <- getCurlHandle()
ftpUpload(what = outputFile, to = ftpUploadFile, userpwd = ftpCredentials, curl = handle)



#Close database connection
odbcCloseAll()



#Summarise the number of records
#allData <- read.table(text = amDBFormat, header = TRUE, sep = "|", stringsAsFactors = FALSE)
#allData$generatedOn <- dmy(gsub(".+ to ", "", allData$statement_period))
#summDataCountry <- allData %>% group_by(country, generatedOn) %>% summarise(totalRecords = n(), uniqueCustomers = n_distinct(card_no))
#summDataAct <- allData %>% group_by(act, generatedOn) %>% summarise(totalRecords = n(), uniqueCustomers = n_distinct(card_no))


#Export summary files
#write.table(x = summDataCountry, file = paste(outputDir, "DailyDB_Summary_by_Country.TXT", sep = "/"), row.names = FALSE, append = TRUE)
#write.table(x = summDataAct, file = paste(outputDir, "DailyDB_Summary_by_Act.TXT", sep = "/"), row.names = FALSE, append = TRUE)


rm(list = ls())



