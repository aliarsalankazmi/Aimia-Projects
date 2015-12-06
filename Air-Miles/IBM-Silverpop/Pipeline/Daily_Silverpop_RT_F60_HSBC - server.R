#Load libraries and directories
library(RODBC)
library(dplyr)
library(RCurl)
library(lubridate)




outputDir <- "C:\\Users\\Kazmi\\Desktop\\Aimia\\Silverpop\\Test Envir"
outputFileName <- "RT_HSBC_BESPOKE_F60_JOURNEY.TXT"
outputFile <- paste(outputDir, outputFileName, sep = "/")


#Connecting to Insights database in RMMEL 10
odbcChannel <- odbcConnect("Rmmel10_Insights")



#running our stored proc
system.time(amDB <- sqlQuery(channel = odbcChannel, query = "EXEC sp_silverpop_member_hsbc_bespoke_f60_journey"))




#re-formatting our results and exporting them
amDBFormat <- paste0(amDB$record, collapse = "\n")



#Export the file
writeLines(text = amDBFormat, con = outputFile)




#Close database connection
odbcCloseAll()



#Upload to FTP
handle <- getCurlHandle()
ftpUploadFile <- paste("ftp://transfer3.silverpop.com/upload", 
			outputFileName, sep = "/")
ftpUpload(what = outputFile, to = ftpUploadFile, userpwd = ftpCredentials, curl = handle)




#rm(list = ls())


