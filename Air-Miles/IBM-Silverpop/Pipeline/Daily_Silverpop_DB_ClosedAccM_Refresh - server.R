#Load libraries and directories
library(RODBC)
library(dplyr)
library(RCurl)
library(lubridate)



outputDir <- "C:\\Users\\Kazmi\\Desktop\\Aimia\\Silverpop\\Test Envir"
outputFileName <- "AM_MEM_DTL_CLOSED_ACCOUNT.TXT"
outputFile <- paste(outputDir, outputFileName, sep = "/")


#Connecting to Insights database in RMMEL 10
odbcChannel <- odbcConnect("Rmmel10_Insights")



#running our stored proc
system.time(amDB <- sqlQuery(channel = odbcChannel, query = "EXEC sp_silverpop_members_closed_accounts_15mos"))




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



