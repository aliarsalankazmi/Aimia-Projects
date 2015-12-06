#Load libraries and directories
library(RODBC)
library(dplyr)
library(RCurl)


#Connect to VPN
source(file = 'C:\\Users\\kazami\\Desktop\\Aimia\\SilverPop\\Connect_SSTP.R')



#inputDir <- "Z:\\SILVERPOP\\Silverpop_Files_Created\\Seedlist"
#Identify files
#myFile <- list.files(path = inputDir, full.names = TRUE)
#Read file in
#fileInput <- readLines(con <- file(myFile, encoding = "UCS-2LE"))
#fileCleaned <- gsub("-+", "", fileInput) 
#zeroSize <- unname(sapply(fileCleaned, function(x) nchar(x) == 0))
#fileCleaned <- fileInput[!zeroSize]
#unnecessaryRows <- grepl("Job|record|affected", fileCleaned, ignore.case = TRUE)
#fileCleaned <- fileCleaned[!unnecessaryRows]



outputDir <- "Z:\\SILVERPOP\\Seedlist"
outputFileName <- "SILVERPOP_SEEDLIST_DAILY.TXT"
outputFile <- paste(outputDir, outputFileName, sep = "/")


#Connecting to Insights database in RMMEL 10 to trigger Stored Proc
odbcChannel <- odbcConnect("Rmmel10_Insights")
seedList <- sqlQuery(channel = odbcChannel, query = "EXEC sp_silverpop_member_details_seedlist")



#Transforming and exporting the file
newSeed <- paste0(seedList$record, collapse = "\n")
writeLines(text = newSeed, con = outputFile)



#Uploading it to FTP Server for Silverpop
handle <- getCurlHandle()
ftpUpload(what = outputFile, to = "ftp://transfer3.silverpop.com/upload/SILVERPOP_SEEDLIST_DAILY.TXT", userpwd = "aliarsalan.kazmi@aimia.com:Iamneo456_", curl = handle)



