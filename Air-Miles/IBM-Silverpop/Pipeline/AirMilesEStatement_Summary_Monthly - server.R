#Load libraries and directories
library(RODBC)
library(dplyr)
library(RCurl)
library(lubridate)



outputDir <- "C:\\Users\\Kazmi\\Desktop\\Aimia\\Silverpop\\Test Envir"
outputFileNames <- c("DB_AM_MEM_DTL_STATEMENT_EGYPT.TXT", "DB_AM_MEM_DTL_STATEMENT_OMAN.TXT", "DB_AM_MEM_DTL_STATEMENT_LEBANON.TXT",
			"DB_AM_MEM_DTL_STATEMENT_BAHRAIN.TXT", "DB_AM_MEM_DTL_STATEMENT_QATAR.TXT", "DB_AM_MEM_DTL_STATEMENT_UAE.TXT")
outputFiles <- paste(outputDir, outputFileNames, sep = "/")


#Connecting to Insights database in RMMEL 10
odbcChannel <- odbcConnect("Rmmel10_Insights")


#Forming our stored proc statements
storedProc <- "EXEC sp_silverpop_member_last_statement"
spArg <- "@country ="
countries <- c("Egypt", "Oman", "Lebanon", "Bahrain", "Qatar", "UAE")
fullStoredProc <- paste(storedProc, spArg, countries, sep = " ")


#running our stored proc
system.time(amMonthlyStatement <- sapply(fullStoredProc, function(x) sqlQuery(channel = odbcChannel, query = x, as.is = TRUE)))


#Checking for a complete run...
length(countries) == length(amMonthlyStatement)


#Close database connection
odbcCloseAll()


#re-formatting our results and exporting them
amMSFormat <- sapply(amMonthlyStatement, function(x) paste0(x, collapse = "\n"), simplify = FALSE)
mapply(FUN = writeLines, text = amMSFormat, con = outputFiles) 



#Uploading it to FTP Server for Silverpop
#ftpUploadFiles <- paste("ftp://transfer3.silverpop.com/upload", 
#			outputFileNames, sep = "/")
#handle <- getCurlHandle()
#mapply(FUN = function(...){
#	     ftpUpload(..., userpwd = ftpCredentials, curl = handle)},
#	     what = outputFiles, to = ftpUploadFiles)


#Merging all data into a single file
columnNames <- amMonthlyStatement[[1]][1]
amMonthlySttmntCleaned <- lapply(amMonthlyStatement, function(y) gsub(pattern = columnNames, replacement = "", x = y))
amMSunlisted <- unlist(amMonthlySttmntCleaned)
rm(amMonthlySttmntCleaned)
colLengths <- summary(nchar(amMSunlisted))
amMSRequired <- amMSunlisted[!nchar(amMSunlisted) == colLengths[1]]
amMSRequired <- c(columnNames, amMSRequired)
allData <- paste0(amMSRequired, collapse = "\n")

rm(amMSRequired, amMSunlisted)

#Export the file
writeLines(text = allData, con = paste(outputDir, "DB_AM_MEM_DTL_STATEMENT.TXT", sep = "/"))


#Upload to FTP
handle <- getCurlHandle()
ftpUpload(what = paste(outputDir, "DB_AM_MEM_DTL_STATEMENT.TXT", sep = "/"), 
		to = "ftp://transfer3.silverpop.com/upload/DB_AM_MEM_DTL_STATEMENT.TXT", 
		userpwd = ftpCredentials, curl = handle)



#Summarise the number of records
allData <- read.table(text = allData, header = TRUE, sep = "|", stringsAsFactors = FALSE)
allData$generatedOn <- dmy(gsub(".+ to ", "", allData$statement_period))
summDataCountry <- allData %>% group_by(country, generatedOn) %>% summarise(totalRecords = n(), uniqueCustomers = n_distinct(card_no))
summDataAct <- allData %>% group_by(act, generatedOn) %>% summarise(totalRecords = n(), uniqueCustomers = n_distinct(card_no))


#Export summary files
write.table(x = summDataCountry, file = paste(outputDir, "Summary_by_Country.TXT", sep = "/"), row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(x = summDataAct, file = paste(outputDir, "Summary_by_Act.TXT", sep = "/"), row.names = FALSE, col.names = FALSE, append = TRUE)


#rm(list = ls())





