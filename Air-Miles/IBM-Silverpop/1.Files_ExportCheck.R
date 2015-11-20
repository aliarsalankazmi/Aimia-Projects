#Loading Libraries
library(RDCOMClient)
library(dplyr)



#Step 1. Checking whether Stored Procedures have successfully exported results

#Setting up directory path in which Stored Procs generate result
filesPath <- "Z:\\SILVERPOP\\Silverpop_Files_Created\\Relational_Tables"
reqFiles <- list.files(path = filesPath, pattern = ".TXT$", full.names = TRUE)
fileNames <- gsub(".+/", "", reqFiles)

#Retrieving files' info
reqFilesDf <- file.info(reqFiles)
reqFilesDf$size <- reqFilesDf$size/1000
reqFilesDf$modifToday <- as.Date(reqFilesDf$mtime) == Sys.Date() & reqFilesDf$size > 1
reqFilesDf$fileNames <- fileNames

filesGenToday <- reqFilesDf %>% filter(as.Date(mtime) == Sys.Date()) %>% select(fileNames) %>% unlist()
filesGenToday <- paste(seq(from = 1, by = 1, to = length(filesGenToday)), filesGenToday, sep = ". ", collapse = "\n")
filesGenTodayMessage <- paste("\nFile(s) that have been modified today:",  filesGenToday, sep = "\n\n", collapse = "\n")

filesGenTodayMessage <- ifelse(length(filesGenToday) == 0, "No files have been generated today.", filesGenTodayMessage)


filesZeroSize <- reqFilesDf %>% filter(as.Date(mtime) == Sys.Date(), size <= 1) %>% select(fileNames) %>% unlist()
filesZeroSize <- paste(seq(from = 1, by = 1, to = length(filesZeroSize)), filesZeroSize, sep = ". ", collapse = "\n")
filesZeroMessage <- paste("File(s) that have been modified today but have a size of less than or equal to 1KB :",  filesZeroSize, sep = "\n\n", collapse = "\n")

filesZeroMessage <- ifelse(length(filesZeroSize) == 0, "No files have been generated today.", filesZeroMessage)

autoMessage <- "This is an automated message."

completeMessage <- paste(autoMessage, filesGenTodayMessage, filesZeroMessage, sep = "\n\n")



#Sending an email for status of our files
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = "aliarsalan.kazmi@aimia.com; mark.edralin@aimia.com"
outMail[["subject"]] = "Stored Proc Outputs for SilverPop (Notification)"
outMail[["body"]] = completeMessage 
## send it                     
outMail$Send()

















