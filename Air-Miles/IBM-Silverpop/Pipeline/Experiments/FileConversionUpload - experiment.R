#Loading Libraries
library(RDCOMClient)
library(RODBC)
library(dplyr)
library(lubridate)


#Step 1. Connect to AIMIA's SSTP and load credentials for FTP
source(file = 'C:\\Users\\Kazmi\\Desktop\\Aimia\\SilverPop\\ftpCredentials.R')



#Step 2. Checking whether exported files have been converted successfully
#Setting up directory path in which Stored Procs generate result
filesPath <- c("Z:\\SILVERPOP\\Silverpop_Files_Created\\Databases", "Z:\\SILVERPOP\\Silverpop_Files_Created\\Relational_Tables")
reqFiles <- unlist(sapply(filesPath, function(x)list.files(path = x, pattern = ".TXT$", full.names = TRUE)))

if(!day(Sys.Date()) == 3){
reqFiles <- reqFiles[!grepl(pattern = "STATEMENT|SEGMENTATION", x = reqFiles)]}
fileNames <- gsub(".+/", "", reqFiles)



#Retrieving files' info
reqFilesDf <- file.info(reqFiles)
reqFilesDf$size <- reqFilesDf$size/1000
reqFilesDf$modifToday <- as.Date(reqFilesDf$mtime) == Sys.Date() & reqFilesDf$size > 1
reqFilesDf$fileNames <- fileNames


filesError <- reqFilesDf %>% filter(size <= 1) %>% select(fileNames) %>% unlist()



codePath <- "C:\\Users\\Kazmi\\Desktop\\Aimia\\Silverpop\\Pipeline\\"


scriptsToRun <- vector(mode = "character", length = 0)


if("DB_AM_MEM_DTL.TXT" %in% filesError){
scriptsToRun <- c(scriptsToRun, paste0(codePath, "Daily_Silverpop_DB_Refresh - server.R"))
}

if("DB_AM_MEM_DTL_TO_BE_CLOSED.TXT" %in% filesError){
scriptsToRun <- c(scriptsToRun, paste0(codePath, "Daily_Silverpop_DB_ClosedAcc_Refresh - server.R"))
}

if("DB_AM_MEM_DTL_STATEMENT.TXT" %in% filesError){
scriptsToRun <- c(scriptsToRun, paste0(codePath, "AirMilesEStatement_Summary_Monthly - server.R"))
}

if(day(Sys.Date()) == 3){
scriptsToRun <- c(scriptsToRun, paste0(codePath, "Daily_Silverpop_DB_ClosedAccM_Refresh - server.R"))
}

if("RT_HSBC_BESPOKE_F60_JOURNEY.TXT" %in% filesError){
scriptsToRun <- c(scriptsToRun, paste0(codePath, "Daily_Silverpop_RT_F60_HSBC - server.R"))
}

if("RT_SDG_SEGMENTATION.TXT" %in% filesError){
scriptsToRun <- c(scriptsToRun, paste0(codePath, "SDG_Monthly_Segmentation - server.R"))
}


if(length(scriptsToRun != 0){
sapply(scriptsToRun, function(x) source(file = x))
}



rm(list = ls())
