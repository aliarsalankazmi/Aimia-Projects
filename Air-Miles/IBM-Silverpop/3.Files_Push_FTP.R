#Load libraries
library(dplyr)
library(RCurl)


#Specify file paths
filesPath <- "Z:\\SILVERPOP"
allFiles <- list.files(path = filesPath, pattern = ".TXT$", full.names = TRUE)
fileNames <- gsub(".+/", "", allFiles)


#Specifying FTP URL



#reqFileNames <- c("RT_AM_MEM_REDM_HIST|RT_AM_MEM_TXN_HIST|RT_HSBC_BESPOKE_F60_JOURNEY|RT_SDG_SEGMENTATION")
#fileNames[grepl(pattern = reqFileNames, x = fileNames, ignore.case = TRUE)]


#Retrieving files' info
reqFilesDf <- file.info(allFiles)
reqFilesDf$size <- reqFilesDf$size/1000
reqFilesDf$modifToday <- as.Date(reqFilesDf$mtime) == Sys.Date() & reqFilesDf$size > 1
reqFilesDf$fileNames <- fileNames

filesGenToday <- reqFilesDf %>% filter(as.Date(mtime) == Sys.Date()) %>% select(fileNames) %>% unlist()
filesGenGrep <- paste(filesGenToday, collapse = "|")
toUpload <- allFiles[grepl(pattern = filesGenGrep, x = allFiles, ignore.case = TRUE)]



#Uploading to FTP
handle <- getCurlHandle()
ftpUpload(what = toUpload[2], to = "ftp://transfer3.silverpop.com/upload", userpwd = "aliarsalan.kazmi@aimia.com:Iamneo123_", curl = handle)




